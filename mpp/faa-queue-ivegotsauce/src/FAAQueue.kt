package mpp.faaqueue

import kotlinx.atomicfu.*

sealed interface Elem

object T : Elem
class Good(val t: Any?) : Elem

class FAAQueue<E> {
    private val head: AtomicRef<Segment> // Head pointer, similarly to the Michael-Scott queue (but the first node is _not_ sentinel)
    private val tail: AtomicRef<Segment> // Tail pointer, similarly to the Michael-Scott queue
    private val enqIdx = atomic(0L)
    private val deqIdx = atomic(0L)

    init {
        val firstNode = Segment(0L)
        head = atomic(firstNode)
        tail = atomic(firstNode)
    }

    /**
     * Adds the specified element [x] to the queue.
     */
    fun enqueue(element: E) {
        while (true) {
            val curTail = tail.value
            val i = enqIdx.getAndIncrement()
            val s = findSegment(curTail, i / SEGMENT_SIZE)
            moveTailForward(s)
            if (s.cas((i % SEGMENT_SIZE).toInt(), null, Good(element))) {
                return
            }
        }
    }

    /**
     * Retrieves the first element from the queue and returns it;
     * returns `null` if the queue is empty.
     */
    fun dequeue(): E? {
        while (true) {
            if (deqIdx.value >= enqIdx.value) {
                return null
            }
            val curHead = head.value
            val i = deqIdx.getAndIncrement()
            val s = findSegment(curHead, i / SEGMENT_SIZE)
            moveHeadForward(s)
            val idx = i.toInt() % SEGMENT_SIZE
            if (s.elements[idx].compareAndSet(null, T)) {
                continue
            }
            return when (val elem = s.get(idx)) {
                is Good -> elem.t as E
                is T -> null
                else -> null
            }
        }
    }

    /**
     * Returns `true` if this queue is empty, or `false` otherwise.
     */
    val isEmpty: Boolean
        get() {
            return deqIdx.value >= enqIdx.value
        }

    private fun findSegment(start: Segment, id: Long): Segment {
        var s = start
        while (s.id < id) {
            s.casNext(null, Segment(s.id + 1))
            s = s.next!!
        }
        return s
    }

    private fun moveTailForward(s: Segment) {
        var curTail = tail.value
        while (s.id > curTail.id) {
            tail.compareAndSet(curTail, s)
            curTail = tail.value
        }
    }

    private fun moveHeadForward(s: Segment) {
        var curHead = head.value
        while (s.id > curHead.id) {
            head.compareAndSet(curHead, s)
            curHead = head.value
        }
    }
}

private class Segment(id__: Long) {
    private val next_: AtomicRef<Segment?> = atomic(null)
    val elements = atomicArrayOfNulls<Elem>(SEGMENT_SIZE)
    private val id_: AtomicLong

    init {
        id_ = atomic(id__)
    }

    val next: Segment?
        get() {
            return next_.value
        }

    val id: Long
        get() {
            return id_.value
        }

    fun get(i: Int) = elements[i].value
    fun cas(i: Int, expect: Elem?, update: Elem) = elements[i].compareAndSet(expect, update)
    fun put(i: Int, value: Any?) {
        elements[i].value = Good(value)
    }

    fun casNext(expect: Segment?, update: Segment?) = next_.compareAndSet(expect, update)
}

const val SEGMENT_SIZE = 2 // DO NOT CHANGE, IMPORTANT FOR TESTS


