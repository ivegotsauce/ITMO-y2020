package mpp.linkedlistset

import java.util.concurrent.atomic.AtomicMarkableReference

class LinkedListSet<E : Comparable<E>> {
    private val first = Node<E>(element = null, next = null, First)
    private val last = Node<E>(element = null, next = null, Last)

    init {
        first.next = AtomicMarkableReference(last, false)
    }

    private val head = AtomicMarkableReference(first, false)

    /**
     * Adds the specified element to this set
     * if it is not already present.
     *
     * Returns `true` if this set did not
     * already contain the specified element.
     */
    fun add(element: E): Boolean {
        while (true) {
            val window = find(head.reference, element)
            val pred = window.first
            val curr = window.second
            if (curr.element == element) return false
            else if (pred.next.compareAndSet(curr, Node(element, curr, Elem), false, false))
                return true
        }
    }

    /**
     * Removes the specified element from this set
     * if it is present.
     *
     * Returns `true` if this set contained
     * the specified element.
     */
    fun remove(element: E): Boolean {
        while (true) {
            val window = find(head.reference, element)
            val pred = window.first
            val curr = window.second
            if (curr.element != element) {
                return false
            } else {
                val succ = curr.next.reference
                if (!curr.next.attemptMark(succ, true)) continue
                if (pred.next.compareAndSet(curr, succ, false, false))
                    return true
            }
        }
    }

    /**
     * Returns `true` if this set contains
     * the specified element.
     */
    fun contains(element: E): Boolean {
        val marked = BooleanArray(1) { false }
        var curr = head
        while (lessThan(curr.reference.element, element, curr.reference.type, Elem)) {
            curr = curr.reference.next
            curr.reference.next.get(marked)
        }
        return curr.reference.element == element && !marked[0]
    }
}

private fun <E : Comparable<E>> find(head: Node<E>, key: E): Pair<Node<E>, Node<E>> {
    val marked = BooleanArray(1) { false }
    loop@ while (true) {
        var pred = head
        var curr = pred.next.reference
        while (true) {
            var succ = curr!!.next.get(marked)
            while (marked[0]) {
                if (!pred.next.compareAndSet(curr, succ, false, false)) continue@loop
                curr = succ
                succ = curr!!.next.get(marked)
            }
            if (!lessThan(curr!!.element, key, curr.type, Elem)) return pred to curr
            pred = curr
            curr = succ
        }
    }
}

private interface Type
private object First : Type
private object Last : Type
private object Elem : Type

private fun <E : Comparable<E>> lessThan(a: E?, b: E?, ta: Type, tb: Type): Boolean {
    return when {
        ta == First && tb == First -> false
        ta == Last && tb == Last -> false
        ta == First -> true
        ta == Last -> false
        tb == Last -> true
        tb == First -> false
        a != null && b != null -> a < b
        else -> false
    }
}

private class Node<E : Comparable<E>>(val element: E?, next: Node<E>?, val type: Type) {
    var next = AtomicMarkableReference(next, false)
}