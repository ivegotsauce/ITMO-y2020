import kotlinx.atomicfu.AtomicRef
import kotlinx.atomicfu.atomic
import kotlin.coroutines.Continuation
import kotlin.coroutines.resume
import kotlin.coroutines.suspendCoroutine

/**
 * An element is transferred from sender to receiver only when [send] and [receive]
 * invocations meet in time (rendezvous), so [send] suspends until another coroutine
 * invokes [receive] and [receive] suspends until another coroutine invokes [send].
 */


interface Type
object Data : Type
object Request : Type
object Dummy : Type

class Node<E>(d: E?, private val type: Type) {

    val next: AtomicRef<Node<E>?> = atomic(null)
    var cont: Continuation<Boolean>? = null
    val data = atomic(d)
    fun isRequest(): Boolean {
        return type is Request
    }

}

class SynchronousQueue<E> {

    private val head: AtomicRef<Node<E>>
    private val tail: AtomicRef<Node<E>>

    init {
        val dummy = Node<E>(null, Dummy)
        head = atomic(dummy)
        tail = atomic(dummy)
    }

    /**
     * Sends the specified [element] to this channel, suspending if there is no waiting
     * [receive] invocation on this channel.
     */
    suspend fun send(element: E): Unit {
        val offer = Node(element, Data)

        while (true) {
            val t = tail.value
            var h = head.value
            if (h == t || !t.isRequest()) {
                val n = t.next.value;
                if (t === tail.value) {
                    if (null != n) {
                        tail.compareAndSet(t, n)
                    } else if (scNodes(offer, t)) {
                        h = head.value
                        if (offer === h.next.value) {
                            head.compareAndSet(h, offer)
                        }
                        return
                    }
                }
            } else {
                val n = h.next.value ?: continue
                if (t !== tail.value || h !== head.value) {
                    continue
                }
                head.compareAndSet(h, n)
                val success = n.isRequest() && n.data.compareAndSet(null, element)
                if (success) {
                    n.cont!!.resume(true)
                    return
                }
            }
        }
    }

    /**
     * Retrieves and removes an element from this channel if there is a waiting [send] invocation on it,
     * suspends the caller if this channel is empty.
     */
    suspend fun receive(): E {
        val offer: Node<E> = Node(null, Request)

        while (true) {
            val t = tail.value
            var h = head.value
            if (h == t || t.isRequest()) {
                val n = t.next.value;
                if (t === tail.value) {
                    if (null != n) {
                        tail.compareAndSet(t, n)
                    } else if (scNodes(offer, t)) {
                        h = head.value
                        if (offer === h.next.value) {
                            head.compareAndSet(h, offer)
                        }
                        return offer.data.value!!
                    }
                }
            } else {
                val n = h.next.value ?: continue
                val nv = n.data.value ?: continue
                if (t !== tail.value || h !== head.value) {
                    continue
                }
                head.compareAndSet(h, n)
                val success = !n.isRequest() && n.data.compareAndSet(nv, null)
                if (success) {
                    n.cont!!.resume(true)
                    return nv
                }
            }
        }
    }

    private suspend fun scNodes(offer: Node<E>, t: Node<E>) =
        suspendCoroutine { cont ->
            offer.cont = cont
            when {
                t.next.compareAndSet(null, offer) -> tail.compareAndSet(t, offer)
                else -> cont.resume(false)
            }
        }

}