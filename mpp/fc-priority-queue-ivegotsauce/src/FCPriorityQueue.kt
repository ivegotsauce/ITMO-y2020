import kotlinx.atomicfu.atomic
import kotlinx.atomicfu.atomicArrayOfNulls
import java.util.*

class FCPriorityQueue<E : Comparable<E>> {
    private val q = PriorityQueue<E>()
    private val locked = atomic(false)
    private val workers = 3 * Runtime.getRuntime().availableProcessors()
    private val fcArray = atomicArrayOfNulls<Operation<E>>(workers)
    private val random = Random()

    private enum class Op {
       POLL,
       PEEK,
       ADD,
       COMPLETED
    }

    private data class Operation<T>(val type: Op, val attr: T?)

    private fun tryLock() = locked.compareAndSet(expect=false, true)

    private fun unlock() {
        locked.getAndSet(false)
    }

    private fun evaluate(type: Op, argument: E?): E? {
        var res: E? = null
        var put = false
        var idx = 0
        while (true) {
            if (tryLock()) {
                if (put) {
                    if (fcArray[idx].value?.type == Op.COMPLETED) {
                        res = fcArray[idx].value?.attr
                        fcArray[idx].value = null
                        unlock()
                        return res
                    }
                    fcArray[idx].value = null
                }
                when (type) {
                    Op.ADD -> q.add(argument)
                    Op.POLL -> res = q.poll()
                    Op.PEEK -> res = q.peek()
                    Op.COMPLETED -> continue
                }
                for (i in 0 until workers) {
                    val op = fcArray[i].value
                    if (op != null) {
                        when (op.type) {
                            Op.COMPLETED -> continue
                            Op.ADD -> {
                                q.add(op.attr)
                                fcArray[i].value = Operation(Op.COMPLETED, null)
                            }
                            Op.PEEK -> fcArray[i].value = Operation(Op.COMPLETED, q.peek())
                            Op.POLL -> fcArray[i].value = Operation(Op.COMPLETED, q.poll())
                        }
                    }
                }
                unlock()
                return res
            } else if (!put) {
                idx = random.nextInt(workers)
                put = fcArray[idx].compareAndSet(null, Operation(type, argument))
            } else if (fcArray[idx].value?.type == Op.COMPLETED) {
                res = fcArray[idx].value?.attr
                fcArray[idx].value = null
                return res
            }
        }
    }

    /**
     * Retrieves the element with the highest priority
     * and returns it as the result of this function;
     * returns `null` if the queue is empty.
     */
    fun poll(): E? {
        return evaluate(Op.POLL, null)
    }

    /**
     * Returns the element with the highest priority
     * or `null` if the queue is empty.
     */
    fun peek(): E? {
        return evaluate(Op.PEEK, null)
    }

    /**
     * Adds the specified element to the queue.
     */
    fun add(element: E) {
        evaluate(Op.ADD, element)
    }
}