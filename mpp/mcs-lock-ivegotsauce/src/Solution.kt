import java.util.concurrent.atomic.*

class Solution(private val env: Environment): Lock<Solution.QNode> {

    private val tail = AtomicReference<QNode>()

    override fun lock(): QNode {
        val my = QNode()
        my.locked.value = true
        val pred = tail.getAndSet(my)
        if (pred != null) {
            pred.next.value = my
            while (my.locked.value) {
                env.park()
            }
        }
        return my
    }

    override fun unlock(node: QNode) {
        if (node.next.get() == null) {
            if (tail.compareAndSet(node, null)) return
            else
                while (node.next.get() == null) continue
        }
        node.next.value.locked.value = false
        env.unpark(node.next.value.thread)
    }

    class QNode {
        val thread: Thread = Thread.currentThread()
        val locked = AtomicReference(false)
        val next =  AtomicReference<QNode>()
    }

}

