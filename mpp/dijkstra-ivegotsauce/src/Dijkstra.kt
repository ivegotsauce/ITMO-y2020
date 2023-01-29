package dijkstra

import java.util.*
import java.util.concurrent.Phaser
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.locks.ReentrantLock
import kotlin.Comparator
import kotlin.concurrent.thread

private val NODE_DISTANCE_COMPARATOR = Comparator<Node> { o1, o2 ->
    when {
        o1 == null && o2 == null -> 0
        o1 == null && o2 != null -> 1
        o1 != null && o2 == null -> -1
        else -> o1.distance.compareTo(o2.distance)
    }
}

class PQ(comparator: Comparator<Node>) {
    private val queue = PriorityQueue(comparator)
    private val lock = ReentrantLock()

    fun tryLock(): Boolean {
        return lock.tryLock()
    }

    fun unlock() {
        lock.unlock()
    }

    fun add(task: Node) {
        queue.add(task)
    }

    fun top(): Node? {
        return queue.peek()
    }

    fun extractTop(): Node? {
        return queue.poll()
    }
}

class MultiQueue(workers: Int, private val comparator: Comparator<Node>) {
    private val cxt = 2 * workers
    private val queues = Array(cxt) { PQ(comparator) }
    private val random = Random()

    private fun getIndex(): Int {
        return random.nextInt(cxt)
    }

    fun insert(task: Node) {
        while (true) {
            val q = queues[getIndex()]
            if (!q.tryLock()) {
                continue
            }
            q.add(task)
            q.unlock()
            return
        }
    }

    fun delete(): Node? {
        while (true) {
            val q1 = queues[getIndex()]
            val q2 = queues[getIndex()]
            if (q1.tryLock()) {
                if (!q2.tryLock()) {
                    val v = q1.extractTop()
                    q1.unlock()
                    return v
                }
            } else continue
            val v = when {
                comparator.compare(q1.top(), q2.top()) < 0 -> q1.extractTop()
                else -> q2.extractTop()
            }
            q1.unlock()
            q2.unlock()
            return v
        }
    }
}

// Returns `Integer.MAX_VALUE` if a path has not been found.
fun shortestPathParallel(start: Node) {
    val workers = Runtime.getRuntime().availableProcessors()
    // The distance to the start node is `0`
    start.distance = 0
    // Create a priority (by distance) queue and add the start node into it
    val q = MultiQueue(workers, NODE_DISTANCE_COMPARATOR)
    q.insert(start)
    // Run worker threads and wait until the total work is done
    val onFinish = Phaser(workers + 1) // `arrive()` should be invoked at the end by each worker
    val activeNodes = AtomicInteger(1)
    repeat(workers) {
        thread {
            while (activeNodes.get() > 0) {
                val u = q.delete() ?: continue
                for (v in u.outgoingEdges) {
                    while (true) {
                        val to = v.to
                        val dist = to.distance
                        val d = u.distance + v.weight
                        if (d >= dist) break
                        if (to.casDistance(dist, d)) {
                            q.insert(to)
                            activeNodes.incrementAndGet()
                            break
                        } else continue
                    }
                }
                activeNodes.decrementAndGet()
            }
            onFinish.arrive()
        }
    }
    onFinish.arriveAndAwaitAdvance()
}