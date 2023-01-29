package mpp.dynamicarray

import kotlinx.atomicfu.*


interface DynamicArray<E> {
    /**
     * Returns the element located in the cell [index],
     * or throws [IllegalArgumentException] if [index]
     * exceeds the [size] of this array.
     */
    fun get(index: Int): E

    /**
     * Puts the specified [element] into the cell [index],
     * or throws [IllegalArgumentException] if [index]
     * exceeds the [size] of this array.
     */
    fun put(index: Int, element: E)

    /**
     * Adds the specified [element] to this array
     * increasing its [size].
     */
    fun pushBack(element: E)

    /**
     * Returns the current size of this array,
     * it increases with [pushBack] invocations.
     */
    val size: Int
}

class DynamicArrayImpl<E> : DynamicArray<E> {
    private object Broken

    private val core = atomic(Core<E>(INITIAL_CAPACITY))
    override val size: Int
        get() {
            val curCore = core.value
            var s = 0
            while (s < curCore.capacity && curCore.array[s].value != null) {
                s++
            }
            return s
        }

    override fun get(index: Int): E {
        if (index < 0 || index >= size) {
            throw IllegalArgumentException("index out of bounds")
        }
        while (true) {
            val curCore = core.value
            when (val elem = curCore.get(index)) {
                is Broken -> {
                    promote(curCore)
                    updateCore(curCore)
                }

                else -> return elem
            }
        }

    }

    override fun put(index: Int, element: E) {
        if (index < 0 || index >= size) {
            throw IllegalArgumentException("index out of bounds")
        }
        while (true) {
            val curCore = core.value
            when (val elem = curCore.get(index)) {
                is Broken -> {
                    promote(curCore)
                    updateCore(curCore)
                }

                else -> {
                    if (curCore.array[index].compareAndSet(elem, element)) {
                        break
                    }
                }
            }
        }
    }

    override fun pushBack(element: E) {
        while (true) {
            val curCore = core.value
            var index = -1
            for (i in 0 until curCore.capacity) {
                if (curCore.array[i].value == null) {
                    index = i
                    break
                }
            }
            if (index != -1) {
                if (curCore.array[index].compareAndSet(null, element)) {
                    return
                }
            } else {
                resize(curCore)
            }
        }
    }

    private fun resize(oldCore: Core<E>) {
        val newCore = Core<E>(2 * oldCore.capacity)
        oldCore.next.compareAndSet(null, newCore)
        promote(oldCore)
        updateCore(oldCore)
    }

    private fun promote(oldCore: Core<E>) {
        for (i in 0 until oldCore.capacity) {
            while (true) {
                val n = oldCore.next.value!!.array[i]
                val nv = n.value
                when (val elem = oldCore.get(i)) {
                    is Broken -> break
                    else -> {
                        if (n.compareAndSet(nv, elem) && oldCore.array[i].compareAndSet(elem, Broken)) {
                            break
                        }
                    }
                }
            }
        }
    }

    private fun updateCore(oldCore: Core<E>) {
        core.compareAndSet(oldCore, oldCore.next.value!!)
    }

}

private class Core<E>(
    cap: Int
) {
    val array = atomicArrayOfNulls<Any>(cap)
    private val _capacity = atomic(cap)
    val capacity: Int get() = _capacity.value
    val next = atomic<Core<E>?>(null)


    @Suppress("UNCHECKED_CAST")
    fun get(index: Int): E = array[index].value as E

}

private const val INITIAL_CAPACITY = 1 // DO NOT CHANGE ME