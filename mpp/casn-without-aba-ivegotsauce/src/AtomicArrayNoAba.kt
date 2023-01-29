import kotlinx.atomicfu.*
import java.util.concurrent.atomic.AtomicReference

class AtomicArrayNoAba<E>(size: Int, initialValue: E) {
    private val a = atomicArrayOfNulls<Any>(size)

    init {
        for (i in 0 until size) a[i].value = initialValue
    }

    @Suppress("UNCHECKED_CAST")
    fun get(index: Int): E {
        val p = readInternal(index, null)
        return p.second as E
    }

    fun cas(index: Int, expected: E, update: E): Boolean {
        while (true) {
            val value = a[index].value
            if (value !is WordDescriptor) return a[index].compareAndSet(expected, update)
            else {
                val parent = value.parent
                if (parent.status.get() == Active) {
                    cas2(parent)
                } else {
                    free(parent)
                }
            }
        }
    }

    @Suppress("UNCHECKED_CAST")
    fun cas2(
        index1: Int, expected1: E, update1: E,
        index2: Int, expected2: E, update2: E
    ): Boolean {
        if (index1 == index2 && expected1 is Int) {
            return cas(index1, expected1, (expected1 + 2) as E)
        }
        val desc = CAS2Descriptor(AtomicReference(Active))
        desc.words = when (index1 < index2) {
            true -> arrayOf(
                WordDescriptor(index1, expected1 as Any, update1 as Any, desc),
                WordDescriptor(index2, expected2 as Any, update2 as Any, desc)
            )

            else -> arrayOf(
                WordDescriptor(index2, expected2 as Any, update2 as Any, desc),
                WordDescriptor(index1, expected1 as Any, update1 as Any, desc)
            )
        }

        return cas2(desc)
    }

    private class WordDescriptor(val address: Int, val old: Any, val new: Any, val parent: CAS2Descriptor)

    private fun cas2(desc: CAS2Descriptor): Boolean {
        var success = true
        loop@ for (wordDesc in desc.words) {
            while (true) {
                val p = readInternal(wordDesc.address, desc)
                if (p.first === wordDesc) continue@loop
                if (p.second != wordDesc.old) {
                    success = false
                    break@loop
                }
                if (desc.status.get() != Active) break@loop
                if (a[wordDesc.address].compareAndSet(p.first, wordDesc)) break
            }
        }
        if (desc.status.compareAndSet(
                Active, when {
                    success -> Successful
                    else -> Failed
                }
            )
        ) {
            free(desc)
        }
        return desc.status.get() == Successful
    }

    private fun free(desc: CAS2Descriptor) {
        for (wordDesc in desc.words) {
            val set = when {
                desc.status.get() is Successful -> wordDesc.new
                else -> wordDesc.old
            }
            a[wordDesc.address].compareAndSet(wordDesc, set)
        }
    }

    private fun readInternal(address: Int, self: CAS2Descriptor?): Pair<Any?, Any?> {
        val value = a[address].value
        while (true) {
            if (value !is WordDescriptor) return value to value
            else {
                val parent = value.parent
                if (parent !== self && parent.status.get() == Active) {
                    cas2(parent)
                } else {
                    return when (parent.status.get()) {
                        Successful -> value to value.new
                        else -> value to value.old
                    }
                }
            }
        }
    }

    private class CAS2Descriptor(val status: AtomicReference<Status>) {
        lateinit var words: Array<WordDescriptor>
    }

    private interface Status
    private object Active : Status
    private object Successful : Status
    private object Failed : Status
}


