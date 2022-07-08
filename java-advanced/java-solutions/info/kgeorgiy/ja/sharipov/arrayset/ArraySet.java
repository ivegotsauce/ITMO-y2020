package info.kgeorgiy.ja.sharipov.arrayset;

import java.util.*;

public class ArraySet<E> extends AbstractSet<E> implements SortedSet<E> {
    private final List<E> data;
    private final Comparator<E> comparator;

    public ArraySet(final Collection<? extends E> collection) {
        // :NOTE: null
        this.data = List.copyOf(new TreeSet<>(collection));
        this.comparator = null;
    }

    public ArraySet(final Collection<? extends E> collection, final Comparator<E> comparator) {
        final TreeSet<E> treeSet = new TreeSet<>(comparator);
        treeSet.addAll(collection);
        this.data = List.copyOf(treeSet);
        this.comparator = comparator;
    }

    public ArraySet() {
        this.data = List.of();
        this.comparator = null;
    }

    @Override
    public Iterator<E> iterator() {
        return data.iterator();
    }

    @Override
    public int size() {
        return data.size();
    }

    @Override
    public Comparator<? super E> comparator() {
        return comparator;
    }

    @Override
    @SuppressWarnings("unchecked")
    public boolean contains(final Object o) {
        return Collections.binarySearch(data, (E) o, comparator) >= 0;
    }

    @Override
    public SortedSet<E> subSet(final E fromElement, final E toElement) {
        if ((comparator != null && comparator.compare(fromElement, toElement) > 0) ||
                (comparator == null && Collections.reverseOrder().compare(fromElement, toElement) < 0)) {
            throw new IllegalArgumentException("fromElement > toElement");
        }
        return getSubSet(getIndex(fromElement), getIndex(toElement));
    }

    @Override
    public SortedSet<E> headSet(final E toElement) {
        return getSubSet(0, getIndex(toElement));
    }

    @Override
    public SortedSet<E> tailSet(final E fromElement) {
        return getSubSet(getIndex(fromElement), size());
    }

    @Override
    public E first() {
        checkEmpty();
        return data.get(0);
    }

    @Override
    public E last() {
        checkEmpty();
        return data.get(size() - 1);
    }

    private int getIndex(final E element) {
        final int index = Collections.binarySearch(data, element, comparator);
        return index >= 0 ? index : -index - 1;
    }

    private ArraySet<E> getSubSet(final int fromIndex, final int toIndex) {
        return new ArraySet<>(data.subList(fromIndex, toIndex), comparator);
    }

    private void checkEmpty() {
        if (this.isEmpty()) {
            throw new NoSuchElementException("ArraySet is empty");
        }
    }

}
