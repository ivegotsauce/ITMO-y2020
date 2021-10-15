package expression.generic;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface ToMiniString<T> {
    default String toMiniString() {
        return toString();
    }
}
