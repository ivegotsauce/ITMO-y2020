package expression.generic;

import expression.operator.Operator;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface Expression<T> extends ToMiniString<T> {
    T evaluate(T x, Operator<T> op);
}
