package expression.generic;

import expression.operator.Operator;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface TripleExpression<T> extends ToMiniString<T> {
    T evaluate(T x, T y, T z, Operator<T> op);
}