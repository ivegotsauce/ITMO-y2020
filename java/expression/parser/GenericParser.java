package expression.parser;

import expression.generic.TripleExpression;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface GenericParser<T> {
    TripleExpression<T> parse(String expression);
}
