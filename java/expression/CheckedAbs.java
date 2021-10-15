package expression;

import expression.generic.Expression;
import expression.generic.ToMiniString;
import expression.generic.TripleExpression;
import expression.operator.Operator;

public class CheckedAbs<T> implements Expression<T>, TripleExpression<T> {
    TripleExpression<T> exp;

    public CheckedAbs(ToMiniString<T> exp) {
        this.exp = (TripleExpression<T>) exp;
    }

    @Override
    public T evaluate(T x, Operator<T> op) {
        return evaluate(x, x, x, op);
    }

    @Override
    public T evaluate(T x, T y, T z, Operator<T> op) {
        T a = exp.evaluate(x, y, z, op);
        return operation(a, op);
    }

    public T operation(T x, Operator<T> op) {
        return op.abs(x);
    }
}
