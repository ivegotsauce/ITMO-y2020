package expression;

import expression.generic.Expression;
import expression.generic.ToMiniString;
import expression.generic.TripleExpression;
import expression.operator.Operator;

public class And<T> extends Operation<T> implements Expression<T>, TripleExpression<T> {

    public And(ToMiniString<T> exp1, ToMiniString<T> exp2) {
        super(exp1, exp2);
    }

    public T operation(T x, T y, Operator<T> op) {
        return op.and(x, y);
    }

    public String getSign() {
        return "&";
    }
}