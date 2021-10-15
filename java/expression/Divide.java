package expression;

import expression.generic.Expression;
import expression.generic.ToMiniString;
import expression.generic.TripleExpression;
import expression.operator.Operator;

public class Divide<T> extends Operation<T> implements Expression<T>, TripleExpression<T> {

    public Divide(ToMiniString<T> exp1, ToMiniString<T> exp2) {
        super(exp1, exp2);
    }

    public T operation(T x, T y, Operator<T> op) {
        return op.divide(x, y);
    }

    public String getSign() {
        return "/";
    }
}

