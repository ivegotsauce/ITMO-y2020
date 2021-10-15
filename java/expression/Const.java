package expression;

import expression.generic.Expression;
import expression.generic.TripleExpression;
import expression.operator.Operator;

import java.util.Objects;

public class Const<T> implements TripleExpression<T>, Expression<T> {

    protected String val;
    
    public Const(String str) {
        this.val = str;
    }

    @Override
    public T evaluate(T x, Operator <T> op) {
        return op.cnst(val);
    }

    public T evaluate(T x, T y, T z, Operator<T> op) {
        return op.cnst(val);
    }

    public String toString() {
        return val;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj != null && obj.getClass() == Const.class) {
            @SuppressWarnings("unchecked")Const<T> that = (Const<T>) obj;
            return this.val.equals(that.val);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(val);
    }
}

