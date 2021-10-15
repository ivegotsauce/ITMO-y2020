package expression;

import expression.generic.Expression;
import expression.generic.TripleExpression;
import expression.operator.Operator;

public class Variable<T> implements TripleExpression<T>, Expression<T> {
    protected String var;

    public Variable(String str) {
        var = str;
    }

    @Override
    public T evaluate(T x, Operator<T> op) {
        return x;
    }

    public T evaluate(T x, T y, T z, Operator<T> op) {
        if(var.equals("x")) return x;
        if(var.equals("y")) return y;
        if(var.equals("z")) return z;
        throw new AssertionError("Invalid variable");
    }

    public String toString() {
        return var;
    }

    public boolean equals(Object obj) {
        if (obj != null && obj.getClass() == Variable.class) {
            @SuppressWarnings("unchecked") Variable<T> that = (Variable<T>) obj;
            return this.var.equals(that.var);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return var.hashCode();
    }
}
