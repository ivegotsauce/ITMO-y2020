package expression;

import expression.generic.Expression;
import expression.generic.ToMiniString;
import expression.generic.TripleExpression;
import expression.operator.Operator;

public abstract class Operation<T> implements Expression<T>, TripleExpression<T> {
    protected TripleExpression<T> e1, e2;

    public Operation(ToMiniString<T> exp1, ToMiniString<T> exp2) {
        this.e1 = (TripleExpression<T>) exp1;
        this.e2 = (TripleExpression<T>) exp2;
    }

    public T evaluate(T x, Operator<T> op) {
        return evaluate(x, x, x, op);
    }

    @Override
    public T evaluate(T x, T y, T z, Operator<T> op) {
        T first = e1.evaluate(x, y, z, op);
        T second = e2.evaluate(x, y, z, op);
        return operation(first, second, op);
    }

    public String toString() {
        return "(" + e1.toString() + " " + getSign() + " " + e2.toString() + ")";
    }

    public abstract T operation(T first, T second, Operator<T> op);

    public abstract String getSign();

    public int hashCode() {
        return e1.hashCode() * 32 + 20 * e2.hashCode() + (int)getSign().charAt(0);
    }

    public boolean equals(Object obj) {
        if (obj != null && obj.getClass() == getClass()) {
            @SuppressWarnings("unchecked") Operation<T> that = (Operation<T>) obj;
            return this.e1.equals(that.e1) && this.e2.equals(that.e2);
        }
        return false;
    }
}