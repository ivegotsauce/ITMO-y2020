package expression;

import java.util.Objects;

public abstract class Operation implements Expression, TripleExpression {
    protected TripleExpression e1, e2;

    public Operation(ToMiniString exp1, ToMiniString exp2) {
        this.e1 = (TripleExpression) exp1;
        this.e2 = (TripleExpression) exp2;
    }

    public int evaluate(int x) {
        return evaluate(x, x, x);
    }

    @Override
    public int evaluate(int x, int y, int z) {
        int first = e1.evaluate(x, y, z);
        int second = e2.evaluate(x, y, z);
        return operation(first, second);
    }

    public String toString() {
        return "(" + e1.toString() + " " + getSign() + " " + e2.toString() + ")";
    }

    public abstract int operation(int first, int second);

    public abstract String getSign();

    public int hashCode() {
        return e1.hashCode() * 32 + 20 * e2.hashCode() + (int)getSign().charAt(0);
    }

    public boolean equals(Object obj) {
        if (obj != null && obj.getClass() == getClass()) {
            Operation that = (Operation) obj;
            return this.e1.equals(that.e1) && this.e2.equals(that.e2);
        }
        return false;
    }
}