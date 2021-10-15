package expression;

import java.util.Objects;

public class Variable implements TripleExpression, Expression {
    protected String var;

    public Variable(String str) {
        var = str;
    }

    @Override
    public int evaluate(int x) {
        return x;
    }

    public int evaluate(int x, int y, int z) {
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
            return this.var.equals(((Variable) obj).var);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return var.hashCode();
    }
}
