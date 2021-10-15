package expression;

public class Const implements TripleExpression, Expression {

    protected int val;
    
    public Const(int a) {
        this.val = a;
    }

    @Override
    public int evaluate(int x) {
        return val;
    }

    public int evaluate(int x, int y, int z) {
        return val;
    }

    public String toString() {
        return Integer.toString(val);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj != null && obj.getClass() == Const.class) {
            return this.val == ((Const) obj).val;
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Integer.hashCode(val);
    }
}

