package expression;

public class Xor extends Operation implements Expression, TripleExpression {

    public Xor(ToMiniString exp1, ToMiniString exp2) {
        super(exp1, exp2);
    }

    public int operation(int x, int y) {
        return x ^ y;
    }

    public String getSign() {
        return "^";
    }
}