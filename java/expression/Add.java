package expression;

import expression.exceptions.ExpressionOverflowException;

public class Add extends Operation implements Expression, TripleExpression {

    public Add(ToMiniString exp1, ToMiniString exp2) {
        super(exp1, exp2);
    }

    public int operation(int x, int y) {
        return x + y;
    }

    public String getSign() {
        return "+";
    }
}
