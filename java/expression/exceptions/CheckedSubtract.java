package expression.exceptions;

import expression.Expression;
import expression.Operation;
import expression.ToMiniString;
import expression.TripleExpression;

public class CheckedSubtract extends Operation implements Expression, TripleExpression {

    public CheckedSubtract(ToMiniString exp1, ToMiniString exp2) {
        super(exp1, exp2);
    }

    public int operation(int x, int y) {
        if (x >= 0 && y <= 0 && Integer.MAX_VALUE - x - 1 < -y - 1) {
            throw new ExpressionOverflowException("integer overflow");
        } else if (x <= 0 && y >= 0 && Integer.MIN_VALUE + y > x) {
            throw new ExpressionOverflowException("integer overflow");
        }
        return x - y;
    }

    public String getSign() {
        return "-";
    }
}
