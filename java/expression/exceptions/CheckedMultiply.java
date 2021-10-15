package expression.exceptions;

import expression.Expression;
import expression.Operation;
import expression.ToMiniString;
import expression.TripleExpression;

public class CheckedMultiply extends Operation implements Expression, TripleExpression {

    public CheckedMultiply(ToMiniString exp1, ToMiniString exp2) {
        super(exp1, exp2);
    }

    public int operation(int x, int y) {
        if (x > 0 && y > 0 && (Integer.MAX_VALUE / x) < y) {
            throw new ExpressionOverflowException("Integer overflow");
        }
        if (x < 0 && y < 0 && (Integer.MAX_VALUE / x) > y) {
            throw new ExpressionOverflowException("Integer overflow");
        }
        if (x < -1 && y > 0 && ((Integer.MIN_VALUE / x) < y)) {
            throw new ExpressionOverflowException("Integer overflow");
        }
        if (x > 0 && y < -1 && ((Integer.MIN_VALUE / y) < x)) {
            throw new ExpressionOverflowException("Integer overflow");
        }
        return x * y;
    }

    public String getSign() {
        return "*";
    }
}
