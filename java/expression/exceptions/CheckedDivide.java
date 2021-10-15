package expression.exceptions;

import expression.Expression;
import expression.Operation;
import expression.ToMiniString;
import expression.TripleExpression;

public class CheckedDivide extends Operation implements Expression, TripleExpression {

    public CheckedDivide(ToMiniString exp1, ToMiniString exp2) {
        super(exp1, exp2);
    }

    public int operation(int x, int y) {
        if (y == 0) {
            throw new ExpressionDBZException("Division by zero");
        }
        if (x == Integer.MIN_VALUE && y == -1) {
            throw new ExpressionOverflowException("Integer overflow");
        }
        return x / y;
    }

    public String getSign() {
        return "/";
    }
}

