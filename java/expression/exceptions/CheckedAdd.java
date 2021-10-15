package expression.exceptions;

import expression.Expression;
import expression.Operation;
import expression.ToMiniString;
import expression.TripleExpression;

import java.nio.BufferOverflowException;
import java.util.InputMismatchException;

public class CheckedAdd extends Operation implements Expression, TripleExpression {

    public CheckedAdd(ToMiniString exp1, ToMiniString exp2) {
        super(exp1, exp2);
    }

    public int operation(int x, int y) {
        if (x > 0 && y > 0 && (Integer.MAX_VALUE - x < y)) {
            throw new ExpressionOverflowException("integer overflow");
        } else if (x < 0 && y < 0 && Integer.MIN_VALUE - x > y) {
            throw new ExpressionOverflowException("integer overflow");
        }
        return x + y;
    }

    public String getSign() {
        return "+";
    }
}

