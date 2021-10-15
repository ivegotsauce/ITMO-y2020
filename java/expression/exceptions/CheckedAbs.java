package expression.exceptions;

import expression.Expression;
import expression.ToMiniString;
import expression.TripleExpression;

public class CheckedAbs implements Expression, TripleExpression {
    TripleExpression exp;

    public CheckedAbs(ToMiniString exp) {
        this.exp = (TripleExpression) exp;
    }

    @Override
    public int evaluate(int x) {
        return evaluate(x, x, x);
    }

    @Override
    public int evaluate(int x, int y, int z) {
        int a = exp.evaluate(x, y, z);
        if (a == Integer.MIN_VALUE) {
            throw new ExpressionOverflowException("Integer overflow");
        }
        return a < 0 ? a * -1 : a;
    }
}
