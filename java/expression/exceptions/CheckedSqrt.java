package expression.exceptions;

import expression.Expression;
import expression.ToMiniString;
import expression.TripleExpression;

public class CheckedSqrt implements Expression, TripleExpression {
    TripleExpression exp;

    public CheckedSqrt(ToMiniString exp) {
        this.exp = (TripleExpression) exp;
    }

    @Override
    public int evaluate(int x) {
        return evaluate(x, x, x);
    }

    @Override
    public int evaluate(int x, int y, int z) {
        int a = exp.evaluate(x, y, z);
        if (a < 0) {
            throw new ExpressionException("Square root of the negative");
        }
        int low = 0;
        int high = a + 1;
        while (high - low > 1) {
            int mid = (low + high) / 2;
            if ((Integer.MAX_VALUE / mid) < mid) {
                high = mid;
            } else if (mid * mid <= a) {
                low = mid;
            } else {
                high = mid;
            }
        }
        return low;
    }

    public String toString() {
        return "Sqrt(" + exp.toString() + ")";
    }
}
