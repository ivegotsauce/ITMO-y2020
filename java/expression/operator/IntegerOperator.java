package expression.operator;

import expression.exceptions.ExpressionDBZException;
import expression.exceptions.ExpressionException;
import expression.exceptions.ExpressionOverflowException;

public class IntegerOperator implements Operator<Integer> {
    public Integer add(Integer x, Integer y) {
        if (x > 0 && y > 0 && (Integer.MAX_VALUE - x < y)) {
            throw new ExpressionOverflowException("integer overflow");
        } else if (x < 0 && y < 0 && Integer.MIN_VALUE - x > y) {
            throw new ExpressionOverflowException("integer overflow");
        }
        return x + y;
    }

    public Integer and(Integer x, Integer y) {
        return x & y;
    }

    public Integer divide (Integer x, Integer y) {
        if (y == 0) {
            throw new ExpressionDBZException("Division by zero");
        }
        if (x == Integer.MIN_VALUE && y == -1) {
            throw new ExpressionOverflowException("Integer overflow");
        }
        return x / y;
    }

    public Integer multiply(Integer x, Integer y) {
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

    public Integer or (Integer x, Integer y) {
        return x | y;
    }

    public Integer subtract (Integer x, Integer y) {
        if (x >= 0 && y <= 0 && Integer.MAX_VALUE - x - 1 < -y - 1) {
            throw new ExpressionOverflowException("Integer overflow");
        } else if (x <= 0 && y >= 0 && Integer.MIN_VALUE + y > x) {
            throw new ExpressionOverflowException("integer overflow");
        }
        return x - y;
    }

    public Integer xor(Integer x, Integer y) {
        return x ^ y;
    }

    public Integer abs(Integer x) {
        if (x == Integer.MIN_VALUE) {
            throw new ExpressionOverflowException("Integer overflow");
        }
        return x < 0 ? x * -1 : x;
    }

    public Integer negate(Integer x) {
        if (x == Integer.MIN_VALUE) {
            throw new ExpressionOverflowException("Integer overflow");
        }
        return x * -1;
    }

    public Integer cnst (String x) {
        return Integer.parseInt(x);
    }

    public Integer sqrt (Integer x) {
        if (x < 0) {
            throw new ExpressionException("Square root of the negative");
        }
        Integer low = 0;
        Integer high = x + 1;
        while (high - low > 1) {
            Integer mid = (low + high) / 2;
            if ((Integer.MAX_VALUE / mid) < mid) {
                high = mid;
            } else if (mid * mid <= x) {
                low = mid;
            } else {
                high = mid;
            }
        }
        return low;
    }

    public Integer value(int x) {
        return x;
    }
}
