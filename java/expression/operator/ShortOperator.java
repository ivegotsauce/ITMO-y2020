package expression.operator;

public class ShortOperator implements Operator<Short>{
    @Override
    public Short add(Short x, Short y) {
        return (short) (x + y);
    }

    @Override
    public Short and(Short x, Short y) {
        return (short) (x & y);
    }

    @Override
    public Short divide(Short x, Short y) {
        return (short)(x/y);
    }

    @Override
    public Short multiply(Short x, Short y) {
        return (short)(x*y);
    }

    @Override
    public Short or(Short x, Short y) {
        return (short)(x|y);
    }

    @Override
    public Short subtract(Short x, Short y) {
        return (short)(x-y);
    }

    @Override
    public Short xor(Short x, Short y) {
        return (short)(x^y);
    }

    @Override
    public Short abs(Short x) {
        return x > 0 ? x : (short) -x;
    }

    @Override
    public Short negate(Short x) {
        return (short) -x;
    }

    @Override
    public Short sqrt(Short x) {
        return (short) Math.sqrt(x);
    }

    @Override
    public Short cnst(String str) {
        return Short.parseShort(str);
    }

    @Override
    public Short value(int x) {
        return (short) x;
    }
}
