package expression.operator;

import java.rmi.MarshalException;

public class UncheckedIntegerOperator implements Operator<Integer>{
    @Override
    public Integer add(Integer x, Integer y) {
        return x + y;
    }

    @Override
    public Integer and(Integer x, Integer y) {
        return x & y;
    }

    @Override
    public Integer divide(Integer x, Integer y) {
        return x / y;
    }

    @Override
    public Integer multiply(Integer x, Integer y) {
        return x * y;
    }

    @Override
    public Integer or(Integer x, Integer y) {
        return x | y;
    }

    @Override
    public Integer subtract(Integer x, Integer y) {
        return x - y;
    }

    @Override
    public Integer xor(Integer x, Integer y) {
        return x ^ y;
    }

    @Override
    public Integer abs(Integer x) {
        return x > 0 ? x : -x;
    }

    @Override
    public Integer negate(Integer x) {
        return -x;
    }

    @Override
    public Integer sqrt(Integer x) {
        return (int) Math.sqrt(x);
    }

    @Override
    public Integer cnst(String str) {
        return Integer.parseInt(str);
    }

    @Override
    public Integer value(int x) {
        return x;
    }
}
