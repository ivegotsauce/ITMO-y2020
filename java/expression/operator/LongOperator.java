package expression.operator;

public class LongOperator implements Operator<Long> {
    @Override
    public Long add(Long x, Long y) {
        return x + y;
    }

    @Override
    public Long and(Long x, Long y) {
        return x & y;
    }

    @Override
    public Long divide(Long x, Long y) {
        return x / y;
    }

    @Override
    public Long multiply(Long x, Long y) {
        return x * y;
    }

    @Override
    public Long or(Long x, Long y) {
        return x | y;
    }

    @Override
    public Long subtract(Long x, Long y) {
        return x - y;
    }

    @Override
    public Long xor(Long x, Long y) {
        return x ^ y;
    }

    @Override
    public Long abs(Long x) {
        return x > 0 ? x : -x;
    }

    @Override
    public Long negate(Long x) {
        return -x;
    }

    @Override
    public Long sqrt(Long x) {
        return (long) Math.sqrt(x);
    }

    @Override
    public Long cnst(String str) {
        return Long.parseLong(str);
    }

    @Override
    public Long value(int x) {
        return (long) x;
    }
}
