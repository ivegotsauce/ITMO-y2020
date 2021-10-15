package expression.operator;

import java.math.BigInteger;

public class BigIntegerOperator implements Operator<BigInteger> {
    @Override
    public BigInteger add(BigInteger x, BigInteger y) {
        return x.add(y);
    }

    @Override
    public BigInteger and(BigInteger x, BigInteger y) {
        return x.and(y);
    }

    @Override
    public BigInteger divide(BigInteger x, BigInteger y) {
        return x.divide(y);
    }

    @Override
    public BigInteger multiply(BigInteger x, BigInteger y) {
        return x.multiply(y);
    }

    @Override
    public BigInteger or(BigInteger x, BigInteger y) {
        return x.or(y);
    }

    @Override
    public BigInteger subtract(BigInteger x, BigInteger y) {
        return x.subtract(y);
    }

    @Override
    public BigInteger xor(BigInteger x, BigInteger y) {
        return x.xor(y);
    }

    @Override
    public BigInteger abs(BigInteger x) {
        return x.abs();
    }

    @Override
    public BigInteger negate(BigInteger x) {
        return x.negate();
    }

    @Override
    public BigInteger sqrt(BigInteger x) {
        return x.sqrt();
    }

    @Override
    public BigInteger cnst(String str) {
        return new BigInteger(str);
    }
    public BigInteger value(int x) {
        return BigInteger.valueOf(x);
    }

}
