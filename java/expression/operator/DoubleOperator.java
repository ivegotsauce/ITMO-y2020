package expression.operator;

public class DoubleOperator implements Operator<Double>{
    @Override
    public Double add(Double x, Double y) {
        return x + y;
    }

    @Override
    public Double and(Double x, Double y) {
        return null;
    }

    @Override
    public Double divide(Double x, Double y) {
        return x / y;
    }

    @Override
    public Double multiply(Double x, Double y) {
        return x * y;
    }

    @Override
    public Double or(Double x, Double y) {
        return null;
    }

    @Override
    public Double subtract(Double x, Double y) {
        return x - y;
    }

    @Override
    public Double xor(Double x, Double y) {
        return null;
    }

    @Override
    public Double abs(Double x) {
        return x > 0 ? x : -x;
    }

    @Override
    public Double negate(Double x) {
        return -x;
    }

    @Override
    public Double sqrt(Double x) {
        return Math.sqrt(x);
    }

    @Override
    public Double cnst(String str) {
        return Double.parseDouble(str);
    }

    public Double value(int x) {
        return (double) x;
    }

}
