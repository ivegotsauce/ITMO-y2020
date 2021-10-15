package expression.operator;

public interface Operator<T> {
    T add(T x, T y);
    T and(T x, T y);
    T divide (T x, T y);
    T multiply(T x, T y);
    T or (T x, T y);
    T subtract (T x, T y);
    T xor(T x, T y);
    T abs(T x);
    T negate(T x);
    T sqrt (T x);
    T cnst (String str);
    T value(int x);
}
