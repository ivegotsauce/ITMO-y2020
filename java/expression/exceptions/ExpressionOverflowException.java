package expression.exceptions;

public class ExpressionOverflowException extends ExpressionException {
    public ExpressionOverflowException() {
        super();
    }

    public ExpressionOverflowException(String message) {
        super(message);
    }
}
