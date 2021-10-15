package expression.exceptions;

public class ExpressionException extends RuntimeException {
    public ExpressionException() {
        super();
    }

    public ExpressionException(String message) {
        super(message);
    }
}
