package expression.exceptions;

public class ParsingException extends RuntimeException {
    public ParsingException() {
        super();
    }

    public ParsingException(String message) {
        super(message);
    }
}
