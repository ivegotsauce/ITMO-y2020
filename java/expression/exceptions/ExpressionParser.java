package expression.exceptions;

import expression.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class ExpressionParser implements Parser {

    private final Map<Character, Character> hashMap = Stream.of(new Character[][]{
            {'(', '('}, {')', ')'}, {'*', '*'},
            {'/', '/'}, {'+', '+'}, {'-', '-'},
            {'&', '&'}, {'^', '^'}, {'|', '|'},
    }).collect(Collectors.toMap(data -> data[0], data -> data[1]));

    private String[] source;
    private int pos;
    private StringBuilder sbr = new StringBuilder();

    public TripleExpression parse(String expression) {
        this.source = smartSplit(expression);
        pos = 0;
        TripleExpression result = or();
        if (pos < source.length) {
            throw new ParsingException(sbr.toString() + " <-- Wrong token: " + source[pos]);
        }
        return result;
    }

    private TripleExpression or() {
        TripleExpression first = xor();
        while (pos < source.length) {
            String operator = source[pos];
            if (!(test(operator, "|", "|"))) {
                break;
            }

            TripleExpression second = xor();
            if (operator.equals("|")) {
                first = new Or(first, second);
            }
        }
        return first;
    }

    private TripleExpression xor() {
        TripleExpression first = and();
        while (pos < source.length) {
            String operator = source[pos];
            if (!(test(operator, "^", "^"))) {
                break;
            }

            TripleExpression second = and();
            if (operator.equals("^")) {
                first = new Xor(first, second);
            }
        }
        return first;
    }

    private TripleExpression and() {
        TripleExpression first = expr();
        while (pos < source.length) {
            String operator = source[pos];
            if (!(test(operator, "&", "&"))) {
                break;
            }

            TripleExpression second = expr();
            if (operator.equals("&")) {
                first = new And(first, second);
            }
        }
        return first;
    }

    private TripleExpression expr() {
        TripleExpression first = term();
        while (pos < source.length) {
            String operator = source[pos];
            if (!(test(operator, "+", "-"))) {
                break;
            }

            TripleExpression second = term();
            if (operator.equals("+")) {
                first = new CheckedAdd(first, second);
            }
            if (operator.equals("-")) {
                first = new CheckedSubtract(first, second);
            }
        }
        return first;
    }

    private TripleExpression term() {
        TripleExpression first = factor(true);

        while (pos < source.length) {
            String operator = source[pos];
            sbr.append(" " + source[pos] + " ");
            if (!(test(operator, "*", "/"))) {
                break;
            }

            TripleExpression second = factor(true);
            if (operator.equals("*")) {
                first = new CheckedMultiply(first, second);
            }
            if (operator.equals("/")) {
                first = new CheckedDivide(first, second);
            }
        }
        return first;
    }

    private TripleExpression factor(boolean ng) {
        try {
            boolean neg = !ng;
            String next = source[pos];
            TripleExpression result;
            if (next.equals("-")) {
                next();
                if (!Character.isDigit(source[pos].charAt(0))) {
                    result = factor(neg);
                    return new CheckedNegate(result);
                } else {
                    next = "-" + source[pos];
                }
            }
            if (next.equals("abs")) {
                next();
                result = factor(!neg);
                return new CheckedAbs(result);
            }
            if (next.equals("sqrt")) {
                next();
                result = factor(!neg);
                return new CheckedSqrt(result);
            }
            if (next.equals("(")) {
                next();
                result = or();
                String nx;
                nx = source[pos];
                if (nx.equals(")")) {
                    next();
                    //  return !neg ? result : new CheckedNegate(result);
                    return result;
                }
                throw new ParsingException(" ')' expected; found: " + " " + nx);
            }
            next();
            if (Character.isLetter(next.charAt(0))) {
                //return !neg ? new Variable(next) : new CheckedNegate(new Variable(next));
                return new Variable(next);
            }

            if (next.charAt(0) == '-' || Character.isDigit(next.charAt(0))) {
                //if (neg) {
                //    next = "-" + next;
                //  }
                return new Const(Integer.parseInt(next));
            }
            throw new ParsingException(sbr.toString() + " <-- Wrong token: " + next);
        } catch (ArrayIndexOutOfBoundsException e) {
            throw new ParsingException(sbr.toString() + " <-- Operand expected");
        } catch (NumberFormatException e) {
            throw new ParsingException(sbr.toString() + " <-- Integer overflow");
        }
    }

    private void next() {
        sbr.append(source[pos]);
        pos++;
    }

    private String[] smartSplit(String expression) {
        List<String> list = new ArrayList();
        int balance = 0;
        for (int i = 0; i < expression.length(); i++) {
            if (Character.isLetter(expression.charAt(i))) {
                char c = expression.charAt(i);
                if (c == 'a' || c == 's') {
                    StringBuilder sb = new StringBuilder();
                    while (i < expression.length() && Character.isLetter(expression.charAt(i))) {
                        sb.append(expression.charAt(i++));
                    }
                    String s = sb.toString();
                    if (s.equals("abs") || s.equals("sqrt")) {
                        list.add(s);
                    } else {
                        throw new ParsingException(expression.substring(0, i) + " <-- Wrong instruction in input");
                    }
                } else if ((c != 'x' && c != 'y' && c != 'z') || i + 1 < expression.length() &&
                        !Character.isWhitespace(expression.charAt(i + 1)) && !hashMap.containsKey(expression.charAt(i + 1))) {
                    throw new ParsingException(expression.substring(0, i + 2) + " <-- Wrong symbol in input: " + expression.charAt(i + 1));
                } else {
                    list.add(String.valueOf(expression.charAt(i)));
                }
            }
            if (i < expression.length() && Character.isDigit(expression.charAt(i))) {
                StringBuilder sb = new StringBuilder();
                while (i < expression.length() && Character.isDigit(expression.charAt(i))) {
                    if (i + 1 < expression.length() && !Character.isWhitespace(expression.charAt(i + 1)) &&
                            !Character.isDigit(expression.charAt(i + 1)) && !hashMap.containsKey(expression.charAt(i + 1))) {
                        throw new ParsingException(expression.substring(0, i + 2) + " <-- Wrong symbol in input: " + expression.charAt(i + 1));
                    }
                    sb.append(expression.charAt(i++));
                }
                list.add(sb.toString());
            }
            if (i < expression.length() && hashMap.containsKey(expression.charAt(i))) {
                list.add(String.valueOf(expression.charAt(i)));
            }
            if (i < expression.length() && !Character.isWhitespace(expression.charAt(i)) &&
                    !Character.isLetterOrDigit(expression.charAt(i)) && !hashMap.containsKey(expression.charAt(i))) {
                throw new ParsingException(expression.substring(0, i) + " <-- Wrong symbol in input: " + expression.charAt(i));
            }
            if (i < expression.length() && expression.charAt(i) == '(') {
                balance++;
            }
            if (i < expression.length() && expression.charAt(i) == ')') {
                balance--;
            }

        }
        if (balance != 0) {
            throw new ParsingException(expression + " <-- Wrong brackets arrangement");
        }
        return list.toArray(new String[0]);
    }

    private boolean test(String s1, String s2, String s3) {
        if (!(s1.equals(s2)) && !(s1.equals(s3))) {
            return false;
        } else {
            pos++;
            return true;
        }
    }

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        ExpressionParser parser = new ExpressionParser();
        while (true) {
            String expr = scanner.nextLine();
            System.out.println(parser.parse(expr).evaluate(3, 4 ,5));
        }
    }
}
