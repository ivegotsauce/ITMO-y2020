package expression.parser;

import expression.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class ExpressionParser implements Parser {

    private Map<Character, Character> hashMap = Stream.of(new Character[][]{
            { '(', '(' }, { ')', ')' }, { '*', '*' },
            { '/', '/' }, { '+', '+' }, { '-', '-' },
            { '&', '&' }, { '^', '^' }, { '|', '|' },
    }).collect(Collectors.toMap(data -> data[0], data -> data[1]));

    private String[] source;
    private int pos;

    public TripleExpression parse(String expression) {
        this.source = smartSplit(expression);
        pos = 0;
        TripleExpression result = or();
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
                first = new Add(first, second);
            }
            if (operator.equals("-")) {
                first = new Subtract(first, second);
            }
        }
        return first;
    }

    private TripleExpression term() {
        TripleExpression first = factor(true);

        while (pos < source.length) {
            String operator = source[pos];
            if (!(test(operator, "*", "/"))) {
                break;
            }

            TripleExpression second = factor(true);
            if (operator.equals("*")) {
                first = new Multiply(first, second);
            }
            if (operator.equals("/")) {
                first = new Divide(first, second);
            }
        }
        return first;
    }

    private TripleExpression factor(boolean ng) {
        boolean neg = !ng;
        String next = source[pos];
        TripleExpression result;
        if (next.equals("-")) {
            pos++;
            return factor(neg);
        }
        if (next.equals("(")) {
            pos++;
            result = or();
            String nx;
            nx = source[pos];
            if(nx.equals(")")) {
                pos++;
                return !neg ? result : new Multiply(new Const(-1), result);
            }
            throw new Error("error"  + " " + nx);
        }
        pos++;
        if (Character.isLetter(next.charAt(0))) {
            return !neg ? new Variable(next) : new Multiply(new Const(-1), new Variable(next));
        }

        if (Character.isDigit(next.charAt(0))) {
            if(neg) {
                next = "-" + next;
            }
            return new Const(Integer.parseInt(next));
        }
        else throw new Error("Wrong input is:" + next + " " + source[pos] + " ");
    }

    private String[] smartSplit(String expression) {
        List<String> list = new ArrayList();
        for (int i = 0; i < expression.length(); i++) {
            if (i < expression.length() && Character.isLetter(expression.charAt(i))) {
                StringBuilder sb = new StringBuilder();
                while (i < expression.length() && Character.isLetter(expression.charAt(i))) {
                    sb.append(expression.charAt(i++));
                }
                list.add(sb.toString());
            }
            if (i < expression.length() && Character.isDigit(expression.charAt(i))) {
                StringBuilder sb = new StringBuilder();
                while (i < expression.length() && Character.isDigit(expression.charAt(i))) {
                    sb.append(expression.charAt(i++));
                }
                list.add(sb.toString());
            }
            if(i < expression.length() && hashMap.containsKey(expression.charAt(i))) {
                list.add(String.valueOf(expression.charAt(i)));
            }
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
}
