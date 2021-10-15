package expression.parser;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import expression.*;
import expression.generic.TripleExpression;

public class GenericExpressionParser<T> implements GenericParser<T> {

    private Map<Character, Character> hashMap = Stream.of(new Character[][]{
            { '(', '(' }, { ')', ')' }, { '*', '*' },
            { '/', '/' }, { '+', '+' }, { '-', '-' },
            { '&', '&' }, { '^', '^' }, { '|', '|' },
    }).collect(Collectors.toMap(data -> data[0], data -> data[1]));

    private String[] source;
    private int pos;

    public TripleExpression<T> parse(String expression) {
        this.source = smartSplit(expression);
        pos = 0;
        TripleExpression<T> result = or();
        return result;
    }
    private TripleExpression<T> or() {
        TripleExpression<T> first = xor();
        while (pos < source.length) {
            String operator = source[pos];
            if (!(test(operator, "|", "|"))) {
                break;
            }

            TripleExpression<T> second = xor();
            if (operator.equals("|")) {
                first = new Or<T>(first, second);
            }
        }
        return first;
    }
    private TripleExpression<T> xor() {
        TripleExpression<T> first = and();
        while (pos < source.length) {
            String operator = source[pos];
            if (!(test(operator, "^", "^"))) {
                break;
            }

            TripleExpression<T> second = and();
            if (operator.equals("^")) {
                first = new Xor<T>(first, second);
            }
        }
        return first;
    }
    private TripleExpression<T> and() {
        TripleExpression<T> first = expr();
        while (pos < source.length) {
            String operator = source[pos];
            if (!(test(operator, "&", "&"))) {
                break;
            }

            TripleExpression<T> second = expr();
            if (operator.equals("&")) {
                first = new And<T>(first, second);
            }
        }
        return first;
    }

    private TripleExpression<T> expr() {
        TripleExpression<T> first = term();
        while (pos < source.length) {
            String operator = source[pos];
            if (!(test(operator, "+", "-"))) {
                break;
            }

            TripleExpression<T> second = term();
            if (operator.equals("+")) {
                first = new Add<T>(first, second);
            }
            if (operator.equals("-")) {
                first = new Subtract<T>(first, second);
            }
        }
        return first;
    }

    private TripleExpression<T> term() {
        TripleExpression<T> first = factor(true);

        while (pos < source.length) {
            String operator = source[pos];
            if (!(test(operator, "*", "/"))) {
                break;
            }

            TripleExpression<T> second = factor(true);
            if (operator.equals("*")) {
                first = new Multiply<T>(first, second);
            }
            if (operator.equals("/")) {
                first = new Divide<T>(first, second);
            }
        }
        return first;
    }

    private TripleExpression<T> factor(boolean ng) {
        boolean neg = !ng;
        String next = source[pos];
        TripleExpression<T> result;
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
                return !neg ? result : new Multiply<T>(new Const<T>("-1"), result);
            }
            throw new Error("error"  + " " + nx);
        }
        pos++;
        if (Character.isLetter(next.charAt(0))) {
            return !neg ? new Variable<T>(next) : new Multiply<T>(new Const<T>("-1"), new Variable<T>(next));
        }

        if (Character.isDigit(next.charAt(0))) {
            if(neg) {
                next = "-" + next;
            }
            return new Const<T>(next);
        }
        else throw new Error("Wrong input is:" + next + " " + source[pos] + " ");
    }

    private String[] smartSplit(String expression) {
        List<String> list = new ArrayList();
        for (int i = 0; i < expression.length(); i++) {
            if (Character.isLetter(expression.charAt(i))) {
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
