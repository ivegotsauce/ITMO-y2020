package com.company;


import java.util.*;

public class Main {

    public interface LogicalExpression {
        boolean evaluate(Map<String, Boolean> map);
    }

    static abstract private class LogicalOperation implements LogicalExpression {
        private final LogicalExpression e1, e2;

        public LogicalOperation(LogicalExpression e1, LogicalExpression e2) {
            this.e1 = e1;
            this.e2 = e2;
        }

        @Override
        public boolean evaluate(Map<String, Boolean> map) {
            boolean first = e1.evaluate(map);
            boolean second = e2.evaluate(map);
            return evaluateImpl(first, second);
        }

        abstract boolean evaluateImpl(boolean first, boolean second);
    }

    static class Or extends LogicalOperation implements LogicalExpression {

        public Or(LogicalExpression e1, LogicalExpression e2) {
            super(e1, e2);
        }

        @Override
        public boolean evaluateImpl(boolean first, boolean second) {
            return first || second;
        }
    }

    static class And extends LogicalOperation implements LogicalExpression {

        public And(LogicalExpression e1, LogicalExpression e2) {
            super(e1, e2);
        }

        @Override
        public boolean evaluateImpl(boolean first, boolean second) {
            return first && second;
        }
    }

    static class Implication extends LogicalOperation implements LogicalExpression {

        public Implication(LogicalExpression e1, LogicalExpression e2) {
            super(e1, e2);
        }

        @Override
        public boolean evaluateImpl(boolean first, boolean second) {
            return !first || second;
        }
    }

    static class Not implements LogicalExpression {

        private final LogicalExpression e;

        public Not(LogicalExpression e) {
            this.e = e;
        }

        @Override
        public boolean evaluate(Map<String, Boolean> map) {
            return !e.evaluate(map);
        }
    }

    static class Variable implements LogicalExpression {

        String name;

        public Variable(String name) {
            this.name = name;
        }

        @Override
        public boolean evaluate(Map<String, Boolean> map) {
            return map.get(name);
        }
    }

    public static class LogicalExpressionParser {

        private char[] source;
        private int pos;
        private final HashSet<String> variables = new HashSet<>();

        public LogicalExpression parse(String expression) {
            source = expression.toCharArray();
            pos = 0;
            return impl();
        }

        public HashSet<String> getVariables() {
            return variables;
        }

        private LogicalExpression impl() {
            skipWs();
            LogicalExpression first = or();
            skipWs();
            while (pos < source.length) {
                skipWs();
                if (source[pos] != '-') {
                    break;
                }
                pos++;
                skipWs();
                if (source[pos] != '>') {
                    break;
                }
                pos++;
                first = new Implication(first, impl());
                skipWs();
            }
            return first;
        }

        private LogicalExpression or() {
            skipWs();
            LogicalExpression first = and();
            skipWs();
            while (pos < source.length) {
                skipWs();
                char operator = source[pos];
                if (operator != '|') {
                    break;
                }
                pos++;
                LogicalExpression second = and();
                first = new Or(first, second);
                skipWs();
            }
            return first;
        }

        private LogicalExpression and() {
            skipWs();
            LogicalExpression first = not();
            skipWs();
            while (pos < source.length) {
                skipWs();
                char operator = source[pos];
                if (operator != '&') {
                    break;
                }
                pos++;
                LogicalExpression second = not();
                first = new And(first, second);
                skipWs();
            }
            return first;
        }

        private LogicalExpression not() {
            skipWs();
            char next = source[pos];
            LogicalExpression result;
            if (next == '!') {
                pos++;
                return new Not(not());
            }

            if (next == '(') {
                pos++;
                result = impl();
                if (source[pos] == ')') {
                    pos++;
                    return result;
                }
            }

            StringBuilder var = new StringBuilder();
            while (pos < source.length &&
                    (Character.isLetter(source[pos]) || Character.isDigit(source[pos]) ||
                            source[pos] == '\'')) {
                var.append(source[pos]);
                pos++;
            }
            String variable = var.toString();
            variables.add(variable);
            return new Variable(variable);
        }

        private void skipWs() {
            while (pos < source.length && Character.isWhitespace(source[pos])) {
                pos++;
            }
        }
    }


    public static void main(String[] args) {
        LogicalExpressionParser parser = new LogicalExpressionParser();
        Scanner sc = new Scanner(System.in);
        String str = sc.nextLine();
        LogicalExpression expression = parser.parse(str);
        ArrayList<String> vars = new ArrayList<>(parser.getVariables());
        boolean valid = true;
        boolean satisfiable = false;
        int trues = 0;
        int falses = 0;
        for (long i = 0; i < 1L << vars.size(); i++) {
            Map<String, Boolean> map = new HashMap<>();
            long j = i;
            StringBuilder sb = new StringBuilder();
            do {
                sb.append(j % 2);
                j /= 2;
            } while (j != 0);
            while (sb.length() < vars.size()) {
                sb.append('0');
            }
            sb.reverse();
            for (int k = 0; k < vars.size(); k++) {
                if (sb.charAt(k) == '0') {
                    map.put(vars.get(k), false);
                } else {
                    map.put(vars.get(k), true);
                }
            }
            boolean tmp = expression.evaluate(map);
            if (tmp) {
                trues++;
            } else {
                falses++;
            }
            valid = valid && tmp;
            satisfiable = satisfiable || tmp;
        }
        if (valid) {
            System.out.println("Valid");
        } else if (!satisfiable) {
            System.out.println("Unsatisfiable");
        } else {
            System.out.printf("Satisfiable and invalid, %d true and %d false cases\n", trues, falses);
        }
    }
}
