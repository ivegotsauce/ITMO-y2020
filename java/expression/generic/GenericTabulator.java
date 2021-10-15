package expression.generic;

import expression.exceptions.ExpressionException;
import expression.operator.*;
import expression.parser.GenericExpressionParser;

import java.util.Map;

public class GenericTabulator implements Tabulator{

    private final Map<String, Operator<?>> operators = Map.of(
            "i", new IntegerOperator(),
            "d", new DoubleOperator(),
            "bi", new BigIntegerOperator(),
            "u", new UncheckedIntegerOperator(),
            "l", new LongOperator(),
            "s", new ShortOperator()
    );

    public Object[][][] tabulate(String mode, String expression, int x1, int x2, int y1, int y2, int z1, int z2) {
        Object[][][] ans = new Object[x2 - x1 + 1][y2 - y1 + 1][z2 - z1 + 1];
        for (int i = 0; i <= x2 - x1; i++) {
            for (int j = 0; j <= y2 - y1; j++) {
                for (int k = 0; k <= z2 - z1; k++) {
                    try {
                        ans[i][j][k] = tabulateImpl(operators.get(mode), i, x1, j, y1, k, z1, expression);
                    } catch (ExpressionException | ArithmeticException e) {
                        ans[i][j][k] = null;
                    }
                }
            }
        }
        return ans;
    }
    private <T> T tabulateImpl(Operator<T> op, int i, int x1, int j, int y1, int k, int z1, String expression) {
        TripleExpression<T> expr = new GenericExpressionParser<T>().parse(expression);
        return expr.evaluate(op.value(i + x1), op.value(j + y1), op.value(k + z1), op);
    }
}
