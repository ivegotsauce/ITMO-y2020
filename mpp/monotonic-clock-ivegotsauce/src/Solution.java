import org.jetbrains.annotations.NotNull;

import java.util.Arrays;

/**
 * В теле класса решения разрешено использовать только финальные переменные типа RegularInt.
 * Нельзя volatile, нельзя другие типы, нельзя блокировки, нельзя лазить в глобальные переменные.
 *
 * @author :Aleksandr Sharipov
 */
public class Solution implements MonotonicClock {
    private final RegularInt c1 = new RegularInt(0);
    private final RegularInt c2 = new RegularInt(0);
    private final RegularInt c3 = new RegularInt(0);

    private final RegularInt p1 = new RegularInt(0);
    private final RegularInt p2 = new RegularInt(0);
    private final RegularInt p3 = new RegularInt(0);

    @Override
    public void write(@NotNull Time time) {
        p1.setValue(time.getD1());
        p2.setValue(time.getD2());
        p3.setValue(time.getD3());

        c3.setValue(p3.getValue());
        c2.setValue(p2.getValue());
        c1.setValue(p1.getValue());
    }

    @NotNull
    @Override
    public Time read() {
        int[] r1 = new int[3];
        int[] r2 = new int[3];
        r1[0] = c1.getValue();
        r1[1] = c2.getValue();
        r1[2] = c3.getValue();

        r2[2] = p3.getValue();
        r2[1] = p2.getValue();
        r2[0] = p1.getValue();

        if (Arrays.equals(r1, r2)) {
            return new Time(r1[0], r1[1], r1[2]);
        }

        int x = 0;
        for (int i = 0; i < 3; i++) {
            if (r1[i] == r2[i]) {
                x++;
            } else {
                break;
            }
        }

        int[] ans = new int[3];
        System.arraycopy(r2, 0, ans, 0, Math.min(3, x + 1));

        return new Time(ans[0], ans[1], ans[2]);
    }
}
