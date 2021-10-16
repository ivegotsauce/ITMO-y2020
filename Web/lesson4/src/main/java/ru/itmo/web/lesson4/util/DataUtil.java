package ru.itmo.web.lesson4.util;

import ru.itmo.web.lesson4.model.Post;
import ru.itmo.web.lesson4.model.User;

import javax.servlet.http.HttpServletRequest;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

public class DataUtil {
    private static final List<User> USERS = Arrays.asList(
            new User(1, "MikeMirzayanov", "Mike Mirzayanov", User.Color.GREEN),
            new User(6, "pashka", "Pavel Mavrin", User.Color.RED),
            new User(9, "geranazarov555", "Georgiy Nazarov", User.Color.BLUE),
            new User(11, "tourist", "Gennady Korotkevich", User.Color.RED)
    );

    private static final List<Post> POSTS = Arrays.asList(
            new Post(1, "ICPC World Finals Moscow",
                    "Hello, Codeforces!\n" +
                            "\n" +
                            "ICPC World Finals Moscow will begin on October 5, " +
                            "2021 at 8:30 (UTC+3). We are thrilled to invite you to " +
                            "join the live broadcast of the main event of the year in " +
                            "the world of sports programming!\n" +
                            "\n" +
                            "For the very first time in October 2021, Moscow will host " +
                            "the world’s most prestigious competition for young IT talents, " +
                            "the ICPC World Finals Championship. The last International " +
                            "Collegiate Programming Contest has hosted over 60000 students from " +
                            "3,514 universities in 115 countries that span the globe. October 5 more " +
                            "than 100 teams will compete in logic, mental speed, and strategic thinking " +
                            "at Russia’s main Manege Central Conference Hall.", 1),
            new Post(2, "touristream 012: Codeforces Round 672 (Div. 2)",
                    "Tune in to my Twitch to watch me solve today's " +
                    "Codeforces Round 672 (Div. 2) virtually.", 11),
            new Post(3, "Видеолекции моего курса в ИТМО ", "В этом семестре записал на видео все " +
                    "лекции курса \"Алгоритмы и структуры данных\", который я читаю в ИТМО. Лекции стримились " +
                    "в прямом эфире на твич и потом выкладывались на ютуб.\n" +
                    "\n" +
                    "Курс скорее академический, а не олимпиадный, но думаю многим начинающим (и не только) " +
                    "олимпиадникам тоже будет интересно.", 6),
            new Post(4, "Поймал кота", "Сегодня поймал очень красивого, пушистого кота!", 6)
    );

    public static void addData(HttpServletRequest request, Map<String, Object> data) {
        data.put("users", USERS);
        data.put("posts", POSTS);

        for (User user : USERS) {
            if (Long.toString(user.getId()).equals(request.getParameter("logged_user_id"))) {
                data.put("user", user);
            }
        }
    }
}
