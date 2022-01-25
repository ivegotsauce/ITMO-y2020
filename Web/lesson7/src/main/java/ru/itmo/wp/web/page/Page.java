package ru.itmo.wp.web.page;

import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.web.exception.RedirectException;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

public abstract class Page {
    protected static void validateUserLoggedIn(HttpServletRequest request) throws RedirectException {
        User user = (User) request.getSession().getAttribute("user");
        if (user == null) {
            request.getSession().setAttribute("message", "You should be logged in");
            throw new RedirectException("/index");
        }
    }
}
