package ru.itmo.wp.web.page;

import com.google.common.base.Strings;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.service.UserService;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

@SuppressWarnings({"unused", "RedundantSuppression"})
public abstract class Page {
    private final UserService userService = new UserService();
    protected HttpServletRequest request;

    protected void before(HttpServletRequest request, Map<String, Object> view) {
        this.request = request;
        User user = getUser();
        if (user != null) {
            view.put("user", user);
        }
        String message = (String) request.getSession().getAttribute("message");
        if (!Strings.isNullOrEmpty(message)) {
            view.put("message", message);
            request.getSession().removeAttribute("message");
        }
        view.put("userCount", userService.findCount());
    }

    private void action(HttpServletRequest request, Map<String, Object> view) {
        // No operations
    }

    protected void after(HttpServletRequest request, Map<String, Object> view) {
        // No operations
    }

    void setMessage(String message) {
        request.getSession().setAttribute("message", message);
    }

    User getUser() {
        return (User) request.getSession().getAttribute("user");
    }

    void setUser(User user) {
        request.getSession().setAttribute("user", user);
    }

}
