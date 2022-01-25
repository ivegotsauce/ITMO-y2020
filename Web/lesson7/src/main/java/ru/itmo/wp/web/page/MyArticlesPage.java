package ru.itmo.wp.web.page;

import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.ValidationException;
import ru.itmo.wp.model.service.ArticleService;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

public class MyArticlesPage  extends Page{
    private final ArticleService articleService = new ArticleService();

    private void action(HttpServletRequest request, Map<String, Object> view) {
        User user = (User) request.getSession().getAttribute("user");
        validateUserLoggedIn(request);
        view.put("articles", articleService.findByUserId(user.getId()));
    }

    private void setHidden(HttpServletRequest request, Map<String, Object> view) throws ValidationException {
        User user = (User) request.getSession().getAttribute("user");
        validateUserLoggedIn(request);
        long id = Long.parseLong(request.getParameter("id"));
        String value = request.getParameter("value");
        articleService.validateSetHidden(id, user.getId(), value);
        articleService.setHidden(id, value);
        view.put("hidden", articleService.find(id).getHidden());
    }
}
