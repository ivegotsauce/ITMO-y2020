package ru.itmo.wp.web.page;

import ru.itmo.wp.model.domain.Article;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.ValidationException;
import ru.itmo.wp.model.service.ArticleService;
import ru.itmo.wp.web.exception.RedirectException;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

public class ArticlePage extends Page {
    private final ArticleService articleService = new ArticleService();

    private void action(HttpServletRequest request, Map<String, Object> view) {
        validateUserLoggedIn(request);
    }

    private void create(HttpServletRequest request, Map<String, Object> view) throws ValidationException {
        Article article = new Article();
        validateUserLoggedIn(request);
        User user = (User) request.getSession().getAttribute("user");
        String title = request.getParameter("title");
        String text = request.getParameter("text");
        article.setUserId(user.getId());
        article.setTitle(title);
        article.setText(text);
        article.setHidden(false);
        articleService.validateArticle(article);
        articleService.save(article);
        request.getSession().setAttribute("message", "Article was successfully created!");
        throw new RedirectException("/index");
    }
}
