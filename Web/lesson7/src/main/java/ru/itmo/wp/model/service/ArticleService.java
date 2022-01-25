package ru.itmo.wp.model.service;

import com.google.common.base.Strings;
import ru.itmo.wp.model.domain.Article;
import ru.itmo.wp.model.exception.ValidationException;
import ru.itmo.wp.model.repository.ArticleRepository;
import ru.itmo.wp.model.repository.UserRepository;
import ru.itmo.wp.model.repository.impl.ArticleRepositoryImpl;
import ru.itmo.wp.model.repository.impl.UserRepositoryImpl;

import java.util.List;

public class ArticleService {
    private final ArticleRepository articleRepository = new ArticleRepositoryImpl();
    private final UserRepository userRepository = new UserRepositoryImpl();

    public void validateArticle(Article article) throws ValidationException {
        if (Strings.isNullOrEmpty(article.getTitle())) {
            throw new ValidationException("Title is required");
        }
        if (Strings.isNullOrEmpty(article.getText())) {
            throw new ValidationException("Text is required");
        }
        if (article.getTitle().chars().allMatch(Character::isWhitespace)) {
            throw new ValidationException("Title can't be blank");
        }
        if (article.getText().chars().allMatch(Character::isWhitespace)) {
            throw new ValidationException("Text can't be blank");
        }
        if (userRepository.find(article.getUserId()) == null) {
            throw new ValidationException("No such user");
        }
    }

    public Article find(long id) {
        return articleRepository.find(id);
    }

    public List<Article> findAll() {
        return articleRepository.findAll();
    }

    public List<Article> findByUserId(long userId) {
        return articleRepository.findByUserId(userId);
    }

    public void save(Article article) {
        articleRepository.save(article);
    }

    public void validateSetHidden(long id, long userId, String value) throws ValidationException {
        Article article = articleRepository.find(id);
        if (article == null) {
            throw new ValidationException("No such article");
        }
        if (article.getUserId() != userId) {
            throw new ValidationException("Can't change this article");
        }
        if (!value.equals("Hide") && !value.equals("Show")) {
            throw new ValidationException("Invalid set hide request");
        }
    }

    public void setHidden(long id, String value) {
        articleRepository.setHidden(id, value);
    }
}
