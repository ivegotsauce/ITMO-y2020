package ru.itmo.wp.model.repository;

import ru.itmo.wp.model.domain.User;

import java.util.List;

public interface UserRepository {
    User find(long id);
    User findByLogin(String login);
    List<User> findAll();
    void save(User user, String passwordSha);
    User findByEmail(String email);
    User findByLoginOrEmailAndPasswordSha(String email, String passwordSha);
    long findCount();
}
