package ru.itmo.wp.model.service;

import ru.itmo.wp.model.domain.Talk;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.ValidationException;
import ru.itmo.wp.model.repository.TalkRepository;
import ru.itmo.wp.model.repository.UserRepository;
import ru.itmo.wp.model.repository.impl.TalkRepositoryImpl;
import ru.itmo.wp.model.repository.impl.UserRepositoryImpl;
import com.google.common.base.Strings;

import java.util.List;

public class TalkService {
    private final TalkRepository talkRepository = new TalkRepositoryImpl();
    private final UserRepository userRepository = new UserRepositoryImpl();

    public void validateMessage(Talk talk, String targetUser) throws ValidationException {
        if (targetUser == null) {
         throw new ValidationException("Target user required");
        }
        if (userRepository.findByLogin(targetUser) == null) {
            throw new ValidationException("No such user");
        }
        if (Strings.isNullOrEmpty(talk.getText())) {
            throw new ValidationException("Text is required");
        }
        if (talk.getText().chars().allMatch(Character::isWhitespace)) {
            throw new ValidationException("Message can't be blank");
        }
    }

    public void sendMessage(Talk talk, String targetUserLogin) {
        User targetUser = userRepository.findByLogin(targetUserLogin);
        talk.setTargetUserId(targetUser.getId());
        talkRepository.save(talk);
    }

    public List<List<Object>> findByUserId(long id) {
        return talkRepository.findByUserId(id);
    }
}
