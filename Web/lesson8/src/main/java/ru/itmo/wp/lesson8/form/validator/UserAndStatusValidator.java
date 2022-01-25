package ru.itmo.wp.lesson8.form.validator;

import org.springframework.stereotype.Component;

import org.springframework.validation.Errors;
import org.springframework.validation.Validator;
import ru.itmo.wp.lesson8.domain.User;
import ru.itmo.wp.lesson8.form.UserAndStatus;
import ru.itmo.wp.lesson8.service.UserService;

@Component
public class UserAndStatusValidator implements Validator {
    private final UserService userService;

    public UserAndStatusValidator(UserService userService) {
        this.userService = userService;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return UserAndStatus.class.equals(clazz) || User.class.equals(clazz);
    }

    @Override
    public void validate(Object target, Errors errors) {
        if (!errors.hasErrors()) {
            UserAndStatus userForm = (UserAndStatus) target;
            String userId = userForm.getUserId().replaceAll(" ", "");
            if (userService.findById(Long.parseLong(userId)) == null) {
                errors.rejectValue("userId", "userId.no-such-user", "No such user");
            }
            try {
                long x = Long.parseLong(userId);
            } catch (NumberFormatException e) {
                errors.rejectValue("userId", "userId.invalid-format", "Invalid user id");
            }
        }
    }
}
