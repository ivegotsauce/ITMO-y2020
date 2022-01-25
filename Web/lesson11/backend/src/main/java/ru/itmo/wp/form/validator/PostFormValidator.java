package ru.itmo.wp.form.validator;

import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;
import ru.itmo.wp.form.PostForm;
import ru.itmo.wp.form.RegisterForm;
import ru.itmo.wp.service.UserService;


@Component
public class PostFormValidator implements Validator {
    private final UserService userService;

    public PostFormValidator(UserService userService) {
        this.userService = userService;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return PostForm.class.equals(clazz);
    }

    @Override
    public void validate(Object target, Errors errors) {
        if (!errors.hasErrors()) {
            PostForm postForm = (PostForm) target;
            try {
                long id = Long.parseLong(postForm.getUserId());
                if (userService.findById(id) == null) {
                    errors.reject("user-no-such", "No such user.");
                }
            } catch (NumberFormatException ignored) {
                errors.reject("user-invalid", "Invalid user.");
            }
        }
    }
}
