package ru.itmo.wp.form.validator;

import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import ru.itmo.wp.form.RegisterForm;
import ru.itmo.wp.form.UserCredentials;
import org.springframework.validation.Validator;
import ru.itmo.wp.service.UserService;

@Component
public class RegisterFormValidator implements Validator {
    private final UserService userService;

    public RegisterFormValidator(UserService userService) {
        this.userService = userService;
    }

    public boolean supports(Class<?> clazz) {
        return RegisterForm.class.equals(clazz);
    }

    public void validate(Object target, Errors errors) {
        if (!errors.hasErrors()) {
            RegisterForm registerForm = (RegisterForm) target;
            if (userService.findByLogin(registerForm.getLogin()) != null) {
                errors.reject("login-already-in-use", "Login is already in use.");
            }
        }
    }
}
