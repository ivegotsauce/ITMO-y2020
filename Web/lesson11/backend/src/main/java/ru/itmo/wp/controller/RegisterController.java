package ru.itmo.wp.controller;

import org.springframework.validation.BindingResult;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.*;
import ru.itmo.wp.domain.User;
import ru.itmo.wp.exception.ValidationException;
import ru.itmo.wp.form.RegisterForm;
import ru.itmo.wp.form.UserCredentials;
import ru.itmo.wp.form.validator.RegisterFormValidator;
import ru.itmo.wp.service.JwtService;
import ru.itmo.wp.service.UserService;

import javax.validation.Valid;

@RestController
@RequestMapping("/api/1")
public class RegisterController {
    private final JwtService jwtService;
    private final UserService userService;
    private final RegisterFormValidator registerFormValidator;

    public RegisterController(JwtService jwtService, UserService userService, RegisterFormValidator registerFormValidator) {
        this.jwtService = jwtService;
        this.userService = userService;
        this.registerFormValidator = registerFormValidator;
    }

    @InitBinder("registerForm")
    public void initBinder(WebDataBinder binder) {
        binder.addValidators(registerFormValidator);
    }

    @PostMapping("users/register")
    public String create(@RequestBody @Valid RegisterForm registerForm, BindingResult bindingResult) {
        if (bindingResult.hasErrors()) {
            throw new ValidationException(bindingResult);
        }
        User user = userService.register(registerForm);
        return jwtService.create(user);
    }

}
