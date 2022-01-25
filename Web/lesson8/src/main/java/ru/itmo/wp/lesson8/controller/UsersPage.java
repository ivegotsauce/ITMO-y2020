package ru.itmo.wp.lesson8.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import ru.itmo.wp.lesson8.form.UserAndStatus;
import ru.itmo.wp.lesson8.form.validator.UserAndStatusValidator;
import ru.itmo.wp.lesson8.service.UserService;

import javax.servlet.http.HttpSession;
import javax.validation.Valid;

@Controller
public class UsersPage extends Page {
    private final UserService userService;
    private final UserAndStatusValidator userAndStatusValidator;

    public UsersPage(UserService userService, UserAndStatusValidator userAndStatusValidator) {
        this.userService = userService;
        this.userAndStatusValidator = userAndStatusValidator;
    }

    @InitBinder
    public void initBinder(WebDataBinder binder) {
        binder.addValidators(userAndStatusValidator);
    }

    @GetMapping("/users/all")
    public String users(Model model) {
        model.addAttribute("users", userService.findAll());
        model.addAttribute("statusForm", new UserAndStatus());
        return "UsersPage";
    }

    @PostMapping("/users/all")
    public String users(@Valid @ModelAttribute("statusForm") UserAndStatus statusForm,
                        BindingResult bindingResult,
                        HttpSession httpSession, Model model) {
        if (getUser(httpSession) == null) {
            setMessage(httpSession, "You should be logged in");
            return "redirect:/users/all";
        }
        if (bindingResult.hasErrors()) {
            System.out.println(bindingResult.getAllErrors());
            model.addAttribute("users", userService.findAll());
            return "UsersPage";
        }

        userService.updateStatus(statusForm.getUserId(), statusForm.getStatus());
        return "redirect:/users/all";
    }
}
