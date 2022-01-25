package ru.itmo.wp.lesson8.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import ru.itmo.wp.lesson8.form.NoticeContent;
import ru.itmo.wp.lesson8.service.NoticeService;

import javax.servlet.http.HttpSession;
import javax.validation.Valid;

@Controller
public class NoticePage extends Page{
    private final NoticeService noticeService;

    public NoticePage(NoticeService noticeService) {
        this.noticeService = noticeService;
    }

    @GetMapping("/notice")
    public String noticeGet(Model model, HttpSession httpSession) {
        if (getUser(httpSession) == null) {
            setMessage(httpSession, "You should be logged in");
            return "redirect:/";
        }
        model.addAttribute( "noticeForm", new NoticeContent());
        return "NoticePage";
    }

    @PostMapping("/notice")
    public String noticePost(@Valid @ModelAttribute("noticeForm") NoticeContent noticeForm,
                             BindingResult bindingResult,
                             HttpSession httpSession) {
        if (getUser(httpSession) == null) {
            setMessage(httpSession, "You should be logged in");
            return "redirect:/";
        }
        if (bindingResult.hasErrors()) {
            return "NoticePage";
        }
        noticeService.save(noticeForm);
        setMessage(httpSession, "Notice successfully added!");

        return "redirect:/";
    }
}
