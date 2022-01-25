package ru.itmo.wp.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import ru.itmo.wp.domain.Post;
import ru.itmo.wp.domain.Role;
import ru.itmo.wp.domain.Tag;
import ru.itmo.wp.form.PostAndTags;
import ru.itmo.wp.form.validator.PostAndTagsValidator;
import ru.itmo.wp.security.AnyRole;
import ru.itmo.wp.service.TagService;
import ru.itmo.wp.service.UserService;

import javax.servlet.http.HttpSession;
import javax.validation.Valid;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

@Controller
public class WritePostPage extends Page {
    private final UserService userService;
    private final TagService tagService;
    private final PostAndTagsValidator postAndTagsValidator;

    public WritePostPage(UserService userService, TagService tagService, PostAndTagsValidator postAndTagsValidator) {
        this.userService = userService;
        this.tagService = tagService;
        this.postAndTagsValidator = postAndTagsValidator;
    }

    @InitBinder("post")
    public void initBinder(WebDataBinder binder) {
        binder.addValidators(postAndTagsValidator);
    }


    @AnyRole({Role.Name.WRITER, Role.Name.ADMIN})
    @GetMapping("/writePost")
    public String writePostGet(Model model) {
        model.addAttribute("post", new Post());
        return "WritePostPage";
    }

    @AnyRole({Role.Name.WRITER, Role.Name.ADMIN})
    @PostMapping("/writePost")
    public String writePostPost(@Valid @ModelAttribute("post") PostAndTags postAndTags,
                                BindingResult bindingResult,
                                HttpSession httpSession) {
        if (bindingResult.hasErrors()) {
            return "WritePostPage";
        }
        Set<Tag> tags = new HashSet<>();
        Set<String> tagStrings = Arrays.stream(postAndTags.getTags().split("\\s+")).collect(Collectors.toSet());
        for (String tagName : tagStrings) {
            Tag tag = new Tag();
            tag.setName(tagName);
            tags.add(tagService.save(tag));
        }
        userService.writePost(getUser(httpSession), postAndTags, tags);
        putMessage(httpSession, "You published new post");

        return "redirect:/posts";
    }
}
