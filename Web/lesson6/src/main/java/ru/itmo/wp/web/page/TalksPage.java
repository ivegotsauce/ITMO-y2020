package ru.itmo.wp.web.page;

import ru.itmo.wp.model.domain.Talk;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.ValidationException;
import ru.itmo.wp.model.service.TalkService;
import ru.itmo.wp.model.service.UserService;
import ru.itmo.wp.web.exception.RedirectException;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

@SuppressWarnings({"unused", "RedundantSuppression"})
public class TalksPage extends Page {
    UserService userService = new UserService();
    TalkService talkService = new TalkService();


    protected void before(HttpServletRequest request, Map<String, Object> view) {
        super.before(request, view);
        if (getUser() == null) {
            setMessage("You should be logged in to join talks!");
            throw new RedirectException("/index");
        }
    }

    private void send(HttpServletRequest request, Map<String, Object> view) throws ValidationException {
        Talk talk = new Talk();

        String targetUser = request.getParameter("target");
        talk.setSourceUserId(getUser().getId());
        talk.setText(request.getParameter("text"));
        view.put("talks", talkService.findByUserId(getUser().getId()));
        view.put("users", userService.findAll());
        talkService.validateMessage(talk, targetUser);
        talkService.sendMessage(talk, targetUser);
    }

    protected void after(HttpServletRequest request, Map<String, Object> view) {
        super.after(request, view);
        view.put("talks", talkService.findByUserId(getUser().getId()));
        view.put("users", userService.findAll());
    }
}
