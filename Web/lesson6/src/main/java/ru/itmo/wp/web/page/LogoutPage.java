package ru.itmo.wp.web.page;

import ru.itmo.wp.model.domain.Event;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.service.EventService;
import ru.itmo.wp.web.exception.RedirectException;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

@SuppressWarnings({"unused", "RedundantSuppression"})
public class LogoutPage extends Page {
    EventService eventService = new EventService();

    private void action(HttpServletRequest request, Map<String, Object> view) {
        User user = getUser();
        if (user != null) {
            request.getSession().removeAttribute("user");

            setMessage("Good bye. Hope to see you soon!");
            Event event = new Event();
            event.setType(Event.Type.LOGOUT);
            event.setUserId(user.getId());
            eventService.save(event);
        }
        throw new RedirectException("/index");
    }
}
