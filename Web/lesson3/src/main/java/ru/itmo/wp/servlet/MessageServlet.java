package ru.itmo.wp.servlet;

import com.google.gson.Gson;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;

public class MessageServlet extends HttpServlet {
    private static class Message {
        String user;
        String text;

        private Message(String user, String text) {
            this.user = user;
            this.text = text;
        }
    }

    ArrayList<Message> messages = new ArrayList<>();

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        String uri = request.getRequestURI();
        response.setContentType("application/json");
        String json;
        HttpSession session = request.getSession();
        switch (uri) {
            case "/message/auth":
                String user = request.getParameter("user");
                if (user != null && user.trim().length() > 0) {
                    session.setAttribute("user", user);
                }
                user = (String) session.getAttribute("user");
                if (user == null) {
                    user = "";
                }
                json = new Gson().toJson(user);
                response.getOutputStream().write(json.getBytes(StandardCharsets.UTF_8));
                break;
            case "/message/findAll":
                json = new Gson().toJson(messages);
                if (session.getAttribute("user") != null) {
                    response.getOutputStream().write(json.getBytes(StandardCharsets.UTF_8));
                }
                break;
            case "/message/add":
                String text = request.getParameter("text");
                if (text.trim().length() > 0) {
                    messages.add(new Message((String) session.getAttribute("user"), text));
                }
                break;
        }
    }
}
