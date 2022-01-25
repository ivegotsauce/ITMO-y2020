package ru.itmo.wp.servlet;

import ru.itmo.wp.util.ImageUtils;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpFilter;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Base64;
import java.util.Random;

public class CaptchaFilter extends HttpFilter {
    @Override
    public void doFilter(HttpServletRequest request, HttpServletResponse response, FilterChain chain)
            throws IOException, ServletException {
        HttpSession session = request.getSession();
        String method = request.getMethod();
        if (method.equals("GET") && session.getAttribute("PassedCaptcha") == null) {
            session.setAttribute("lastRequestURI", request.getRequestURI());
            writeCaptcha(request, response);
        } else if (method.equals("POST") && request.getRequestURI().equals("/captcha")) {
            if (request.getParameter("number").equals(session.getAttribute("answer"))) {
                session.setAttribute("PassedCaptcha", "passed");
                response.sendRedirect((String) session.getAttribute("lastRequestURI"));
            } else {
                writeCaptcha(request, response);
            }
        } else {
            chain.doFilter(request, response);
        }
    }

    private void writeCaptcha(HttpServletRequest request, HttpServletResponse response) throws IOException {
        response.setContentType("text/html");
        String number = Integer.toString(new Random().nextInt(900) + 100);
        request.getSession().setAttribute("answer", number);
        OutputStream stream = response.getOutputStream();
        stream.write("<div><img src=\"data:image/jpeg;base64, ".getBytes());
        stream.write(Base64.getEncoder().encode(ImageUtils.toPng(number)));
        stream.write(("\"></div>\n" +
                "<div>\n" +
                "    <form action=\"captcha\" method=\"post\">\n" +
                "        <label>Enter the number above: </label>" +
                "        <input name=\"number\">\n" +
                "    </form>\n" +
                "</div>").getBytes());
        stream.flush();
    }
}