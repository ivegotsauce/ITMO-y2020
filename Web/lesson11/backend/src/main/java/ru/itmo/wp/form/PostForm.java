package ru.itmo.wp.form;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

public class PostForm {
    @NotEmpty
    @NotNull
    @NotBlank
    @Size(min = 1, max = 100)
    private String title;

    @NotEmpty
    @NotNull
    @NotBlank
    @Size(min = 1, max = 10000)
    private String text;

    @NotNull
    private String userId;

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    public String getUserId() {
        return userId;
    }

    public void setUserId(String userId) {
        this.userId = userId;
    }
}
