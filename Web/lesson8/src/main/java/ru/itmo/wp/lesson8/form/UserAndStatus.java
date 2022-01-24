package ru.itmo.wp.lesson8.form;



import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;

public class UserAndStatus {
    @NotNull
    @NotEmpty
    private String userId;
    @NotNull
    @NotEmpty
    @Pattern(regexp = "enable|disable", message = "Invalid set status request")
    private String status;

    public String getUserId() {
        return userId;
    }

    public void setUserId(String userId) {
        this.userId = userId;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }
}
