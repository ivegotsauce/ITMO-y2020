window.notify = function (message) {
    $.notify(message, {
        position: "right bottom",
        className: "success"
    });
}

window.ajax = function ($error, data, success) {
    $.ajax({
        type: "POST",
        url: "",
        dataType: "json",
        data: data,
        success: function (response) {
            if (success !== null) {
                success(response);
            }
            if (response["error"]) {
                $error.text(response["error"]);
            } else if (response["redirect"]) {
                location.href = response["redirect"];
            }
        }
    });
}