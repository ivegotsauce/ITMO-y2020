<#-- @ftlvariable name="users" type="ru.itmo.wp.model.domain.User[]" -->
<#-- @ftlvariable name="user" type="ru.itmo.wp.model.domain.User" -->
<#import "commons.ftlh" as c>

<@c.page>
    <div class="users datatable">
        <div class="caption">User</div>
        <table>
            <thead>
            <tr>
                <th>Id</th>
                <th>Login</th>
                <#if user?? && user.admin>
                    <th>Admin</th>
                </#if>
            </tr>
            </thead>
            <tbody>
            <template>
                <tr>
                    <td class="user-id"></td>
                    <td class="user_login"></td>
                    <#if user?? && user.admin>
                        <td class="admin">
                            <a class="isAdmin"></a>
                            <a class="setAdmin" href=""></a>
                        </td>
                    </#if>
                </tr>
            </template>
            <tr class="noData">
                <td colspan="3">
                    No data
                </td>
            </tr>
            </tbody>
        </table>
    </div>

    <script>
        $(function () {
            const $noData = $(".users .noData");
            $noData.find("td").text("Loading...");
            ajax(
                null,
                {
                    action: "findAll"
                },
                function(response) {
                    response["users"].forEach(function(user) {
                        $noData.hide();
                        const $tr = $($(".users template").prop("content")).clone();
                        $tr.find(".user-id").text(user["id"]);
                        $tr.find(".user_login").text(user["login"]);
                        $tr.find("tr").attr("data-userId", user["id"]);
                        $tr.find(".admin .isAdmin").text(user["admin"]);
                        $tr.find(".admin .setAdmin").text(user["admin"] ? "disable" : "enable");
                        $(".users tbody").append($tr);
                    });
                    $(".users .admin .setAdmin").click(function () {
                        let $value = $(this);
                        ajax(
                            null,
                            {
                                action: "setAdmin",
                                id: ($(this).parent().parent().find(".user-id").text()),
                                value: $value.text()
                            },
                            function (response) {
                                $value.parent().find(".isAdmin").text(response["admin"]);
                                $value.text(response["admin"] ? "disable" : "enable");
                            }
                        )
                        return false;
                    })
                    $(".users tr").click(function() {
                        ajax(
                            null,
                            {
                                action: "findUser",
                                userId: $(this).attr("data-userId")
                            },
                            function (response) {
                                notify(response["user"].creationTime);
                            }
                        )
                    });
                }
            );
        })
    </script>
</@c.page>
