<#-- @ftlvariable name="articles" type="ru.itmo.wp.model.domain.Article[]" -->
<#import "commons.ftlh" as c>

<@c.page>
    <div class="articles datatable">
        <div class="caption">User</div>
        <table>
            <thead>
            <tr>
                <th>Id</th>
                <th>Title</th>
                <th>Hidden</th>
            </tr>
            </thead>
            <tbody>
            <#list articles as article>
                <tr>
                    <td class="data-id">${article.id}</td>
                    <td>${article.title}</td>
                    <td class="hidden">
                            <#if article.hidden>
                                <input id="show" type="submit" value="Show">
                                <#else>
                                    <input id="reset" type="submit" value="Hide">
                            </#if>
                    </td>
                </tr>
            </#list>
            </tbody>
        </table>
    </div>
    <script>
        $(function () {
            $(".articles tr .hidden input").click(function () {
                let $value = $(this);
                ajax(
                    null,
                    {
                        action: "setHidden",
                        id: ($(this).parent().parent().find(".data-id").text()),
                        value: $value.val()
                    },
                    function (response) {
                        if (response["hidden"] === true) {
                            $value.val("Show");
                        } else {
                            $value.val("Hide");
                        }
                    }
                )
                return false;
            });
        })
    </script>
</@c.page>