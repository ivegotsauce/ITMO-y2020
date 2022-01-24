<#-- @ftlvariable name="users" type="ru.itmo.wp.model.domain.User[]" -->
<#-- @ftlvariable name="talks" type="java.util.ArrayList<java.util.ArrayList<Object>>" -->
<#-- @ftlvariable name="error" type="java.lang.String" -->
<#import "commons.ftlh" as c>

<@c.page>
    <div class="body">
        <div class="message-box">
            <form method="post" action="">
                <input type="hidden" name="action" value="send">
                <div class="field">
                    <div class="name">
                        <label for="text">New message</label>
                    </div>
                    <div class="value">
                        <textarea type="text" id="text" name="text" rows="5" cols="50"></textarea>
                    </div>
                </div>
                <div class="field">
                    <div class="select-submit">
                        <div>
                            <label for="users">
                                <select name="target" id="target">
                                    <#list users as user>
                                        <option>${user.login}</option>
                                    </#list>
                                </select>
                            </label>
                        </div>
                        <div class="button-field">
                            <input type="submit" value="Send Message">
                        </div>
                    </div>
                </div>
                <div class="error">
                <#if error??>
                    <div class="error">
                        ${error!}
                    </div>
                </#if>
                </div>
            </form>
        </div>
        <div class="datatable">
            <div class="caption">Talks</div>
            <table>
                <thead>
                <tr>
                    <th>Id</th>
                    <th>Source</th>
                    <th>Target</th>
                    <th>Message</th>
                    <th>Date</th>
                </tr>
                </thead>
                <tbody>
                <#list talks as talk>
                    <tr>
                        <td>${talk[0].id}</td>
                        <td>${talk[1]}</td>
                        <td>${talk[2]}</td>
                        <td>${talk[0].text}</td>
                        <td>${talk[0].creationTime}</td>
                    </tr>
                </#list>
                </tbody>
            </table>
        </div>
    </div>
</@c.page>
