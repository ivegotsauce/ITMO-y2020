<#-- @ftlvariable name="comment" type="ru.itmo.wp.domain.Comment" -->
<#import "commons.ftlh" as c>

<@c.page>
    <#if postById??>
        <@c.post post=postById/>
        <#if user??>
            <div class="comment-form">
                <div class="body">
                    <form method="post" action="">
                        <input type="hidden" name="action" value="writeComment"/>
                        <div class="field">
                            <div class="name">
                                <label for="text">Comment</label>
                            </div>
                            <div class="value">
                                <textarea id="text" name="text">${comment.text!}</textarea>
                            </div>
                            <@c.error "comment.text"/>
                        </div>
                        <div class="button-field">
                            <input type="submit" value="Write">
                        </div>
                    </form>
                </div>
            </div>
        </#if>
        <#list postById.comments as comm>
            <div class="comment">
                <div class="user-login">${comm.user.login}</div>
                <div class="text">${comm.text}</div>
            </div>
        </#list>
    <#else>
        No such post
    </#if>
</@c.page>