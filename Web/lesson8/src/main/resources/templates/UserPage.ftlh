<#import "commons.ftlh" as c>

<@c.page>
    <#if userById??>
        <div class="id">id: ${userById.id}</div>
        <div class="login">login: ${userById.login}</div>
        <div class="creationTime">creationTime: ${userById.creationTime}</div>
        <#else>
        No such user
    </#if>
</@c.page>
