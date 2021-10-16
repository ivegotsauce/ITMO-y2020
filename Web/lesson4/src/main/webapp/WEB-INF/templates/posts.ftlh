<#-- @ftlvariable name="user_id" type="long" -->
<#import "./common.ftlh" as c>

<@c.page>
    <#if user_id??>
        <#assign p = c.findBy(posts, "userId", user_id)!/>
    </#if>
    <#if !user_id??>
        <@c.postList true/>
        <#elseif p?has_content>
            <@c.postList short=true id="" user_id=p.userId/>
        <#else>
            No such posts
    </#if>
</@c.page>