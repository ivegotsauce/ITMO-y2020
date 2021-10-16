<#-- @ftlvariable name="post_id" type="long" -->
<#import "./common.ftlh" as c>

<@c.page>
    <#if post_id??>
        <#assign p = c.findBy(posts, "id", post_id)!/>
    </#if>

    <#if p?has_content>
            <@c.postList short=false id=p.id/>
        <#else>
            No such post
    </#if>
</@c.page>
