<#-- @ftlvariable name="handle" type="java.lang.String" -->
<#-- @ftlvariable name="user_id" type="long" -->
<#import "./common.ftlh" as c>

<@c.page>
    <#if handle??>
        <#assign u = c.findBy(users, "handle", handle)!/>
    <#elseif user_id??>
        <#assign u = c.findBy(users, "id", user_id)!/>
    </#if>

    <#if u?has_content>
        <div class="user-box">
            <img src="/img/no-title.jpg" title="${u.handle}" alt="${u.handle}">
            <a href="" class="name">
                ${u.name}
            </a>
            <ul class="information">
                <li>id: ${u.id}</li>
                <li>handle: <@c.userlink u false/></li>
                <li>name: ${u.name}</li>
                <li>number of posts: <a href="/posts?user_id=${u.id}"> ${c.numberOfPosts(u.id)} </a> </li>
            </ul>
        </div>
        <div class="arrows">
            <#assign pu = c.findPrevUser(u)>

            <#if pu != u>
                <a class="active" href="/user?handle=${pu.handle}">
            <#else>
                <a>
            </#if>
            &#8592</a>
            <#assign nu = c.findNextUser(u)>
            <#if nu != u>
                <a class="active" href="/user?handle=${nu.handle}">
            <#else>
                <a>
            </#if>
            &#8594</a>
        <div>
    <#else>
        No such user
    </#if>
</@c.page>
