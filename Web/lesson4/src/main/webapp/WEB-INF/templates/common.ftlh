<#macro page>
    <!DOCTYPE html>
    <html lang="ru">
    <head>
        <meta charset="UTF-8">
        <title>Codeforces</title>
        <link rel="stylesheet" href="/css/normalize.css">
        <link rel="stylesheet" href="/css/style.css">
        <link rel="icon" href="/favicon.ico">
    </head>
    <body>
    <@header/>
    <div class="middle">
        <@sidebar/>
        <main>
            <#nested/>
        </main>
    </div>
    <@footer/>
    </body>
    </html>
</#macro>

<#macro header>
    <header>
        <a href="#" class="logo"><img alt="Codeforces" title="Codeforces"
                                      src="/img/codeforces-logo-with-telegram.png"></a>
        <div class="languages">
            <a href="#"><img alt="In English" title="In English"
                             src="/img/gb.png"></a>
            <a href="#"><img alt="In Russian" title="In Russian"
                             src="/img/ru.png"></a>
        </div>
        <div class="enter-or-register-box">
            <#if user??>
                <@userlink user true/>
                |
                <a href="/logout">Logout</a>
            <#else>
                <a href="/enter">Enter</a>
                |
                <a href="/register">Register</a>
            </#if>
        </div>
        <nav>
            <#assign items=[
            {"href": "/index", "name": "Home"},
            {"href": "/misc/help", "name": "Help"},
            {"href": "/contests", "name": "Contests"},
            {"href": "/users", "name": "Users"}
            ]/>
            <ul>
                <#list items as item>
                    <li
                            <#if uri == item.href>
                                class="current"
                            </#if>
                    >
                        <a href="${item.href}">${item.name}</a>
                    </li>
                </#list>
            </ul>
        </nav>
    </header>
</#macro>

<#macro sidebar>
    <aside>
        <#list posts as p>
            <@sidebarPost p/>
        </#list>
    </aside>
</#macro>

<#macro sidebarPost post>
<#-- @ftlvariable name="p" type="ru.itmo.web.lesson4.model.Post" -->
    <section>
        <div class="header">Post #${post.id}</div>
        <div class="body"><@postText post true/>
        </div>
        <div class="footer">
            <a href="/post?post_id=${post.id}">View all</a>
        </div>
    </section>
</#macro>

<#macro footer>
    <footer>
        2010-2021 &copy; by Mike Mirzayanov
    </footer>
</#macro>

<#macro userlink user nameOnly>
<#-- @ftlvariable name="user" type="ru.itmo.web.lesson4.model.User" -->
    <#if nameOnly>
        <a href="/user?handle=${user.handle}">${user.name}</a>
    <#else>
        <a class="user-color user-${user.color?string?lower_case}"
           href="/user?handle=${user.handle}">${user.handle}</a>
    </#if>
</#macro>

<#macro postText post short>
<#-- @ftlvariable name="post" type="ru.itmo.web.lesson4.model.Post" -->
    <#if post.text?length gt 250 && short>
        ${post.text?substring(0, 250)}&hellip;
    <#else>
        ${post.text}
    </#if>
</#macro>

<#macro postList short id="" user_id="">
    <ul class="posts">
        <#list posts as p>
            <#if (!id?has_content && !user_id?has_content) || (!id?has_content && user_id == p.userId) ||
            (id?is_number && id == p.id && !user_id?has_content)>
                <li class="post">
                    <article>
                        <div class="title">${p.title}</div>
                        <div class="information">By <@userlink findBy(users, "id", p.userId) false/>,
                            2 days ago, translation
                        </div>
                        <div class="body">
                            <p>
                                <@postText p short/>
                            </p>
                        </div>
                        <ul class="attachment">
                            <li>Attachment <a href="#">#1</a></li>
                            <li>Attachment <a href="#">#2</a></li>
                        </ul>
                        <div class="footer">
                            <div class="left">
                                <img src="/img/voteup.png" title="Vote Up" alt="Vote Up"/>
                                <span class="positive-score">+173</span>
                                <img src="/img/votedown.png" title="Vote Down" alt="Vote Down"/>
                            </div>
                            <div class="right">
                                <img src="/img/date_16x16.png" title="Publish Time" alt="Publish Time"/>
                                2 days ago
                                <img src="/img/comments_16x16.png" title="Comments" alt="Comments"/>
                                <a href="#">68</a>
                            </div>
                        </div>
                    </article>
                </li>
            </#if>
        </#list>
    </ul>
</#macro>

<#function findBy items key value>
    <#list items as item>
        <#if item[key] == value>
            <#return item/>
        </#if>
    </#list>
</#function>

<#-- @ftlvariable name="users" type="java.util.List<ru.itmo.web.lesson4.model.User>" -->
<#function findPrevUser user>
    <#assign p = users[0]>
    <#list users as u>
        <#if user == u>
            <#return p>
        </#if>
        <#assign p = u>
    </#list>
</#function>

<#function findNextUser user>
    <#assign i = 0>
    <#list users as u>
        <#if (i > 0) && users[i - 1] == user>
            <#return u>
        </#if>
        <#assign i = i + 1>
    </#list>
    <#return user>
</#function>

<#function numberOfPosts user_id>
    <#assign i = 0>
    <#list posts as p>
        <#if p["userId"] == user_id>
            <#assign i = i + 1/>
        </#if>
    </#list>
    <#return i/>
</#function>
