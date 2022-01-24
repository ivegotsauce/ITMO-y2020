<#import "commons.ftlh" as c>

<@c.page>
    <div class="articles">
        <div class="noData">
            No Data
        </div>

        <template>
            <div class="article">
                <div class="title"></div>
                <div class="information">
                    <a class="login">loading...</a>
                </div>
                <div class="body">
                    <p></p>
                </div>
                <ul class="attachment">
                    <li>Announcement of <a href="#">Codeforces Round #510 (Div. 1)</a></li>
                    <li>Announcement of <a href="#">Codeforces Round #510 (Div. 2)</a></li>
                </ul>
                <div class="footer">
                    <div class="left">
                        <img src="img/voteup.png" title="Vote Up" alt="Vote Up"/>
                        <span class="positive-score">+173</span>
                        <img src="img/votedown.png" title="Vote Down" alt="Vote Down"/>
                    </div>
                    <div class="right">
                        <img src="img/date_16x16.png" title="Publish Time" alt="Publish Time"/>
                        <img src="img/comments_16x16.png" title="Comments" alt="Comments"/>
                        <a href="#">68</a>
                    </div>
                </div>
            </div>
        </template>
    </div>
    <script>
        $(function () {
            const $noData = $(".articles .noData");
            $noData.text("Loading...");
            ajax(null,
                {
                    action: "findAll"
                },
                function (response) {
                response["articles"].forEach(function(article) {
                    $noData.hide();
                    const $article = $($(".articles template").prop("content")).clone();
                    $article.find(".title").text(article["title"]);
                    $article.find(".body p").text(article["text"]);
                    $article.find(".information").contents()[0].replaceWith("By ");
                    let $value = $article.find(".information a");
                    ajax(
                        null,
                        {
                            action: "findLoginById",
                            id: article["userId"]
                        },
                        function (response) {
                            $value.text(response["login"]);
                        }
                    )
                    $article.find(".information").contents()[2].replaceWith(", " + article["creationTime"]);
                    $article.find(".right").contents()[2].replaceWith(" " + article["creationTime"]);
                    $(".articles").append($article);
                });
            });
        })
    </script>
</@c.page>
