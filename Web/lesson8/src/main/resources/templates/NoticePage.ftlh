<#import "commons.ftlh" as c>

<@c.page>
    <div class="notice-form">
        <div class="header">Notice</div>
        <div class="body">
            <form method="post" action="">
                <div class="field">
                    <div class="name">
                        <label for="text">Text</label>
                    </div>
                    <div class="value">
                        <textarea type="text" id="text" name="content"></textarea>
                    </div>
                    <@c.error "noticeForm.content"/>
                </div>
                <div class="error"></div>
                <div class="button-field">
                    <input type="submit" value="Add">
                </div>
            </form>
        </div>
    </div>
</@c.page>
