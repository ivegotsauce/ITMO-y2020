<template>
  <article>
    <a href="#" class="title" @click.prevent="onPost(post)">{{ post.title }}</a>
    <div class="information">By {{ userLogin }}, 2 days ago</div>
    <div class="body">{{ post.text }}</div>
    <ul class="attachment">
      <li>Announcement of <a href="#">Codeforces Round #510 (Div. 1)</a></li>
      <li>Announcement of <a href="#">Codeforces Round #510 (Div. 2)</a></li>
    </ul>
    <div class="footer">
      <div class="left">
        <img src="../../assets/img/voteup.png" title="Vote Up" alt="Vote Up"/>
        <span class="positive-score">+173</span>
        <img src="../../assets/img/votedown.png" title="Vote Down" alt="Vote Down"/>
      </div>
      <div class="right">
        <img src="../../assets/img/date_16x16.png" title="Publish Time" alt="Publish Time"/>
        2 days ago
        <img src="../../assets/img/comments_16x16.png" title="Comments" alt="Comments"/>
        <a href="#" @click.prevent="onPost(post)">{{ commentsCount }}</a>
      </div>
    </div>
  </article>
</template>

<script>
export default {
  name: "Article",
  props: ["post", "commentsCount", "userLogin"],
  methods: {
    changePage: function (page) {
      this.$root.$emit("onChangePage", page);
    },
    onPost: function ( post ) {
      this.$root.$emit("onPost", post);
      this.changePage("Post");
    }
  }
}
</script>

<style scoped>
section {
  margin-bottom: 1rem;
}
</style>