<template>
  <div>
    <Article :post="post" :commentsCount="Object.values(comments).length"
             :userLogin="author.login"/>
    <div class="comments" v-if="Object.values(comments).length > 0">Comments:</div>
    <div class="comments" v-else>No comments</div>
    <div class="comment" v-for="comment in comments" :key="comment.id">
      <div class="user-login">{{ Object.values(usersCommented).filter(u => u.id === comment.userId)[0].login }}</div>
      <div class="text"> {{ comment.text }}</div>
    </div>
  </div>
</template>

<script>
import Article from "../article/Article";

export default {
  name: "Post",
  props: ["post", "comments", "usersCommented", "author"],
  components: {Article}
}
</script>

<style scoped>

</style>