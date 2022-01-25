<template>
  <div>
    <Article :post="post"/>
    <div class="comments" v-if="post.comments.length > 0">Comments:</div>
    <div class="comments" v-else>No comments</div>
    <div class="comment" v-for="comment in post.comments" :key="comment.id">
      <div class="user-login">{{ comment.user.login }}</div>
      <div class="text"> {{ comment.text }}</div>
    </div>
  </div>
</template>

<script>
import Article from "../article/Article";

export default {
  name: "Post",
  props: ["post"],
  components: {Article}
}
</script>

<style scoped>

</style>