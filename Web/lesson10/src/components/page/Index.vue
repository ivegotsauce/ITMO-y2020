<template>
  <div>
    <Article v-for="post in viewPosts" :post="post" :key="post.id"
             :commentsCount="Object.values(comments).filter(c => c.postId === post.id).length"
             :userLogin="Object.values(usersPosted).filter(u => u.id === post.userId)[0].login"/>
  </div>
</template>

<script>
import Article from "../article/Article";

export default {
  name: "Index",
  props: ["posts", "comments", "usersCommented", "usersPosted"],
  components: {Article},
  computed: {
    viewPosts: function () {
      return Object.values(this.posts).sort((a, b) => b.id - a.id);
    }
  }
}
</script>

<style scoped>

</style>
