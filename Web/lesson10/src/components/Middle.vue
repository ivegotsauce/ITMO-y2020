<template>
  <div class="middle">
    <Sidebar :posts="viewPosts"/>
    <main>
      <Index v-if="page === 'Index'" :posts="posts" :comments="comments" :usersCommented="usersCommented" :usersPosted="usersPosted"/>
      <Enter v-if="page === 'Enter'"/>
      <WritePost v-if="page === 'WritePost'"/>
      <EditPost v-if="page === 'EditPost'"/>
      <Register v-if="page === 'Register'"/>
      <Users v-if="page === 'Users'" :users="$root.users"/>
      <Post v-if="page === 'Post'" :post="post" :comments="Object.values(comments).filter(c => c.postId === post.id)"
            :usersCommented="Object.values(usersCommented).filter(u => Object.values(comments).filter(c => c.postId === post.id && c.userId === u.id).length > 0)"
            :author="Object.values(usersPosted).find(u => u.id === post.userId)"/>
    </main>
  </div>
</template>

<script>
import Sidebar from "./sidebar/Sidebar";
import Index from "./page/Index";
import Enter from "./page/Enter";
import WritePost from "./page/WritePost";
import EditPost from "./page/EditPost";
import Register from "./page/Register";
import Users from "./page/Users";
import Post from "./page/Post"

export default {
  name: "Middle",
  data: function () {
    return {
      page: "Index",
      post: null
    }
  },
  components: {
    WritePost,
    Enter,
    Index,
    Sidebar,
    EditPost,
    Register,
    Users,
    Post
  },
  props: ["posts", "comments", "usersPosted", "usersCommented"],
  computed: {
    viewPosts: function () {
      return Object.values(this.posts).sort((a, b) => b.id - a.id).slice(0, 2);
    }
  }, beforeCreate() {
    this.$root.$on("onChangePage", (page) => this.page = page);
    this.$root.$on("onPost", (post) => this.post = post);
  }
}
</script>

<style scoped>

</style>
