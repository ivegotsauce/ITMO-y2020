<template>
  <div>
    <Article v-for="post in posts" :post="post" :key="post.id"/>
  </div>
</template>

<script>
import Article from "../article/Article";

export default {
  name: "Index",
  props: ["posts", "postingUsers"],
  components: {Article},
}
</script>

<style scoped>

</style>
