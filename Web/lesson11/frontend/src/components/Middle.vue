<template>
    <div class="middle">
        <Sidebar :posts="viewPosts"/>
        <main>
            <Index v-if="page === 'Index'" :posts="indexPosts"
                   :postingUsers= "Object.values(users).filter(u => Object.values(posts).filter(p => p.user.id === u.id).length > 0)"/>
            <Enter v-if="page === 'Enter'"/>
            <WritePost v-if="page === 'WritePost'"/>
            <Register v-if="page === 'Register'"/>
            <Users v-if="page === 'Users'" :users="users"/>
            <Post v-if="page === 'Post'" :post="post"/>
        </main>
    </div>
</template>

<script>
import Sidebar from "./sidebar/Sidebar";
import Index from "./main/Index";
import Enter from "./main/Enter";
import Register from "./main/Register";
import Users from "./page/Users";
import WritePost from "./page/WritePost";
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
        Register,
        Enter,
        Index,
        Sidebar,
        Users,
        WritePost,
        Post
    },
    props: ["posts", "users"],
    computed: {
        viewPosts: function () {
            return Object.values(this.posts).sort((a, b) => b.id - a.id).slice(0, 2);
        },
        indexPosts: function () {
          return Object.values(this.posts).sort((a,b) => (b.creationTime > a.creationTime) ? 1 : ((a.creationTime > b.creationTime) ? -1 : 0))
        }
    }, beforeCreate() {
        this.$root.$on("onChangePage", (page) => this.page = page);
        this.$root.$on("onPost", (post) => this.post = post);
    }
}
</script>

<style scoped>

</style>
