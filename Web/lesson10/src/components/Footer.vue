<template>
    <footer>
        <a href="/">Codeforces</a> &copy; 2021 by Mike Mirzayanov users: {{ usersSize }} posts: {{ postsSize }}
    </footer>
</template>

<script>
export default {
    name: "Footer",
    props: ["usersSize", "postsSize"]
}
</script>

<style scoped>

</style>