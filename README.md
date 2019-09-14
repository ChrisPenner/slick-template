# Slick Template

Get up and running with slick in no time! 

Here's all you need to do to have your own site:

1. Click the green `Use this template` button at the top of this page (next to "clone")
2. Clone your new site repo
3. Edit your `siteMeta` inside `Main.hs`
4. Add some awesome blog posts in `site/posts/` by copying the sample post there
4. run `stack build`; `stack exec build-site`
5. Serve your `docs` directory by enabling Github Pages in your repository's settings
6. ...?
7. Profit!

---

# Notes

* Everything is just html, css, and javascript! Edit things to your heart's content!
* Templates are in `site/tempates`; they use the [Mustache](https://mustache.github.io/) template language.
* You'll need to delete your `.shake` directory when you edit `Main.hs` to avoid stale build caches.
* Slick is good at **updating** and **creating** files, but it doesn't delete stale files. When in doubt you can delete your whole output directory.
