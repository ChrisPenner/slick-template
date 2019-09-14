# Slick Template

For an example of what the site will look like, check out [my blog](https://chrispenner.ca)!

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

If you want a quick tool for serving your file system during development I recommend using `serve`:

```shell
$ npm install -g serve
serve dist
```

Then navigate to the port which is serving (usually http://localhost:3000 or http://localhost:5000 )


---

# Tips

* Everything is just html, css, and javascript! Edit things to your heart's content!
* Templates are in `site/tempates`; they use the [Mustache](https://mustache.github.io/) template language.
* You'll need to delete your `.shake` directory when you edit `Main.hs` to avoid stale build caches.
* Slick is good at **updating** and **creating** files, but it doesn't delete stale files. When in doubt you can delete your whole output directory.

# Caching guide

Shake takes care of most of the tricky parts, but there're still a few things you need to know.

Cache-busting in Slick works using [`Development.Shake.Forward`](https://hackage.haskell.org/package/shake/docs/Development-Shake-Forward.html). The idea is that you can wrap actions with [`cacheAction`](https://hackage.haskell.org/package/shake-0.18.3/docs/Development-Shake-Forward.html#v:cacheAction), providing an unique identifier for each time it runs. Shake will track any dependencies which are triggered during the first run of that action and can use them to detect when that particular action must be re-run. Typically you'll want to cache an action for each "thing" you have to load, e.g. when you load a post, or when you build a page. You can also nest these caches if you like.

When using `cacheAction` Shake will automatically serialize and store the results of that action to disk so that on a later build it can simply 'hydrate' that asset without running the command. For this reason, your data models should probably implement `Binary`. Here's an example data model:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import Data.Aeson (ToJSON, FromJSON)
import Development.Shake.Classes (Binary)
import GHC.Generics (Generic)

-- | Data for a blog post
data Post =
    Post { title   :: String
         , author  :: String
         , content :: String
         , url     :: String
         , date    :: String
         , image   :: Maybe String
         }
    deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)
```

If you need to run arbitrary shell commands you can use [`cache`](https://hackage.haskell.org/package/shake-0.18.3/docs/Development-Shake-Forward.html#v:cache); it will do its best to track file use during the run of the command and cache-bust on that; results may vary. It's likely better to use explicit tracking commands like `readFile'` when possible, (or even just use `readFile'` on the files you depend on, then throw away the results. It's equivalent to explicitly depending on the file contents).

Shake has many dependency tracking combinators available; whenever possible you should use the shake variants of these (e.g. `copyFileChanged`, `readFile'`, `writeFile'`, etc.). This will allow shake to detect when and what it needs to rebuild.

Note: You'll likely need to delete `.shake` in your working directory after editing your `Main.hs` file as shake can get confused if rules change without it noticing.
