---
title: 24 Days of Hackage: web-routes-boomerang
---

As you start building larger web applications, it quickly becomes important how
you deal with the site's routing. Managing simple strings rapidly
becomes limiting and a maintenance nightmare; we're so used to type safety,
it's easy to forget how cumbersome it is to pass strings around. Furthermore, we
need to ensure that the routes that we accept *in* are the same links that were
*placing* on web pages - there should always be a one-to-one correspondence. What
we'd really like is type-safe routing with minimal fuss, and that's what today's
post is about.

Jeremy Shaw's [`web-routes`](http://hackage.haskell.org/package/web-routes)
library abstracts the pattern of routing, but today we'll focus specifically on
[`web-routes-boomerang`](http://hackage.haskell.org/package/web-routes-boomerang),
which is a library for building type safe routing with minimal fuss.

`web-routes-boomerang` builds on `web-routes` by using
[`boomerang`](http://hackage.haskell.org/package/boomerang), which is a
*reversible parsing library*. Usually, when we work with parsers like `parsec`
the operation moves in one direction - we can parse text into Haskell types, but
we don't get a corresponding pretty-printer. `boomerang` is different here. By
creating a parser, we also create a pretty-printer - the same definition now
works in both directions. Furthermore, the output of the pretty-printer can be
fed back into the parser, so we have a true isomorphism between text and Haskell
values. This is ideal when we work with web sites, as we can parse requests
into Haskell types representing paths on the website, and then later encode
these paths back into URLs. This pushes string handling to the boundaries of
application, and this isolation gives us less scope for errors.

To start using `web-routes-boomerang`, we begin by defining our site map. We'll
use the canonical web programming example of a blog.

```Haskell
type PostId = Integer

data Sitemap
  = Index
  | Post PostId
  | Tagged [Text]
```

Our blog is simple - we have a landing page, a page for an individual blog post,
and a route that shows all posts that intersect with a set of tags. Ideally, we'd
like to have routes like the following:

* `/` -- corresponds to `Index`
* `/post/5398` -- corresponds to `Post 5398`
* `/tags/haskell+frp` -- corresponds to `Tagged ["haskell", "frp"]`

To get going with `boomerang`, we need to use a minimal amount of Template
Haskell.

```
makeBoomerangs ''Sitemap
```

This brings into scope individual *boomerangs* for each constructor -
specifically, `rIndex`, `rPost` and `rTagged`. We can now build the router for
our website. To do this, we combine distinct routes of our site using the
`Monoid` instance, and combine parts of the route using the `Category`
instance. Let's take it slow, and begin with the index page:

```haskell
siteRouter = mconcat
  [ rIndex
```

The `Index` path parses the empty string, and is printed as the empty
string. The next route needs to parse an integer for the `PostId`. To do so,
we'll begin by parsing the string `post`, then expect a `/`, and then parse an
integer:

```haskell
  , rPost . "post" </> integer
```

As you can see, `boomarang`-based site routes have a really nice DSL that
concisely captures the sites routes. Finally, we need to parse the tags route.
This one begins as before, but we'll use the `rListSep` combinator to parse tags
separated by `+`:

```haskell
  , rTagged . "tags" </> (satisfyStr (not . Text.null) `rListSep` "+")
  ]
```

Lovely stuff! All that remains now is to put it all together into something we
can use. For this, we can use the `boomerangSiteRouteT` helper, which takes a
handler function and a router, and gives back a `Site`. However, first of all we
need a handler function.

Our handler will be given a value of type `Sitemap`, which we can pattern match
on to learn the page a user requested. Then, we produce a value under the
`RouteT` monad - a monad which gives us access to the `Sitemap` in order to
format links on a page. Usually, you would layer this on top of a monad provided
by a web server, but today we'll just use `IO` and demonstrate things in the
console. For brevity, I'll demonstrate just one route here, but you can see more
routes in [today's associated code](http://github.com/ocharles/blog).

```haskell
handler :: Sitemap -> RouteT Sitemap IO ()
handler route = case route of
  Index -> do
    posts <- liftIO getPosts
    liftIO $ putStrLn "Posts:"
    forM_ posts $ \post -> do
      postUrl <- showURL (Post (postId post))
      liftIO $ putStrLn $
        Text.unpack (postTitle post) ++ " - " ++ Text.unpack postUrl
```

Finally, we combine this into a `Site`:

```haskell
site :: Site Sitemap (IO ())
site = boomerangSiteRouteT handler siteRouter
```

We can now prod this in GHCI. For example, if we request `/`, which is an empty
list of path parts, we're presented with an index listing:

```
> either error id $ runSite "" site []
Posts:
24 Days of Hackage - /post/42
10 Reasons Why P = NP - /post/91
```

If we received a request for `/post/91`, we can split this apart into the
request for `[ "post", "91" ]`, which shows details about post #42:

```
> either error id $ runSite "" site [ "post", "42" ]
You are reading "24 Days of Hackage"
```

And just to wrap it all up, if you request a route that can't be parsed,
`runSite` will return `Left String` to indicate parsing fails:

```
> either error id $ runSite "" site [ "admin", "h4x" ]
*** Exception: parse error at path segment 1, character 0: unexpected "admin";
               expecting "post" or "tags" while parsing ["admin","h4x"]
```

`web-routes-boomerang` is a fantastic library when it comes to web
programming. It doesn't require a lot more investment to get some tremendous
results, and it easily sets strong foundations for building much more complex
web applications in the future. Not only that, it shows off some really cool
techniques in Haskell programming - which makes it all the more fun to work
with!
