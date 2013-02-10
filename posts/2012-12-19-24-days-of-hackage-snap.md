---
title: 24 Days of Hackage: snap
---

The [Snap framework](http://snapframework.com) is one of a handful of web
frameworks available for Haskell, and comes complete with a high performance
HTTP server, a rich monad for web programming, composable snaplets, and strong
integration with the Heist templating language. In today's post, we'll have a
look at how simple it is to build applications using Snap.

## `snap-core`

The first library we'll look at is
[`snap-core`](http://hackage.haskell.org/package/snap-core), which provides some
low level tools for building dynamic websites. Let's jump straight in to getting
some results. A simple web handler is simply a function in anything that is an
instance of `MonadSnap`:

```haskell
hello :: Snap ()
hello = writeText "Hello, world!"
```

Hello world doesn't get any simpler than that! Of course, this is only our
handler, the next step is to actually add a *route* to this handler:

```haskell
app :: Snap ()
app = route [("/hello", hello)]
```

This adds a single route to our application, if the user browses `/hello`, then
Snap will use our `hello` handler, and simply write "Hello, world" back to the
browser. Finally, we need to run our application - we'll use Snap's server for
this:

```haskell
main :: IO ()
main = quickHttpServe app
```

That's it - in 6 lines of code, all of which are straightforward Haskell 98,
we have a web app ready to serve requests! `snap-core` provides a little
more, including the ability to parse query/body paramaters, get and set cookies,
perform error handling, and manipulating the request and response for the
current request.

## Snaplets

We haven't seen anything exactly ground breaking so far, and that's because
`snap-core` is designed to be simplistic and unassuming. While it's certainly
possible to build complex applications with Snap, the normal process of
refactoring and abstracting common patterns is certainly workable, the
[`snap`](http://hackage.haskell.org/package/snap) library contains the snaplets
abstraction, which is a very powerful way of building up applications.

Snaplets are small little self-contained pieces of functionality that can be
combined together to build larger applications. Snaplets were introduced in Snap
0.6, and already there are many on Hackage - Snaplets for
[Riak](http://hackage.haskell.org/package/snaplet-riak),
[`postgresql-simple`](http://hackage.haskell.org/package/snaplet-postgresql-simple),
[Recaptcha](http://hackage.haskell.org/package/snaplet-recaptcha), and
more. Let's see how snaplets work:

```haskell
data App = App { _db :: Snaplet Postgres }

makeLenses ''App

initApp :: SnapletInit App App
initApp = makeSnaplet "myapp" "My application" Nothing $
  App <$> nestSnaplet "db" db pgsInit
      <* addRoutes [("/hello/:id", helloDb)]

helloDb :: Handler App App ()
helloDb = do
  Just mUId <- getParam "id"
  userName <- with db $
    listToMaybe <$>
      query "SELECT name FROM users WHERE id = ?" (Only mUId)
  writeText $ maybe "User not found" ("Hello, " ++ ) userName

main :: IO ()
main = serveSnaplet defaultConfig initApp
```

Here I've created my `App` data type, which is passed around through all
requests. We initialize the application using `initApp`, which creates a Snaplet
itself, with a `Snaplet Postgres` nested inside it. This will let us perform
database queries. Finally, I've added a single route to the application - this
route is a bit different from the one we saw before, as it also binds a variable
- that's what `:id` is for. This means that we can go to pages like `/hello/10`
and our `helloDb` handler will be fired, with `id` set to `10`.

Our `helloDb` handler is also a bit more complex now. I first retrieve the `id`
parameter, here somewhat circumventing the safety of `Maybe`. We then use `with`
to "move into" our database Snaplet, which now lets us use things in the
`Handler b Postgres` monad - in this case, `query`. Finally, we return this back
up to our `helloDb` application, and then write out a greeting.

Snap is my favourite choice for building web applications - I find the beautiful
combination of simplicity and elegance in `snap-core` is extremely powerful
when combined with the snaplets abstraction. If you're looking to do some web
programming with Haskell over the weekend, and want a framework with a low
barrier to entry, you could do far worse than give Snap a try.
