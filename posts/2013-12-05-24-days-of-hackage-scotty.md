---
title: 24 Days of Hackage: scotty
---

A fairly new movement in the web programming scene is to build web sites using
so called *micro frameworks*. Way back in 2005,
[Ruby on Rails](http://rubyonrails.org) changed the web programming landscape by
bringing to the masses the idea of building web applications out of re-usable
components. However, by today's standards, Ruby on Rails stands as a somewhat
heavy-weight solution. Idiomatically, it's strongly
[MVC](http://en.wikipedia.org/wiki/Model%E2%80%93view%E2%80%93controller), but
sometimes you just don't need that. The micro frameworks movement is much more
about a small DSL that is designed specifically for routing web requests, and
doing a bit of logic depending on what URL the user requests. Python has
[Flask](http://flask.pocoo.org/) and [Bottle](http://bottlepy.org/docs/dev/),
Ruby has [Sinatra](http://www.sinatrarb.com/) and [Camping](http://camping.io/),
Perl has [Dancer](http://www.perldancer.org/), and of course we have a solution
in Haskell too: [`scotty`](http://hackage.haskell.org/package/scotty).

[Last year](/posts/2012-12-19-24-days-of-hackage-snap.html) we took a brief look
at the [Snap framework](http://snapframework.com), which still remains my framework
of choice when it comes to building web applications. But much like I alluded to
earlier with Ruby, Snap can also feel a tad heavyweight - especially when you
just want to throw up an application under a URL for prototyping purposes. This
is exactly where `scotty` shines.

`scotty` is a DSL that works with a combination of two monads - the `ScottyM`
monad and the `ActionM` monad. Your entire application sits in the `ScottyM`
monad, while requests are processed by `ActionM` actions. We introduce `ActionM`
actions by providing a *route* to them.

For example, we have just been comissioned by Christmas & Christmas
Ltd. to build their new social Christmas app, chrstms.ly. All we need to put
online for now is a landing page that lets people enter their email address in
order to record their interest, and to let them know when we launch. The first thing
we can do is define our routes:

```haskell
chrstmsly :: ScottyM ()
chrstmsly = do
  get "/" showLandingPage
  post "/register" register
```

`get` and `post` both take a route as their first parameter, and an `ActionM`
action to run if the route is matched. Naturally, `get` only matches if the HTTP
request method is `GET`, and `post` only matches `POST` requests. Easy! Now we
need to actually implement the actions. For the `showLandingPage` action we'll
just serve out a HTML file our designers provided us:

```haskell
showLandingPage :: ActionM ()
showLandingPage = do
  setHeader "Content-Type" "text/html"
  file "landing.html"
```

For the registration we need to do a bit more. I'll assume that we've been given
a `registerInterest :: String -> IO (Maybe String)` function by someone else in the
team. The designers have told us that full page requests are so 90s, and now
they are using AngularJS and want to do registration with full AJAX to keep the
UI slick. We just need to be able to take a POST request with an email address
as a query parameter, and return a JSON document to signal whether or not the
request was handled correctly. Here's what this might look like:

```haskell
register :: ActionM ()
register = do
  emailAddress <- param "email"
  registered <- liftIO (registerInterest emailAddress)
  case registered of
    Just errorMessage -> do
      json $ object [ "error" .= errorMessage ]
      status internalServerError500

    Nothing -> do
      json $ object [ "ok" .= ("ok" :: String) ]
```

We use `param` to pull out the `email` parameter from the submission, and then
try and call the `registerInterest` routine. `registerInterest` is an `IO`
action, so we have to use `liftIO` to embed it into our `ActionM` action. This
returns `Just errorMessage` if it fails - in which case we relay the error to
the browser as a JSON object, and indicate the failure by setting the response
status code to 500. If it was successful, we indicate that everything was OK
with an "ok" JSON object. The browser will also see a 200 response code, as that
is the default from `scotty`.

But wait a minute - we're being a little bit too trusting here. Web programming
101 says that we should never trust requests coming into the system, and that's
no different here. For example, what happens if we `POST` to `/register` without
an email address? In that case, we'll fail to pull out the parameter and
we'll throw an exception! We can do better than that.

`scotty` comes with a simple exception handling system. With it, we can have
multiple actions for the same URL, and we can fall-through to the next action
when exceptions are thrown. To do so, we change our `chrstmsly` application to
the following:

```haskell
chrstmsly :: ScottyM ()
chrstmsly = do
  get "/" showLandingPage
  post "/register" register
  post "/register" registrationFailure
```

Next, we write the `registrationFailure` action:

```haskell
registrationFailure :: ActionM ()
registrationFailure = do
  json $ object [ "error" .= ("Invalid request" :: String) ]
  status badRequest400
```

Finally, we add a bit more exception handling to the `register` action. The
current behavior is to throw an exception if the `email` parameter isn't in the
request, but we'd rather fall through to `registrationFailure`. We can use
`rescue` to do this:


```haskell
register :: ActionM ()
register = do
  emailAddress <- param "email" `rescue` (const next)
  registered <- liftIO (registerInterest emailAddress)
  case registered of
    Just errorMessage -> do
      json $ object [ "error" .= errorMessage ]
      status internalServerError500

    Nothing -> do
      json $ object [ "ok" .= ("ok" :: String) ]
```

`rescue` is given the contents of the exception, but in this case we don't care
and simply request the next action. Now, when we can't pull out the `email`
parameter, we will instead fall through to `registrationFailure` and provide a
much more understandable response.

The last piece of the puzzle is to serve the application. `scotty` uses the WAI
interface, so any WAI-compatible server can be used to serve the
application. It also comes with support for launching the application using
[`warp`](http://hackage.haskell.org/package/warp) with the `scotty` action:

```haskell
main :: IO ()
main = scotty 9176 chrstmsly
```

If we run this and head over to http://localhost:9176, we get presented with our
application. We can poke around with `curl` to see what happens:

```
ollie@io ~> curl 'http://localhost:9176/'
<!DOCTYPE html>
<html>
  <head>
    <title>Coming soon...</title>
  </head>
  <body>...</body>
</html>

ollie@io ~> curl -XPOST 'http://0:9176/register?email=a@b.com'
{"ok":"ok"}⏎

ollie@io ~> curl -XPOST 'http://0:9176/register?email=foo'
{"error":"I broke :("}⏎
```

This post is my first experience with using `scotty`, but I'm really excited
about it. The real value I see is that `scotty` is really simple, and this is
fantastic for people who are new to Haskell. There are only a few things that
you have to understand - how to do routing, and how to write actions. After
that, it's bread-and-butter Haskell. While `scotty` doesn't come with a huge
amount included, it is extendable with WAI middleware, of which there is
[a lot of](http://hackage.haskell.org/packages/search?terms=wai). Furthermore,
by keeping it simple, `scotty`-built applications don't look particularly
alien when you put them next to a Python project, for example. Thus I think
`scotty` is probably going to become one of my go-to suggestions to give to
people when they graduate from LYAH and want to try building something a little
more practical.

If you like the look of `scotty`, [Aditya Bhargava](http://adit.io) has written
a
[much more in-depth tutorial](http://adit.io/posts/2013-04-15-making-a-website-with-haskell.html) -
go check it out! The code for today's blog post can be found
[here](https://github.com/ocharles/blog/blob/master/code/2013-12-05-scotty.hs).
