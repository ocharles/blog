---
title: Another Approach to Default Function Parameters
---

Recently, there has been some new discussion around the issue of providing
default values for function parameters in Haskell. First, [Gabriel
Gonzalez](http://www.haskellforall.com) showed us his new
[`optional-args`](http://www.haskellforall.com/2015/06/optional-args-100-optional-function.html)
library, which provides new types for optional arguments along with heavy
syntactic overloading. To follow that, [Dimitri
Sabadie](http://phaazon.blogspot.fr) published a blog post [discouraging the
use](http://phaazon.blogspot.fr/2015/07/dont-use-default.html) of the currently
popular
[`Default`](https://hackage.haskell.org/package/data-default-0.5.3/docs/Data-Default.html)
type class. These are both good discussions, and as with any good discussion
have been lingering around in the back of my head.

Since those discussions took place, I've been playing with my point in the
FRP-web-framework design space -
[Francium](https://github.com/ocharles/Francium). I made some big refactorings
on an application using Francium, mostly extending so called "component" data
types (buttons, checkboxes, etc), and was frustrated with how much code broke
just from introducing new record fields. The [Commercial Haskell](http://commercialhaskell.com) group
published an article on [how to design for
extensibility](https://github.com/commercialhaskell/haskelldocumentation/blob/master/content/designing-apis-for-extensibility.md)
back in March, so I decided to revisit that.

It turns out that with a little bit of modification, the approach proposed in
designing for extensibility also covers optional arguments pretty well!

First, let's recap what it means to design for extensibility. The key points
are:

1. Functions take `Settings` values, which specify a general configuration.
2. These `Settings` values are opaque, meaning they cannot be constructed by a
   data constructor, but they have a smart constructor instead. This
   smart constructor allows you to provide default values.
3. Provide get/set functions for all configurable fields in your `Settings` data
   type, preventing the use of record syntax for updates (which leaks
   implementation details).

Regular Haskell users will already be familiar a pattern that can be seen in point 3:
we often use a different piece of technology to solve this problem - lenses. Lenses
are nice here because they reduce the surface area of our API - two exports can
be reduced to just one, which I believe reduces the time to learn a new library.
They also compose very nicely, in that they can be embedded into other
computations with ease.

With point 3 amended to use some form of lens, we end up with the following
type of presentation. Take a HTTP library for example. Our hypothetical library
would have the following exports:

```haskell
data HTTPSettings

httpKeepAlive :: Lens HTTPSettings Bool
httpCookieJar :: Lens HTTPSettings CookieJar

defaultHTTPSettings :: HTTPSettings

httpRequest :: HTTPSettings -> HTTPRequest -> IO Response
```

which might have usage

```haskell
httpRequest
  (defaultHTTPSettings & httpKeepAlive .~ True)
  aRequest
```

This is an improvement, but I've never particularly liked the reverse function
application stuff with `&`. The repeated use of `&` is essentially working in an
`Endo` `Writer` monad, or more generally - a state monad. The `lens` library
ships with operators for working specifically in state monads (of course it
does), so let's use that:

```haskell

httpRequest :: State HTTPSettings x -> HTTPRequest -> IO Response

....

httpRequest
  (do httpKeepAlive .= True)
  aRequest
```

It's a small change here, but when you are overriding a lot of parameters, the
sugar offered by the use of `do` is hard to give up - especially when you throw
in more monadic combinators like `when` and `unless`.

With this seemingly simple syntactic change, something interesting has happened;
something which is easier to see if we break open `httpRequest`:

```haskell
httpRequest :: State HTTPSettings x -> HTTPRequest -> IO Response
httpRequest mkConfig request =
  let config = execState mkConfig defaultHttpSettings
  in ...
```

Now the default configuration has moved *inside* the HTTP module, rather than
being supplied by the user. All the user provides is essentially a function
`HTTPSettings -> HTTPSettings`, dressed up in a state monad. This means that to
use the default configuration, we simply provide a do-nothing state composition:
`return ()`. We can even give this a name

```haskell
def :: State a ()
def = return ()
```

and voila, we now have the lovely name-overloading offered by `Data.Default`,
but without the need to introduce a lawless type class!

To conclude, in this post I've shown that by slightly modifying the presentation
of an approach to build APIs with extensibility in mind, we the main benefit of
`Data.Default`. This main benefit - the *raison d'être* of `Data.Default` - is
the ability to use the single symbol `def` whenever you just want *a*
configuration, but don't care what it is. We still have that ability, and we
didn't have to rely on an ad hoc type class to get there.

However, it's not all rainbows and puppies: we did have to give something up to
get here, and what we've given up is a compiler enforced consistency. With
`Data.Default`, there is only a single choice of default configuration for a
given type, so you know that `def :: HTTPSettings` will be the same set of
defaults *everywhere*. With my approach, exactly what `def` means is down to the
function you're calling and how they want to interpret `def`. In practice, due
to the lack of laws on `def`, there wasn't much reasoning you could do about
what that single instance was anyway, so I'm not sure much is given up in
practice. I try and keep to a single interpretation of `def` in my libraries by
still exporting `defaultHTTPSettings`, and then using `execState mkConfig
defaultHTTPSettings` whenever I need to interpret a `State HTTPConfig`.
