---
title: 24 Days of GHC Extensions: Nullary Type Classes
---

While I wait for a few more guest posts to soldify, I thought it might be fun to
look at another new extension to GHC -
[`NullaryTypeClasses`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/type-class-extensions.html#nullary-type-classes).
 
> {-# LANGUAGE NullaryTypeClasses #-}

Ordinarily in Haskell, a type class is defined as the following:

```haskell
class ctx => C a where
  ...
```

That is, our type class can have a context, and can operate on a single type
parameter. However, working with one type is a restriction - later, we'll see
about working with multiple type parameters is there any reason we couldn't work
on *no* type parameters?

It sounds crazy, but GHC can do just that! The linked documentation has one
scenario of when you may want to do that - if you need to make a controversial
assumption, by using a nullary type class you can give your end users the choice
as to whether or not they want to commit to using code that requires that
assumption to hold.

However, nullary type classes can also be used as a form of implicit
configuration. For example, let's assume we're building a Haskell library, and
we'd like to give our users the ability to log its activity. There are many ways
to log messages these days - in development we might use `STDERR`, but in
production we might push our logs directly to `journald` or `syslog`.

By using nullary type classes, we can abstract over this logging function, and
let the end user provide a logging strategy. To begin, we define our type class,
mostly as normal - but we don't use *any* type parameters:

> class Logger where
>   logMessage :: String -> IO ()

The presence of this type class allows us to call `logMessage`, but defer the
implementation to the user of the library:

> type Present = String
> queueNewChristmasPresents :: Logger => [Present] -> IO ()
> queueNewChristmasPresents presents = do
>   mapM (logMessage . ("Queueing present for delivery: " ++)) presents
>   return ()

Notice how our library function clearly indicates the assumption - here we are
assuming that it will be possible to log a message, using `IO`.

Now to provide a logging implementation, all we have to do is provide the
instance for the `Logger` type class:

> instance Logger where
>   logMessage t = putStrLn ("[XMAS LOG]: " ++ t)

Running our library function does just what we would expect:
 
```
.> queueNewChristmasPresents ["Cuddly Lambda", "Gamma Christmas Pudding"]
[XMAS LOG]: Queueing present for delivery: Cuddly Lambda
[XMAS LOG]: Queueing present for delivery: Gamma Christmas Pudding
```

Perfect!

If you're feeling a little uncomfortable now - I'm with you. It's not clear to
me whether this is really a good idea or not, but it's nice that it is available
in GHC. In a future post we'll look at another extension that attempts to solve
the implicit configuration problem.
 
----

*This post is part of
[24 Days of GHC Extensions](/pages/2014-12-01-24-days-of-ghc-extensions.html) -
for more posts like this, check out the
[calendar](/pages/2014-12-01-24-days-of-ghc-extensions.html)*.
