---
title: 24 Days of Hackage: extensible-effects
---

To a lot of programmers, the idea of programming with monads is almost
synonymous with the idea of programming in Haskell. Of course, to those us who
write a lot of Haskell we only consider monads a small part of our work - but it's
true, monads do have a fairly important role in how we structure our
computations. Despite the benefits monads give us, they are not without their
pain points. For a long time, we've known how to combine monads (see for
example, last year's post on
[`transformers`](/posts/2012-12-20-24-days-of-hackage-transformers.html)) - but
we've also known that monads are *not* closed under composition. This means that
if you take any two monads, it's not always true that they can be composed to
form a new monad. There's some great discussion of this on
[Stack Overflow](http://stackoverflow.com/questions/13034229/concrete-example-showing-that-monads-are-not-closed-under-composition-with-proo),
which I highly recommend reading.

So even though it's not always possible to compose monads, we do at least have
the `transformers` and `mtl` libraries to allow us to layer up monads to make
more powerful monads. This is definitely a very powerful way of programming, but
even then having an enforced ordering of the layers can make interaction
*between* the layers a difficult task. Section 5 of the paper
"[Extensible Effects](http://okmij.org/ftp/Haskell/extensible/index.html)" by
Kiselyov, Sabry and Swords discusses just this, and they show some computations
that simply can't be expressed by independent layering of monads. I won't go
into the details now, but this is also interesting reading!

If we can't always compose monads, and the layering can be cumbersome, is it
time to abandon ship and go back to writing Java? Absolutely not! Monad
transformers are actually only one of the available tools we have available, and
since the above paper was published we have another new tool -
[`extensible-effects`](http://hackage.haskell.org/package/extensible-effects). `extensible-effects`
is a direct implemantion of the concepts explored in the paper. Today, we'll
have a look at how we can use this library to build effectful computations.

The problem that we're looking to solve is being able to add in logging support
for applications. However, we might change our minds on how we want to perform
this logging - for example in production we want to log to syslog, in
development we want to log to `stderr`, and in tests we want to be able to
collect the logs into some value that we can inspect - after all, logging is a
feature and should be tested as such. Let's have a look at how we can do this
using the effect framework given by `extensible-effects`.

The main twist with extensible effects is that it operates under a client/server
type abstraction. You, the client, program under the `Eff` monad, and send
*requests* to a "server". This server then interprets the request, performing
whatever effects it needs to, and then responds to the client. This describes
the "effects" part - the "extensible" part comes out of the fact that the set of
possible requests and servers to handle them is open - users are able to define
their own, outside the `extensible-effects` library.

We can see this by looking closer at the type of the `Eff` monad. `Eff` takes a
type parameter `r` which describes the set of "requests" that a client can send,
which in turn corresponds to the set of possible effects a program can
have. This is an open set, so you can add in new effects in your own
libraries. In our case, we're going to build a logging effect. Starting from the
top-level, we want to be able to write something like this:

```haskell
verboseAddition :: Member Log r => Eff r Int
verboseAddition = do
  log "I'm starting with 1..."
  x <- return 1

  log "and I'm adding 2..."
  y <- return 2

  let r = x + y

  log $ "Looks like the result is " ++ show r
  return r
```

So we know that we have a client/server model, and that this is all managed for
us by the `Eff` monad. But how do we actually submit a request? This is done
using the `send` primitive. The type of `send` is a bit scary...

```haskell
send :: (forall w. (a -> VE w r) -> Union r (VE w r)) -> Eff r a
```

...but it's actually really quite simple. To `send` a request, we have to
provide a function that will produce the description of a step in our final
computation. We can also provide a result to the next action, though we won't
need that functionality here. I'll jump right in to the code, and we'll reflect
on how this works after:

```haskell
data Log v = Log String v deriving (Functor, Typeable)

log :: Member Log r => String -> Eff r ()
log txt = send $ \next -> inj (Log txt (next ()))
```

To `log` some text, we introduce a `Log` action and keep hold of the text we
should be logging. Logging doesn't produce a result, so we simply pass on `()`
to the next action. This lets us construct a `Log` value, which we *inject* into
the open set of requests using `inj`.

Now that we have a way to introduce the effect, we also need
a way to run it. The concept of running events mean that if we are given an
`Eff` with some effect in its set of effects, we can produce a new `Eff` which
doesn't have that effect - because we've now performed it. So we
gradually run the effects we need to, until we have ran all effects.

There are two functions that we need to run effects. The first is `admin`, which
is used to begin the evaluation process. `admin` turns our `Eff r a` into a `VE
a r` - and this type indicates whether we are producing a value (possibly using
other effects), or performing an effect and another computation. Let's start by
dealing with the simple case - `Val`:

```haskell
runLogger :: Eff (Log :> r) a -> Eff r (a, [String])
runLogger logAction = go (admin logAction)
 where
  go (Val v) = return (v, [])
```

If our `logAction` is constructed with `Val` then it produced no log lines, so
we return an empty log, and the value itself.

So far so good. Next, we need to do the interesting part - actually dealing with
logging effect. This is done by using the `handleRelay` function. `handleRelay`
takes a request, and works out whether or not it is an effect that we should
deal with, or if it is a different type of effect. Thus we need to know how to
relay requests on, and how to deal with an actual logging request. Relaying on
requests is as simple as just calling `go`. If we have a request to log
something, then we perform that logging, and then perform the rest of the
computation. Therefore, we end up with:

```haskell
runLogger :: Eff (Log :> r) a -> Eff r (a, [String])
runLogger logAction = go (admin logAction)
 where
  go (Val v) = return (v, [])
  go (E request) =
    let prefixLogWith txt (v, l) = (v, txt:l)
        performLog (Log txt next) =
          fmap (prefixLogWith txt) (go next)
    in handleRelay request go performLog
```

We use `handeRelay` on the request, and use `go` to relay on requests that
aren't interesting to us. If it's a logging request, then we have access to the
`Log` constructor we introduced earlier. We pattern match on this to discover
the text that we have to log, and the computation to run after performing the
logging. The later computation will return a value and a log, so we just prefix
the log of the further computation with this log entry. And we're done!

We can now run our verbose addition:

```
> :t run (runLogger verboseAddition) 
run (runLogger verboseAddition) :: (Int, [String])

> run (runLogger verboseAddition)
(3,["I'm starting with 1...","and I'm adding 2...","Looks like the result is 3"])
```

We can also run our logger action by logging to `stderr`, and here things become
a bit more interesting. If you have a computation that requires logging, and you
want to run it by logging to `stderr` then you *actually* have a computation
that requires logging *and* IO. Here we see our first interaction of
effects. Thankfully, it's really quite straight forward. Here's the code:

```haskell
runIOLogger :: SetMember Lift (Lift IO) r => Eff (Log :> r) a -> Eff r a
runIOLogger logAction = go (admin logAction)
 where
  go (Val v) = return v
  go (E request) =
    let performLog (Log txt next) =
          lift (putStrLn txt) >> go next
    in handleRelay request go performLog
```

Now our `performLog` function has changed. We pattern match on the `Log` request
as we did before, but the `Eff` we now return is to first perform some IO, and
then to perform the rest of the computation. It's also worth noticing how the
top-level type signature has changed. We can see that `runIOLogger` takes an
`Eff` and removes the logging effect (by running it), which gives us a subset of
the original effects. However, that subset also has to contain the ability to
perform IO - and that's what `SetMember Lift (Lift IO) r` is all about.

This time, we have to run the action in a slightly different form:

```
> :t runLift $ runIOLogger verboseAddition
runLift $ runIOLogger verboseAddition :: IO Int

> runLift $ runIOLogger verboseAddition   
I'm starting with 1...
and I'm adding 2...
Looks like the result is 3
3
```

I really like the `extensible-effects` work, and find it a very refreshing way
of exploring ideas. While I haven't built anything hugely complex with it, I
certainly hope I will, as I think it's a really powerful way of
programming. [Oleg mentioned](http://www.haskell.org/pipermail/haskell-cafe/2013-November/111551.html)
in the Haskell cafe that

> I must stress that thinking of extensible-effects effects as just another
> implementation of MTL is not productive. Not all effects can be decomposed
> into State, Reader, etc. layers. Manly [sic], effects should not be decomposed
> into layers.

It's entirely possible my post has missed the point, so I look forward to being
told just how wrong I am! The code for today's post is on
[Github](https://github.com/ocharles/blog/tree/master/code).
