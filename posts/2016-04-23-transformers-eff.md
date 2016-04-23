---
title: Announcing transformers-eff
---

In my [last post](/posts/2016-01-26-transformers-free-monads-mtl-laws.html), I
spent some time discussing a few different approaches to dealing with
computational effects in Haskell - namely monad transformers, free monads, and
the monad transformer library. I presented an approach to systematically
building `mtl`-like type classes based on the idea of lifting languages for a
given effect into larger monad transformer stacks. This approach felt so
mechanical to me I set about exploring a way to formalise it, and am happy to
announce a new experimental library --
[`transformers-eff`](https://hackage.haskell.org/package/transformers-eff).

`transformers-eff` takes inspiration from the work of algebraic
effects and handlers, and splits each effect into composable programs for
introducing effects and handlers that eliminate these effects. As the name
indicates, this work is also closely related to monad transformer stacks, as
they provide the implementation of the specific effects. I believe the
novelty in my approach is that we can do this entirely within the system of
monad transformers, and this observation makes it very convenient to create
re-usable effects.

## Core API

Before looking at an example, I want to start by presenting the core API. First,
we have the `Eff` monad transformer:

```haskell
data Eff (f :: * -> *) (m :: * -> *) (a :: *)
```

If you squint, you'll see that  `Eff` has the familiar shape of a *monad
transformer* - it transforms a given monad `m`, providing it access to effects
described by `f`. As `Eff f m` is itself a monad, it's possible to stack `Eff`s
together. The type parameter `f` is used to indicate which effects this `Eff`
transformer talks about.

Next, the library provides a way to eliminate `Eff` by *translating* it into a
concrete monad transformer:

```haskell
translate :: (Monad m,Monad (t m),MonadTrans t)
          => (forall x r. f x -> ContT r (t m) x)
          -> Eff f m a
          -> t m a
```

Translations are defined by a single function that is very similar to the type
of "lifts" we saw in my previous blog post. The difference here is that the
homomorphism maps into `ContT`, which allows the translation to adjust control
flow. For many effects it will be enough to simply `lift` directly into this,
but it can be useful to inspect the continuation, for example to build
non-deterministic computations.

Finally, we have one type class method:

```haskell
interpret :: (Monad m) => f a -> m a
```

However, this type class is fairly constrained in its instances, so you should
read `m` as actually being some sort of monad transformer stack containing
`Eff f`.

## Examples

Let's dive in and look at some examples.

### Reader effects

Last post we spent a lot of time looking at various representations of the
reader monad, so let's see how this looks under `transformers-eff`.

We already have a definition for our language, `r -> a` as we saw last week.
While we could work directly with this, we'll be interpreting into `ReaderT` so
I'll use the `Reader` newtype for a little extra readibility. Given this
language, we just need to write a translation into a concrete monad
transformer, which will be `ReaderT`:

```haskell
effToReaderT :: Monad m => Eff (Reader e) m a -> ReaderT e m a
effToReaderT = translate (\r -> lift (hoist generalize r))
```

This is a little dense, so let's break it down. When we call `translate`, we
have to provide a function with the type:

```haskell
forall a m. Reader r a -> ContT _ (ReaderT r m) a
```

The `ReaderT r m` part is coming from the type we gave in the call to
`translate`, that is -- the type of `effToReaderT`. We don't really need to
concern outselves with continuations for this effect, as reading from a fixed
environment does not change the flow of control - so we'll begin with `lift`. We
now have to produce a `ReaderT r m a` from a `Reader r a`. If we notice that
`Reader r a = ReaderT r Identity a`, we can make use of the tools in the
`mmorph` library, which lets us map that `Identity` to any `m` via `hoist
generalize`.

We still need a way to easily introduce these effects into our programs,
and that means writing an `mtl` type class. However, the instances require
almost no work on our behalf *and* we only have to provide two, making this is a
very quick process:

```haskell
class (Monad m) => EffReader env m | m -> env where
  liftReader :: Reader env a -> m a

instance Monad m => EffReader env (Eff (Reader env) m) where
  liftReader = interpret

instance {-# OVERLAPPABLE #-} EffReader env m =>
           EffReader env (Eff effects m) where
  liftReader = lift . liftReader
```

I then provide a user-friendly API built on this lift operation:

```haskell
ask :: EffEnv e m => m e
ask = liftReader (Reader id)
```

Finally, most users are probably more interested in running the effect rather
than just translating it to `ReaderT`, so let's provide a convenience function
to translate and run all in one go:

```haskell
runReader :: Eff (Reader r) m a -> r -> m a
runReader eff r = runReaderT (effToReaderT eff) r
```

In total, the reader effect is described as:

```haskell
class (Monad m) => EffReader env m | m -> env where
  liftReader :: Reader env a -> m a

instance Monad m => EffReader env (Eff (Reader env) m) where
  liftReader = interpret

instance {-# OVERLAPPABLE #-} EffReader env m =>
           EffReader env (Eff effects m) where
  liftReader = lift . liftReader

ask :: EffEnv e m => m e
ask = liftReader (Reader id)

effToReaderT :: Monad m => Eff (Reader e) m a -> ReaderT e m a
effToReaderT = translate (\r -> lift (hoist generalize r))
```

### A logging effect

We also looked at a logging effect last week, and this can also be built using
`transformers-eff`:

```haskell
data LoggingF message a = Log message deriving (Functor)

class (Monad m) => EffLog message m | m -> message where
  liftLog :: Free (LoggingF message) a -> m a

instance Monad m => EffLog env (Eff (Free (LoggingF message)) m) where
  liftLog = interpret

instance {-# OVERLAPPABLE #-} EffLog env m =>
           EffLog env (Eff effects m) where
  liftLog = lift . liftLog

log :: EffLog message m => message -> m ()
log = liftLog . liftF . Log

runLog :: (MonadIO m)
       => Eff (Free (LoggingF message) e) m a
       -> (message -> IO ())
       -> m a
runLog eff =
  runIdentityT (translate (iterM (\(Log msg) -> liftIO (io msg))))
```

The interpretation here is given an `IO` action to perform whenever a message is
logged. I could have implemented this in a few ways - perhaps lifting the whole
computation into `ReaderT (message -> IO ())`, but instead I have just used
`IdentityT` as the target monad transformer, and added a `MonadIO` constraint
onto `m`. Whenever a message is logged, we'll directly call the given `IO`
action. As you can also see, I've used a free monad as the source language for
the effect. This example demonstrates that we are free to mix a variety of tools
(here free monads, `MonadIO` and the identity transformer) in order to get the
job done.


## What does this approach bring?

### Less type class instances

We saw above that when we introduced our `EffLog` type class, it was
immediately available for use along side `EffReader` effects - and we didn't
have to do anything extra! To me, this is a huge win - I frequently find myself
frustrated with the amount of work required to do when composing many different
projects together with `mtl`, and this is not just a theoretical frustration. To
provide just one example from today, I wanted to use `ListT` with some Yesod
code that required `MonadLogger`. There is obviously no `MonadLogger` instance
for `ListT`, and it's almost unsolvable to provide such an instance withoutrs/o
using orphan instances - neither one of those libraries should need to depend on
the other, so we're stuck! If you stay within `Eff`, this problem doesn't occur.

Many will be quick to point out that in `mtl` it doesn't necessary make sense to
have all transformers compose due to laws (despite the lack of any laws actually
being stated...), and I'm curious if this is true here. In this library, due to
the limitation on having to write your effectful programs based on an underlying
algebra, I'm not sure it's possible to introduce the problematic type class
methods like `local` and `catch`.

### One effect at a time

In the `mtl` approach a single monad transformer stack might be able to deal
with a whole selection of effects in one go. However, I've found that this can
actually make it quite difficult to reason about the flow of code. To provide an
example, let's consider this small API:

```haskell
findOllie :: (MonadDb m, MonadPlus m) => m Person
findOllie =
  do x <- dbLookup (PersonId 42)
     guard (personName x == "Ollie")
     return x

type QueryError = String
dbLookup :: (MonadDb m, MonadError QueryError m) => PersonId -> m Person

data DbT m a
instance Monad m => Monad (DbT m)
instance Monad m => MonadDb (DbT m)

runDb :: (MonadIO m) :: DbT m a -> m a
```

If we just try and apply `runDb` to `findOllie`, we'll get

```
runDb findOllie :: (MonadError QueryError m, MonadIO m, MonadPlus m) => m Person
```

We still need to take care of `MonadError` and `MonadPlus`. For `MonadError`
I'll use `ExceptT`, and for `MonadPlus` I'll use `MaybeT`:

```
runMaybeT (runExceptT (runDb findOllie)) :: IO (Maybe (Either QueryError Person))
```

Next, let's consider a few scenarios. Firstly, the case where everything
succeeds -

```
> runMaybeT (runExceptT (runDb findOllie))
Just (Right Person ...)
```

However, that query could fail, which would cause an error

```
> runMaybeT (runExceptT (runDb findOllie))
Just (Left "Table `person` not found")
```

Still as expected. Finally, person 42 might not actually be me, in which case we
get

```
> runMaybeT (runExceptT (runDb findOllie))
Just (Left "")
```

Huh? What's happened here is that we've hit the `MonadPlus` instance for
`ExceptT`, and because our `QueryError` is a `String` we have a `Monoid`
instance, so we were given an "empty" error. This is not at all what we were
expecting!

While this example is a contrived one, I am very nervous that this accidental
choice of instances could happen deep within another section of code, for
example where I expect to do some local error handling and accidentally
eliminate a chance of failure that I was expecting to deal with elsewhere.

In `transformers-eff` this is not possible, as each `Eff` deals with one
*and only one* effect at a time. This could be done with `mtl` by introducing a
separate type class for failure and only adding an instance for `MaybeT`, we are
working around the problem by convention, and I would much rather bake that in
to the types.

### Fast code

The underlying implementation of `Eff` is built on top of continuations, and due
to aggressive inlineing, GHC is able to work some serious magic. In fact, in all
the benchmarks I've produced so far, `Eff` is as fast as `transformers`, and
even comes out slightly faster in one (though within the same order of
magnitude).

### Compatible with the rest of Hackage

As `Eff` is just another monad transformer, you can stack in other monad
transformers. Note that by doing this you may lack the type class instances you
need, so explicit `lift`ing might be necessary. I mainly expect this being
useful by putting `Eff` "on the top" - for example I can use `Eff` locally with
in a `Snap` monad computation, provided I eventually run back down to just
`Snap`. This is the same pattern as locally using `transformers`.
