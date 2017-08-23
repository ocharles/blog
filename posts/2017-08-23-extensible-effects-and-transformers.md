---
title: Providing an API for extensible-effects and monad transformers
---

I was recently working on a small little project - a client API for the
[ListenBrainz](https://listenbrainz.org) project. Most of the details aren't
particularly interesting - it's just a HTTP client library to a REST-like API
with JSON. For the implementation, I let Servant and aeson do most of the heavy
lifting, but I got stuck when considering what final API to give to *my* users.

Obviously, interacting with ListenBrainz requires some sort of IO so whatever
API I will be offering has to live within some sort of monad. Currently, there
are three major options:

1. *Supply an API targetting a concrete monad stack.*

    Under this option, our API would have types such as
    
    ```
    submitListens :: ... -> M ()
    getListens :: ... -> M Listens
    ```
    
    where `M` is some *particular* monad (or monad transformer).
    
2. *Supply an API using type classes*

    This is the `mtl` approach. Rather than choosing which monad my users have
    to work in, my API can be polymorphic over monads that support accessing the
    ListenBrainz API. This means my API is more like:

    ```
    submitListens :: MonadListenBrainz m => ... -> m ()
    getListens :: MonadListenBrainz m => ... -> m Listens
    ```

3. *Use an extensible effects framework.*

    Extensible effects are a fairly new entry, that are something of a mix of
    the above options. We target a family of concrete monads - `Eff` - but the
    extensible effects framework lets our effect (querying ListenBrainz)
    seamlessly compose with other effects. Using `freer-effects`, our API would
    be:
    
    ```
    submitListens :: Member ListenBrainzAPICall effects => ... -> Eff effects ()
    getListens :: Member ListenBrainzAPICall effects => ... -> Eff effects Listens
    ```

So, which do we choose? Evaluating the options, I have some concerns. 

For option one, we impose pain on all our users who want to use a different
monad stack. It's unlikely that you're application is going to be written soley
to query ListenBrainz, which means client code becomes littered with `lift`. You
may write that off as syntatic, but there is another problem - we have committed
to an interpretation strategy. Rather than describing API calls, my library now
skips directly to prescribing how to run API calls. However, it's entirely
possible that you want to intercept these calls - maybe introducing a caching
layer or additional logging. Your only option is to duplicate my API into your
own project and wrap each function call and then change your program to use
your API rather than mine. Essentially, the program itself is no longer a first
class value that you can transform.

Extensible effects gives us a solution to both of the above. The use of the
`Member` type class automatically reshuffles effects so that multiple effects
can be combined without syntatic overhead, and we only commit to an
interpretation strategy when we actually run the program. `Eff` is essentially a
*free monad*, which captures the syntax tree of effects, rather than the result
of their execution.

Sounds good, but extensible effects come with (at least) two problems that make
me hesistant: they are experimental and esoteric, and it's unclear that they are
performant. By using *only* extensible effects, I am forcing an extensible
effects framework on my users, and I'd rather not dictate that. Of course,
extensible effects can be composed with traditional monad transformers, but I've
still imposed an unnecessary burden on my users.

So, what do we do? Well, as Old El Paso has taught us: why don't we have both?

It's trivial to actually support both a monad transformer stack *and* extensible
effects by using an `mtl` type class. As I argue in [Monad transformers, free
monads, mtl, laws and a new
approach](/blog/posts/2016-01-26-transformers-free-monads-mtl-laws.html), I
think the best pattern for an `mtl` class is to be a monad homomorphism from a
program description, and often a free monad is a fine choice to lift:

```
class Monad m => MonadListenBrainz m where
  liftListenBrainz :: Free f a -> m a
```

But what about `f`? As observed earlier, extensible effects are basically free
monads, so we can actually share the same implementation. For `freer-effects`,
we might describe the ListenBrainz API with a GADT such as:

```
data ListenBrainzAPICall returns where
  GetListens :: ... -> ListenBrainzAPICall Listens
  SubmitListens :: ... -> ListenBrainzAPICall ()
```

However, this isn't a functor - it's just a normal data type. In order for 
`Free f a` to actually be a monad, we need `f` to be a functor. We could rewrite
`ListenBrainzAPICall` into a functor, but it's even easier to just fabricate a
functor for free - and that's exactly [what `Coyoneda` will
do](https://www.reddit.com/r/haskelltil/comments/4ea7er/coyoneda_is_just_the_free_functor/).
Thus our `mtl` type class becomes:

```
class Monad m => MonadListenBrainz m where
  liftListenBrainz :: Free (Coyoneda ListenBrainzAPICall) a -> m a 
```

We can now provide an implementation in terms of a monad transformer:

```
instance Monad m => MonadListenBrainz (ListenBrainzT m)
  liftListenBrainz f =
    iterM (join . lowerCoyoneda . hoistCoyoneda go)

    where
      go :: ListenBrainzAPICall a -> ListenBrainzT m a
```

or extensible effects:

```haskell 
instance Member ListenBrainzAPICall effs => MonadListenBrainz (Eff effs) where
  liftListenBrainz f = iterM (join . lowerCoyoneda . hoistCoyoneda send) f 
```

or maybe directly to a free monad for later inspection:

```haskell
instance MonadListenBrainz (Free (Coyoneda ListenBrainzAPICall)) where
  liftListenBrainz = id
```

For the actual implementation of performing the API call, I work with a concrete
monad transformer stack:

```
performAPICall :: Manager -> ListenBrainzAPICall a -> IO (Either ServantError a)
```

which both my extensible effects "run" function calls, or the `go` function in
the `iterM` call for `ListenBrainzT`'s `MonadListenBrainz` instance.

In conclusion, I'm able to offer my users a choice of either:

* a traditional monad transformer approach, which doesn't commit to a particular
  intepretation strategy by using an `mtl` type class
* extensible effects

All without extra syntatic burden, a complicated type class, or duplicating the
implementation.

You can see the final implemantion of [`listenbrainz-client`
here](https://hackage.haskell.org/package/listenbrainz-client-1.0.1/docs/src/Web-ListenBrainz.html).


### Bonus - what about the ReaderT pattern?

The [ReaderT design
pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern) has
been mentioned recently, so where does this fit in? There are two options if we
wanted to follow this pattern:

* We require a HTTP `Manager` in our environment, and commit to using this. This
  has all the problems of providing a concrete monad transformer stack - we are
  committing to an interpretation.
* We require a family of functions that explain how to perform each API call.
  This kind of like a [van Laarhoven free
  monad](http://r6.ca/blog/20140210T181244Z.html), or really just explicit
  dictionary passing. I don't see this really gaining much on abstracting with
  type classes.
  
I don't feel like the ReaderT design pattern offers anything that isn't already
dealt with above.
