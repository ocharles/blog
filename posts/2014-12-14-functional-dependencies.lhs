---
title: 24 Days of GHC Extensions: Functional Dependencies
---

> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FunctionalDependencies #-}
> import Data.Foldable (forM_)
> import Data.IORef

Over the last few days we've seen a few different ways to model the class of mutable variables using type classes. First of all we saw that we could use [type families](/posts/2014-12-12-type-families.html) to associate the type of monad with the type of mutable variables, and yesterday we saw that we could almost take the same approach using [multi-parameter type classes](/posts/2014-12-13-multi-param-type-classes.html). Unfortunately, when we moved to multi-parameter type classes, the type inference engine became a little less useful, as there are multiple possible choices of monad for any given mutable variable.
 
What we really wanted to do with the multiple types was to model a *relation* between the types - knowing the type of the mutable variable should be enough to inform us as to the type of monad. By using the `FunctionalDependencies` extension, we have the ability to augment a type class with information about [*functional dependencies*](https://en.wikipedia.org/wiki/Functional_dependency) - a concept you might already be familiar with from relational database theory. Loosely speaking, a functional dependency lets us indicate that a given set of one or more types determine the type of a single other type. The notation for this is to indicate a list of types and then use an arrow (`->`) to note the dependency.

Revisiting our mutable variables type class, we can now write:

> class Store store m | store -> m where
>  new :: a -> m (store a)
>  get :: store a -> m a
>  put :: store a -> a -> m ()

This is the same type class as yesterday, but we have now indicated that `store` determines `m`. We are able to re-use the existing instances unchanged:
 
> instance Store IORef IO where
>   new = newIORef
>   get = readIORef
>   put ioref a = modifyIORef ioref (const a)
 
However, *now* when we ask for the type of yesterday's `ex` function and choose `IORef` as our store, GHC will be able to infer that the type of `m` must be `IO` - as that was determined by the instance above:
 
```
.> :t ex
ex :: [Present] -> IO [Present]
```
 
Perfect!

While this may seem inconsequential, this use of functional dependencies to direct the type inference engine is significant if we want to build practical libraries. While it's great to be able to do a lot with types, many agree that giving up type inference can be too much of a cost.

That said, the fun doesn't stop there - as functional dependencies and multi-parameter type classes really do start to capture relations between types we can start using type classes as a form of logic programming. A prime example of this is the paper [Fun with Functional Dependencies](http://www.cse.chalmers.se/~hallgren/Papers/wm01.html). Another example is in the work around the [HList](http://hackage.haskell.org/package/HList) library - discussed in the paper [Strongly Typed Heterogeneous Collections](http://okmij.org/ftp/Haskell/HList-ext.pdf).

---

To recap, here is yesterday's code:
 
> type Present = String
> storePresents :: (Store store m, Monad m) => [Present] -> m (store [Present])
> storePresents xs = do
>   store <- new []
>   forM_ xs $ \x -> do
>     old <- get store
>     put store (x : old)
>   return store
>
> ex ps = do
>   store <- storePresents ps
>   get (store :: IORef [Present])

----

*This post is part of
[24 Days of GHC Extensions](/pages/2014-12-01-24-days-of-ghc-extensions.html) -
for more posts like this, check out the
[calendar](/pages/2014-12-01-24-days-of-ghc-extensions.html)*.
