---
title: "24 Days of GHC Extensions: Multi-parameter Type Classes"
---

Over the last few days, we've looked at a few extensions that can extend the notion of type classes in Haskell. First, we saw that [nullary type classes](/posts/2014-12-10-nullary-type-classes.html) remove the requirement that a type class varies over a single type by allowing it mention no types at all, and yesterday we saw how [type families](/posts/2014-12-12-type-families.html) can be used to associate more types against a single type. Today, we're going to revisit yesterdays example and use the [multi-parameter type classes](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/type-class-extensions.html) extension.
 
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> import Control.Monad.Trans.Class (lift)
> import Control.Monad.Trans.Reader (ReaderT)
> import Data.Foldable (forM_)
> import Data.IORef

The extension does just what it says on the tin - with `MultiParamTypeClasses` enabled, GHC removes the constraint that a type class can mention only a single type. Now, we're able to have our type class mention multiple types at once. Lifting this constraint has significant consequences; if we think of a type class over one type as modelling a set of types, whereas multiple types now let us model *relations* between types. The latter is interesting, though beyond the scope of this article. Interested readers are pointed to [Oleg Kiselyov's](http://okmij.org/ftp/) home page - which is full of mind bending tricks with type classes!
 
Yesterday, we looked at a traditional example around type classes - modelling the class of types that represent mutable variables. We used type families to associate the type of monad with each mutable variable, reaching the following API:

```haskell
class Store store where
  type StoreMonad store :: * -> *
  new :: a -> (StoreMonad store) (store a)
  get :: store a -> (StoreMonad store) a
  put :: store a -> a -> (StoreMonad store) ()
```
 
However, the API that we ended at is a little obtuse - those types take quite a bit of mental parsing to understand. Conceptually, we can think of mutable variables as having a relationship between types - the type of a mutable variable is related to the type of its monad. Using `MultiParamTypeClasses`, we can encode just this idea - we simply vary the type class over both the variable type *and* its monad:

> class Store store m where
>  new :: a -> m (store a)
>  get :: store a -> m a
>  put :: store a -> a -> m ()

This API is much easier to understand! Furthermore, because the type class itself mentions the type of monad, using this type in our programs is straightforward. We can port over yesterdays example with only changes to the type:

> type Present = String
> storePresents :: (Store store m, Monad m) => [Present] -> m (store [Present])
> storePresents xs = do
>   store <- new []
>   forM_ xs $ \x -> do
>     old <- get store
>     put store (x : old)
>   return store

I'm sure you'll agree, that's a much more manageable type. All that is left now is to provide instances for our type class:

> instance Store IORef IO where
>   new = newIORef
>   get = readIORef
>   put ioref a = modifyIORef ioref (const a)

Again, very little has changed from yesterdays code here - we just move the type of monad up to the type class instance declaration, rather than using an associated type.

So far I've put the extension in a great light, but there is a caveat: the use of multi-parameter type classes can lead to ambiguity during type checking. This can be a huge problem when writing large applications, as it means we now have to annotate our programs extensively.

To look at this problem in more detail, let's look at using the `storePresents` function we wrote earlier. If we build a store out of a list of `Present`s as an `IORef` and then query for the contents of the `IORef`, something perculiar seems to happen:

> ex ps = do
>   store <- storePresents ps
>   get (store :: IORef [Present])

What would you expect the type of this function to be? We've chosen `IORef` as our store, and `IORef`s are associated with the `IO` monad, so we have `ex :: [Present] -> IO [Present]`, right? Let's see what GHCI makes of it:

```
.> :t ex
ex :: (Store IORef m, Monad m) => [Present] -> m [Present]
```
 
That's odd! GHCI clearly knows that the variable type itself is an `IORef`, but
that's *not* enough information to determine type of monad. For example, another
equally valid definition of `Store IORef` would be:

> instance Store IORef (ReaderT () IO) where
>   new = lift . newIORef
>   get = lift . readIORef
>   put ioref a = lift (modifyIORef ioref (const a))

The problem we're encountering is that multi-parameter type classes don't add
any information to the type inference engine - because knowing one type doesn't
let you know anything about the other types. However, we needn't abandon hope
here - this problem can be solved, it just needs another extension (oh, of
course!).

----

*This post is part of
[24 Days of GHC Extensions](/pages/2014-12-01-24-days-of-ghc-extensions.html) -
for more posts like this, check out the
[calendar](/pages/2014-12-01-24-days-of-ghc-extensions.html)*.
