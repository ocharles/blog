---
title: "24 Days of GHC Extensions: Type Families"
---
 
Today, we're going to look at an extension that radically alters the behavior of
GHC Haskell by extending what we can do with types. The extension that we're
looking at is known as [type
families](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/type-families.html),
and it has a wide variety of applications.

> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE TypeFamilies #-}
> import Control.Concurrent.STM
> import Control.Concurrent.MVar
> import Data.Foldable (forM_)
> import Data.IORef

As the extension is so large, we're only going to touch the surface of the
capabilities - though this extension is well documented, so there's plenty of
extra reading for those who are interested!

Associated Types
----------------

To begin, lets look at the interaction of type families and type classes. In
ordinary Haskell, a type class can associate a set of *methods* with a type. The
type families extension will now allow us to associate *types* with a type.

As an example, lets try and abstract over the various mutable stores that we
have available in Haskell. In the `IO` monad, we can use `IORef`s and `MVar`s to
store data, whereas other monads have their own specific stores, as we'll soon
see. To begin with, we'll start with a class over the different types of store:

> class IOStore store where
>   newIO :: a -> IO (store a)
>   getIO :: store a -> IO a
>   putIO :: store a -> a -> IO ()

This works fine for `IO` stores: we can add an instance for `MVar`...

> instance IOStore MVar where
>   newIO = newMVar
>   getIO = readMVar
>   putIO mvar a = modifyMVar_ mvar (return . const a)

and an instance for `IORef`:

> instance IOStore IORef where
>   newIO = newIORef
>   getIO = readIORef
>   putIO ioref a = modifyIORef ioref (const a)

Now we have the ability to write functions that are polymorphic over stores:

> type Present = String
> storePresentsIO :: IOStore store => [Present] -> IO (store [Present])
> storePresentsIO xs = do
>   store <- newIO []
>   forM_ xs $ \x -> do
>     old <- getIO store
>     putIO store (x : old)
>   return store

While this example is obviously contrived, hopefully you can see how we are able
to interact with a memory store without choosing *which* store we are commiting
to. We can use this by choosing the type we need, as the following GHCI session
illustrates:

```
.> s <- storePresentsIO ["Category Theory Books"] :: IO (IORef [Present])
.> :t s
s :: IORef [Present]
.> get s
["Category Theory Books"]
```

Cool - now we can go and extend this to `TVar` and other `STM` cells!
Ack... there is a problem. Reviewing our `IOStore` type class, we can see that
we've commited to working in the `IO` monad - and that's a shame. What we'd like
to be able to do is associate the type of monad with the type of store we're
using - as knowing the store tells us the monad that we have to work in.

To use type families, we use the `type` keyword within the `class` definition,
and specify the *kind* of the type:

> class Store store where
>   type StoreMonad store :: * -> *
>   new :: a -> (StoreMonad store) (store a)
>   get :: store a -> (StoreMonad store) a
>   put :: store a -> a -> (StoreMonad store) ()

As you can see, the types of the methods in the type class has become a little
more complicated. Rather than working in the `IO` monad, we calculate the monad
by using the `StoreMonad` type family.
 
The instances are similar to what we saw before, but we also have to provide the
necessary type of monad:

> instance Store IORef where
>   type StoreMonad IORef = IO
>   new = newIORef
>   get = readIORef
>   put ioref a = modifyIORef ioref (const a)
>
> instance Store TVar where
>   type StoreMonad TVar = STM
>   new = newTVar
>   get = readTVar
>   put ioref a = modifyTVar ioref (const a)

As you can see - our methods don't need to change at all; type families
naturally extend the existing type class functionality. Our original
`storePresentsIO` can now be made to work in any monad, with only a change to
the type:

> storePresents :: (Store store, Monad (StoreMonad store))
>               => [Present] -> (StoreMonad store) (store [Present])
> storePresents xs = do
>   store <- new []
>   forM_ xs $ \x -> do
>     old <- get store
>     put store (x : old)
>   return store

As we have an instance for `Store TVar`, we can now use this directly in an
`STM` transaction:

````
.> atomically (do (storePresents ["Distributed Computing Through Combinatorial Topology"]
                     :: STM (TVar [Present])) >>= get)
["Distributed Computing Through Combinatorial Topology"]
````

Awesome!
 
Type Families and Computation
-----------------------------

What we've seen so far is extremely useful, but the fun needn't stop there! Type
families also give us the ability to compute over types! Traditionally, Haskell
is built around value level computation - running programs should do
something. That said, we all know how useful it is to have functions - so why
can't we have them at the type level? Well, now that we have the ability to
associate types with types, we can!

To look at this new functionality (closed type families), we need a few more
extensions to really unlock the potential here, so I'll finish this blog post on
that cliff hanger. Watch this space!

----

*This post is part of
[24 Days of GHC Extensions](/pages/2014-12-01-24-days-of-ghc-extensions.html) -
for more posts like this, check out the
[calendar](/pages/2014-12-01-24-days-of-ghc-extensions.html)*.
