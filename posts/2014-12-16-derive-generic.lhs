---
title: "24 Days of GHC Extensions: DeriveGeneric"
---

[Yesterday](/guest-posts/2014-12-15-deriving.html), Andraz showed us a variety of extensions that came with GHC to help us avoid writing boilerplate code. We saw that GHC can automatically derive instances for `Functor`, `Traversable`, `Foldable`, along with the usual class of `Eq`, `Ord`, `Show`, etc. However, as exciting as this is, you might have been left a little worried that this is where it stops.

All of the classes that mentioned so far exist in the `base` library, and that allows us to extend `GHC` to automatically derive code for these classes. But what if you have a type class that's not in base? It would be disappointing if there wasn't a way to avoid boilerplate without extending the compiler, and with GHC 7.2, we got a new extension that helps solve exactly this problem.

> {-# LANGUAGE DeriveGeneric #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FunctionalDependencies #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE ViewPatterns #-}
> import GHC.Generics

The (relatively) new [`DeriveGeneric`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/generic-programming.html) extension allows us to use a paradigm of programming called *data-type generic programming*. In this style of programming, we are able to write our functions over arbitrary data types - provided they have the right "shape". To get an idea of what it means for things to have the same shape, let's start by looking at the humble `Either` data type:

```haskell
data Either a b = Left a | Right b
```
 
Another data type that's similar could be a validation data type:

> data Valid e a = Error e | OK a
>   deriving (Generic)

In fact, this data type is more than just similar - we can say it is *isomorphic* to `Either`. In this sense, isomorphic is just a fancy word for a pair of functions to translate between `Valid` and `Either`, without losing information. This ideas relates to have a *structure-preserving mapping* between `Valid` and `Either`. If that sounds scary, we can easily write this up in code:

> toEither :: Valid e a -> Either e a
> toEither (Error e) = Left e
> toEither (OK a) = Right a
 
> fromEither :: Either e a -> Valid e a
> fromEither (Left e) = Error e
> fromEither (Right a) = OK a
 
See - easy!

As you can imagine, there are lots of different data types that are isomorphic, and the insight behind data-type generic programming is that (most) data-types can be built up out of simpler pieces. For `Either` and `Valid`, they are both built out of the same parts:

1. Each data-type has two constructors
2. Each constructor has one field
 
[`GHC.Generics`](http://hackage.haskell.org/package/base-4.7.0.1/docs/GHC-Generics.html) is the library behind the `DeriveGeneric` extension, and it gives us the following pieces to build data types:

* Fields
* Type parameterized fields
* Field *products* - which allow us to make a constructor with multiple fields
* Constructor *sums* - which allow a data-type to have multiple constructors

The library also goes a little further than this, by providing you with program specific information, such as the name of types and fields. The latter can be useful for working with serializers such as JSON and XML.
 
So far, we've skimmed the idea of isomorphic types, and also that GHC.Generics gives us a set of basic parts to build types. It would be tedious if we had to write conversion functions ourselves, and by using `DeriveGeneric`, GHC will do all the heavy lifting behind the scenes for us.

As you can see above, we used `deriving (Generic)` on `Valid`, which means we already get some transformations that we can play with:

```
.> from (Error "ENOCHRISTMASPRESENTS")
M1 {unM1 = L1 (M1 {unM1 = M1 {unM1 = K1 {unK1 = "ENOCHRISTMAS"}}})}
 
.> from (OK ["Books", "Calculators"])
M1 {unM1 = R1 (M1 {unM1 = M1 {unM1 = K1 {unK1 = ["Books","Calculators"]}}})}
```

While the output is a little dense, notice how the contents of the data type is present in the `K1` data type, and we choose a side of the `Valid` data type with `L1` ("left" for `Error`) or `R1` ("right" for `OK`).
 
Now that we've got a generic representation, we're able to start writing generic functions over data-type that conforms to the shape we want. As a small example, let's write a generic function to try and get the error out of an error-containing data type. As we have a large amount of different types (`M1`, `L1`, etc), we use a variety of type classes to navigate the data structure:

> class GetError rep e | rep -> e where
>   getError' :: rep a -> Maybe e

> instance GetError f e => GetError (M1 i c f) e where
>   getError' (M1 m1) = getError' m1
 
> instance GetError l e => GetError (l :+: r) e where
>   getError' (L1 l) = getError' l
>   getError' (R1 _) = Nothing
 
> instance GetError (K1 i e) e where
>   getError' (K1 e) = Just e

> getError :: (Generic (errorLike e a), GetError (Rep (errorLike e a)) e) => errorLike e a -> Maybe e
> getError = getError' . from

A little daunting, but I'll be mean and leave it as an exercise to the reader to determine how this code works. However, just to prove it *does* work - let's have a play in GHC:

```
.> getError (Error "Oh no!")
Just "Oh no!"
.> getError (OK "Phew")
Nothing
.> getError (Left "More explosions!")
Just "More explosions!"
.> getError (Right "Oh, false alarm")
Nothing
```

Now that's my idea of some Christmas magic.

Undoubtedly, generic programming is not a simple concept, and it will take time
to get used to it if you're new to it. [Andres Löh](http://www.kosmikus.org/), a firm supporter of generic
programming, has a lovely [Skills Matter
talk](https://skillsmatter.com/skillscasts/3932-a-haskell-lecture-with-leading-expert-andres-loh)
that goes into more detail about this very extension.
 
----

*This post is part of
[24 Days of GHC Extensions](/pages/2014-12-01-24-days-of-ghc-extensions.html) -
for more posts like this, check out the
[calendar](/pages/2014-12-01-24-days-of-ghc-extensions.html)*.
