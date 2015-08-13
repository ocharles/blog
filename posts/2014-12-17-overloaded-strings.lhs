---
title: 24 Days of GHC Extensions: Overloaded Strings
---

Today we'll take a look at a handy extension that allows us to *redefine* the meaning of literals in Haskell source code. Ordinarily, the string literal has a specific type:

```
.> :t "Hello, readers!"
"Hello, readers!" :: [Char]
```
 
However, this is in constrast to some of the other literals that we can write in GHC source code. For example, if we write an integer literal

```
.> :t 42
42 :: Num a => a
```

then we write a literal that is polymorphic over *all* `Num` instances. Likewise, a literal floating point number

```
.> :t 3.142
3.142 :: Fractional a => a
```

is polymorphic over all `Fractional` instances.

This polymorphism is extremely powerful, and it allows us to write embedded domain specific languages in Haskell source code, without having to introduce new constructs for otherwise normal values. So why should string literals be any different?

The good news is - they don't have to be! If we enable the [overloaded strings](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/type-class-extensions.html#overloaded-strings), then string literals get a different type:

```
.> :set -XOverloadedStrings
.> :t "Oh, what's this?"
"Oh, what's this?" :: Data.String.IsString a => a
```

By enabling this extension, string literals are now a call to the `fromString` function, which belongs to the `IsString` type class.

You'll find instances of this all over Haskell, making this one of the most common extensions for day to day programmers. There are obvious instances, like `IsString Text`, but there are also some interesting uses.

One interesting use is in the [`postgresql-simple`](http://hackage.haskell.org/package/postgresql-simple) library. In this library, we are able to interact with the PostgreSQL relational database by writing SQL queries. However, SQL queries are notorious for injection attacks when we concatenate strings. Interestingly, `postgresql-simple` provides a `Query` type that *only* has a `IsString` instance. This means that it's very lightweight to write a literal query, but the moment we want to start concatenating strings for our query, we have to be very explicit. This is a very non-intrusive feature, but I feel it's a nice touch that helps remind other developers about the risks they might be walking in to.

----

*This post is part of
[24 Days of GHC Extensions](/pages/2014-12-01-24-days-of-ghc-extensions.html) -
for more posts like this, check out the
[calendar](/pages/2014-12-01-24-days-of-ghc-extensions.html)*.
