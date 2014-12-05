---
title: 24 Days of GHC Extensions: Bang Patterns
---

Over the last few days, we've been looking at various GHC extensions that centre
around forming bindings. Today I'd like to look at one more extension in this
area -
[bang patterns](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/bang-patterns.html). Much
like with [record wildcards yesterday](/posts/2014-12-04-record-wildcards.html),
the extension is small, yet extremely useful.

> {-# LANGUAGE BangPatterns #-}
> import Data.Function (fix)
> import Data.List (foldl')
 
Generally speaking, bang patterns allow us to annotate pattern matches to
indicate that they should be strict. To understand this, we should start by
understanding the interaction between pattern matching and Haskell's evaluation
strategy. When we are writing functions, any inputs to the function will not be
evaluated until we pattern match on them. For example, the following contrived
function doesn't pattern match on its argument, so it doesn't force any
evaluation on it:

> hello :: Bool -> String
> hello loud = "Hello."

If we apply `hello` to various arguments, the behaviour is the same - even for
`undefined` values:

```
-> hello True
"Hello".
-> hello False
"Hello".
-> hello undefined
"Hello".
-> hello (fix id)
"Hello".
```

However, by pattern matching on the `Bool`, we force evaluation of `loud`:

> hello2 :: Bool -> String
> hello2 True = "Hello!"
> hello2 False = "hello"

```
-> hello2 True
"Hello"!
-> hello2 False
"hello"
-> hello2 undefined
*** Exception: Prelude.undefined
-> hello2 (fix id)
"*** Exception: <<loop>>
```

Specifically, the pattern match will evaluate the input argument enough to
perform the pattern match - to determine which pattern is appropriate. Usually
this would be evaluation to weak head normal form, but that's not strictly true
with nested pattern matches. For more of a discussion on this, interested
readers are pointed to Simon Marlow's book
[Parallel and Concurrent Programming in Haskell](http://chimera.labs.oreilly.com/books/1230000000929),
which has a fantastic discussion on this.

But what does this all have to do with bang patterns? Bang patterns is an
extension that will evaluate specific arguments to weak head normal form
*regardless* of the pattern match performed. If we revisit our example `hello`
function, rewriting it with bang patterns, we have

> hello3 :: Bool -> String
> hello3 !loud = "Hello."

This function will now produce values only if `loud` evaluates to `True` or
`False`:


```
-> hello3 True
"Hello."
-> hello3 False
"hello."
-> hello3 undefined
*** Exception: Prelude.undefined
-> hello3 (fix id)
"*** Exception: <<loop>>
```

So much for theory, but why would you want to do such a thing? Bang patterns are a
fantastic extension when you *don't* need Haskell's implicit laziness. A common
case is when performing computations over large lists of data. If we're just
summarising a list or collection, forcing the value at every step leads to
considerably better memory usage, and that in turn leads to better
performance. [Johan Tibell](http://blog.johantibell.com/) - an expert in the
realm of high performance haskell - has a
[lovely example](http://www.slideshare.net/tibbe/highperformance-haskell) of
where bang patterns are useful, in this snippet for calculating the mean of a
list of `Double`s:

> mean :: [Double] -> Double
> mean xs = s / fromIntegral l
>   where
>     (s, l) = foldl' step (0, 0) xs
>     step (!s, !l) a = (s + a, l + 1)

Here we're finding the mean of a list of numbers. If we kept this entirely lazy,
we'll build up a huge computation - `a + b + c + d + e + ...` and `0 + 1 + 1 +
1 + 1 + ...`, for the entire length of the list! This is a horrible usage of
memory, and we don't need this laziness. It looks like using `foldl'`
should be sufficient, but note that `foldl'` only evaluates to weak head normal
form. In this case, that's the pair of `Double`s but *not* the `Double`s
themselves! Therefore we use bang patterns on `s` and `l`, forcing every step of
the computation to evaluate the underlying `Double`.

It may be illuminating to consider the desugared version of the program:

```haskell
mean :: [Double] -> Double
mean xs = s / fromIntegral l
  where
    (s, l) = foldl' step (0, 0) xs
    step (s, l) a = let s' = s + a
                        l' = l + 1
                    in s' `seq` l' `seq` (s', l')
```

This program is equivalent in strictness, but as you can see - syntactically we
had to do a lot more work to get there.

In conclusion, bang patterns are a lovely extension for working with high
performance code. I particularly like that we can indicate strictness
syntactically, which I find makes scanning through code to understand its
evaluation strategy clearer than looking for `seq`s. Also, `BangPatterns` are so
lightweight, when we are trying to optimise our program - often an inherently
experimental process - it's easy to swap out different variations on strictness.

----

*This post is part of
[24 Days of GHC Extensions](/pages/2014-12-01-24-days-of-ghc-extensions.html) -
for more posts like this, check out the
[calendar](/pages/2014-12-01-24-days-of-ghc-extensions.html)*.
