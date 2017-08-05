---
title: "24 Days of GHC Extensions: Scoped Type Variables"
---

Today I'm happy to announce that we have another guest blog post. Today,
[Tim Docker](http://twdkz.wordpress.com/) is going to give us a slighty
different perspective (different for me, at least!) on the *scoped type
variables* extension. Over to you, Tim!

---

I'm a fairly conservative haskell developer. Most of my code is close
to haskell 2010, with just the odd extension enabled here and there. I
like learning new language features, but I'm hesitant to use them
in day to day coding until I feel I understand them well.

[ScopedTypeVariables][4] is a straightforward extension that I use
often. In this post, I'll explain why.

Type inference is one of haskell's many appealing features. But
writing explicit type signatures is still important, for several
reasons:

   * Type signatures are excellent documentation

   * Type signatures are a way to confirm that the compiler's
     "understanding" of the code matches my mental model.

   * Certain type system extensions require type signatures.

It is considered good haskell style to write type signatures for all
top level definitions. And, in complex code it's often beneficial to
write signatures for key subterms. The issue is that, in haskell 2010,
explicit type signatures cannot be written for all well typed
subterms.

Hence the motivation for ghc's ScopedTypeVariables extension, as
quoted from the [original paper][1]:

> it should be possible for the programmer to write an explicit type
> signature for any sub-term of the program.

I am a lazy coder - sometimes the code just flows through my fingers
into emacs, but as function types get more complex, I have to think
harder.  Hence, I like to use the compiler as an assistant, moving
from an empty edit buffer to working code, with ghc checking each step
along the way. I'll work through a short example showing this.

For some reason, calls to `foldr` and `foldl` have never flowed easily
for me - the intuition of folding is natural enough, but the code
always needs a little thought, particularly when the folds are nested.

Consider a function where I want to insert a list of values into a
map, combining new values with existing ones. Our function will look
like this:

```haskell
import qualified Data.Map as Map

insertMany ::  Ord k => (v -> v -> v) -> [(k,v)] -> Map.Map k v -> Map.Map k v
insertMany acc vs m = undefined
```

Now, clearly this is a fold of some function over `vs`, so lets write
it as such, and get ghc to check it:

```haskell
insertMany ::  Ord k => (v -> v -> v) -> [(k,v)] -> Map.Map k v -> Map.Map k v
insertMany accf vs m = foldr f1 m vs
  where
    f1 = undefined
```

All good. So, what is the type of f1? I could read the documentation
for `foldr`, but given the above code type checks, the compiler
already knows the type.  Let's have it tell us, by forcing a type
error:

```haskell
insertMany ::  Ord k => (v -> v -> v) -> [(k,v)] -> Map.Map k v -> Map.Map k v
insertMany accf vs m = foldr f1 m vs
  where
    f1 :: Int
    f1 = undefined
```

results in the error:

```
    Couldn't match expected type ‘(k, v) -> Map.Map k v -> Map.Map k v’
            with actual type ‘Int’
Relevant bindings include
  ...
In the first argument of ‘foldr’, namel
```

Let's paste that type in:

```haskell
insertMany ::  Ord k => (v -> v -> v) -> [(k,v)] -> Map.Map k v -> Map.Map k v
insertMany accf vs m = foldr f1 m vs
  where
    f1 :: (k, v) -> Map.Map k v -> Map.Map k v
    f1 = undefined
```

Again, ghc is happy, and now we can clearly see what we need to
implement. Conveniently, Map has the `insertWith` function, which
look fit for the task:

```haskell
insertMany ::  Ord k => (v -> v -> v) -> [(k,v)] -> Map.Map k v -> Map.Map k v
insertMany accf vs m = foldr f1 m vs
  where
    f1 :: (k, v) -> Map.Map k v -> Map.Map k v
    f1 (k,v) m = Map.insertWith accf k v m
```

But now, ghc is not happy at all:

```
Could not deduce (v ~ v1)
from the context (Ord k)
  bound by the type signature for
             insertMany :: Ord k =>
                           (v -> v -> v) -> [(k, v)] -> Map.Map k v -> Map.Map k v
  at /Users/timd/annex/blog/scoped-type-variables/Test.hs:3:16-78
  ‘v’ is a rigid type variable bound by
      the type signature for
        insertMany :: Ord k =>
                      (v -> v -> v) -> [(k, v)] -> Map.Map k v -> Map.Map k v
      at /Users/timd/annex/blog/scoped-type-variables/Test.hs:3:16
  ‘v1’ is a rigid type variable bound by
       the type signature for
         f1 :: (k1, v1) -> Map.Map k1 v1 -> Map.Map k1 v1
       at /Users/timd/annex/blog/scoped-type-variables/Test.hs:6:11
Expected type: v1 -> v1 -> v1
  Actual type: v -> v -> v
Relevant bindings include
   ...
In the first argument of ‘Map.insertWith’, namely ‘accf’
In the expression: Map.insertWith accf k v m
```

Yikes! Yet it compiles if I comment out the type annotation on f1:

```haskell
insertMany ::  Ord k => (v -> v -> v) -> [(k,v)] -> Map.Map k v -> Map.Map k v
insertMany accf vs m = foldr f1 m vs
  where
--    f1 :: (k, v) -> Map.Map k v -> Map.Map k v
    f1 (k,v) m = Map.insertWith accf k v m
```

and it works too:

```
> insertMany (+) [("a",5),("b",4),("c",2),("b",11)] Map.empty
fromList [("a",5),("b",15),("c",2)]
>
```

I could delete the signature and get on with coding, but it makes
useful documentation. And why doesn't it type check anyway? The key
lines from the error message are:

```
Could not deduce (v ~ v1)
  ‘v’ is a rigid type variable bound by
      the type signature for
        insertMany :: Ord k => (v -> v -> v) -> [(k, v)] -> Map.Map k v -> Map.Map k v
 ‘v1’ is a rigid type variable bound by
       the type signature for
         f1 :: (k1, v1) -> Map.Map k1 v1 -> Map.Map k1 v1
```

Note that it has reproduced my two type signatures, but the signature
for f1 has different type variables. The compiler is telling me that,
as written, v and v1 are not the same type. This is the crux of the
problem - by writing

```haskell
f1 :: (k, v) -> Map.Map k v -> Map.Map k v
```

I have promised that this function will work for *any* types k and
v. But as I use accf within the implementation of f1, v in f1 is
constrained to match the v in insertMany.

The code type checks if I delete the type signature because haskell
2010 can *infer* the correct type for the f1. But there is no facility
to directly express this type in a signature. In haskell 2010, a type
variable is only scoped over the type signature in which it appears -
I can't directly "connect" type variables in different type
signatures.

The ScopedTypeVariables extension lets us extend the scope of type
variables into the corresponding expression, and hence to internal
type signatures. Here's the example, making use of the extension:

```haskell
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Map as Map

insertMany ::  forall k v . Ord k => (v -> v -> v) -> [(k,v)] -> Map.Map k v -> Map.Map k v
insertMany f vs m = foldr f1 m vs
  where
    f1 :: (k, v) -> Map.Map k v -> Map.Map k v
    f1 (k,v) m = Map.insertWith f k v m
```

We enable the extension with the `LANGUAGE` pragma, and then declare
that the type variables in the `insertMany` signature are to be scoped
over the corresponding expression with the `forall k v`
quantifier. Hence, `v` now means the same type wherever it appears in
the implementation of `insertMany`. And hence we have our completed
code, annotated as desired.

This is a relatively simple example where I have incrementally written
the code whilst using the compiler to ensure that, at each stage, I am
proceeding towards a correct solution.  It's longwinded to describe
the process, but in practice, with fast error checking from
[ghc-mod][2], the sequence of steps above takes only a minute or
two. As the types become more complex, I find this technique very
helpful. Using types to guide the writing of the code is sometimes
referred to as "hole-driven development".  Mathew Brecknell has
created a nice [demonstration][3] that shows the process in more
detail.

A key part of this technique is writing the type signatures before the
code. The ScopedTypeVariables extension makes it possible to write
type signatures which cannot be expressed in haskell 2010.

One thing that I didn't know until I wrote this post is that the
`forall` syntax is not the only way of introducing a scoped type
variable. It turns out there are 3 other ways - see the
[ghc documentation][4] for details.

A final note - the forall keyword is not part of haskell 2010, but is
used by several different extensions (including ScopedTypeVariables,
ExistentialQuantification, RankNTypes).  So when you see it in code,
it's worth checking which of these extensions are active.


[1]: http://research.microsoft.com/en-us/um/people/simonpj/papers/scoped-tyvars/
[2]: https://hackage.haskell.org/package/ghc-mod
[3]: http://matthew.brecknell.net/post/hole-driven-haskell/
[4]: https://downloads.haskell.org/~ghc/7.8.2/docs/html/users_guide/other-type-extensions.html#scoped-type-variables
1
----

*This post is part of
[24 Days of GHC Extensions](/pages/2014-12-01-24-days-of-ghc-extensions.html) -
for more posts like this, check out the
[calendar](/pages/2014-12-01-24-days-of-ghc-extensions.html)*.
