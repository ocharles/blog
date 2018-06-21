---
title: "24 Days of GHC Extensions: Rebindable Syntax"
---

As the first week of 24 Days of GHC Extensions (or as I like to call it, 24
DOGE) comes to an end, it's with great pleasure that I give the stage to our
first guest poster - [Benjamin Kovach](http://kovach.me/). Ben has two posts
lined up for us, so without further ado, lets look at his first extension.

---

```haskell
{-# LANGUAGE RebindableSyntax, NoMonomorphismRestriction #-}

import Prelude hiding ((>>), (>>=), return)
import Data.Monoid
import Control.Monad ((<=<))
import Data.Map as M
```

Today we'll be talking about
[`RebindableSyntax`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#rebindable-syntax). `RebindableSyntax`
allows you to rewrite your own versions of operators in the Prelude (with
different types, even!). At first glance, this doesn't seem incredibly useful:
Why would we want to rebind operators when we can just define new functions or
operators to handle the functionality we want? Well, there are a couple of cases
where overloading an operator *drastically* changes how you can write code. In
particular, we'll be overloading `>>` and `return` in order to make `do`
notation `do` -- basically -- whatever we want it to!

Recall that:

```haskell
do x <- a  ==  a >>= \x -> g x
   g x
```

and:

```haskell
do x  == x >> y
   y
```

As a first example of how we can leverage `RebindableSyntax`, let's say you want to sum up some numbers. You could write a chain of `+`s, but why would you do that when you can leverage `do` notation and just write out numbers instead?

```haskell
addNumbers = do
  80
  60
  10
  where (>>) = (+)
```

I'm joking, of course, but this produces exactly what you'd expect: `150`.

## Monoids

If you've been using Haskell for long enough, or have some experience with abstract algebra, you'll know that Integers form a `Monoid` using 0 as the identity element and addition as the binary operator. Why not generalize the above, using `Data.Monoid`'s `Sum` data type. For the next couple of examples, we'll use the following top-level bindings:

```haskell
(>>) = mappend
return = mempty
```

We can perform the same computation as above using the `Sum` wrapper:

```haskell
someSum :: Sum Int
someSum = do
    Sum 80
    Sum 60
    Sum 10
    return
```

We're explicitly using `return` just to illustrate that we can use it as the identity in the same way we use it as the identity in monads, but it's optional in this case. We can also use the `Product` wrapper to multiply elements in sequence:

```haskell
someProduct :: Product Int
someProduct = do
    Product 10
    Product 30
```

Why not try something non-numeric?

```haskell
tummyMuscle :: String
tummyMuscle = do
    "a"
    "b"
```

Cool, we can use `do` notation now to handle monoidal computations! What else can we do?

## Composition

If you're coming from an imperative programming language, you might be wondering if we can apply a bunch of functions in sequence. Well, sure we can! Using flipped composition as `>>`, we can apply functions to an input in sequence and output the result. For the next few examples, we'll use the following bindings:

```haskell
(>>)    = flip (.)
return  = id
```

```haskell
arithmetic = do
    (+1)
    (*100)
    (/300)
    return
```

Here, the input is numeric and all functions operate on a number. What if we want to take a list and output a string? No problem:

```haskell
check = do
    sum
    sqrt
    floor
    show
```

As long as the domain/range pairs match up in the usual way, we have no problem. So now we can compose normal functions, that's cool! But what if we want to compose -- say -- `Kleisli` arrows (functions of the form `Monad m => a -> m b`)? We can do this as well, why not?

```haskell
(>>) = (<=<)
```

```haskell
kleisliExample :: (Ord a, Ord b) => Map b c -> Map a b -> a -> Maybe c
kleisliExample mp1 mp2 = do
  flip M.lookup mp1
  flip M.lookup mp2
```

Another thing you might be wondering is if we can replace `>>=` with `=>>`, and `return` with `extract` (from `Control.Comonad`) and get some meaningful expression from it. The short answer is: Not really. If you think about the following equivalence:

```haskell
do x <- a  ==  a >>= \x -> g x
   g x
```

Recall that `(=>>) :: Comonad w => w a -> (w a -> b) -> w b`. If we replace `>>=` with `=>>`, we get:

```haskell
a =>> \x -> g x
```

See the problem? The `x` we're "extracting" is just the `a` we passed in. So `do{ x <- a; g x }` is exactly equivalent to `g a`, which isn't very useful.

## Forcing the Monad

It's a relatively common problem in Haskell to *think* you have a `Monad` instance for some data type, but in reality, additional constraints make this impossible. A good example is `Set` from `Data.Set`.

One might expect that `Set`s admit a monad instance given that `[]` does -- we want to be able to, for example, write this:

```haskell
import Data.Set as S

main = print $ do
	x <- S.fromList [1, 2, 3]
	y <- S.fromList [4, 5, 6]
	return (x * y)
```

...but that doesn't work because `Set`s require `Ord` constrained elements. However, we can write a function

```haskell
setBind :: Ord b => Set a -> (a -> Set b) -> Set b
setBind s f = S.foldr S.union S.empty (S.map f s)
```

...which obeys the monad laws as long as `b` is orderable. Using `RebindableSyntax`, we can use this as `>>=` and pretend that `Set` is a real monad.

```haskell
main = print $ do
    x <- S.fromList [1, 2, 3]
    y <- S.fromList [4, 5, 6]
    return (x * y)
    where (>>=) = setBind
          return = S.singleton
```

Another useful feature of `RebindableSyntax` is that it allows "extended" monads to be used as if they were just plain old monads. For instance, [Indexed Monads](http://hackage.haskell.org/package/category-extras-0.53.1/docs/Control-Monad-Indexed.html#t:IxMonad) come equipped with a `>>>=` operator and an `ireturn` function which behave -- as one might imagine -- similarly to `>>=` and `return`. Using `RebindableSyntax` we can bind the normal monadic functions to these and have nice readable code that performs functions outside the scope of regular monads. To talk about this at a deeper level is beyond the scope of this post, but if you're interested, check out Ollie Charles's blog post on [using indexed free monads to quickcheck JSON](https://ocharles.org.uk/blog/posts/2013-11-24-using-indexed-free-monads-to-quickcheck-json.html).

Finally, I wanted to shamelessly plug my own work and mention my drum machine language [`Bang`](https://github.com/5outh/Bang), which uses two separate operators to compose beats sequentially (`<>`) and concurrently (`><`). Using these operators as `>>`, we can compose drum machine patterns in a readable way using `do` notation (to run this example you'll have to `cabal install bang`):

```haskell
music1, music2 :: Music Dur PercussionSound
music1 = do
  m4 bd bd bd bd
  hc
  m4 bd qr bd qr >< m4 hc hc hc hc
  where (>>) = (<>)

music2 = quintuplets $ 5 #> do
    bd
    cc
    where (>>) = (><)

music :: IO ()
music = bang $ 4 #> do
    music1
    music2
    where (>>) = (<>)
```

This will play `music1`'s rows sequentially, `music2`s rows concurrently, and finally play `music1`, then `music2`, sequentially, in `music`.

----

*This post is part of
[24 Days of GHC Extensions](/pages/2014-12-01-24-days-of-ghc-extensions.html) -
for more posts like this, check out the
[calendar](/pages/2014-12-01-24-days-of-ghc-extensions.html)*.
