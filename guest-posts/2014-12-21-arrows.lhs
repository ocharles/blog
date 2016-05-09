---
title: 24 Days of GHC Extensions: Arrows
---
 
Today's guest post comes from [Tom Ellis](http://web.jaguarpaw.co.uk/~tom/blog/). If you haven't heard, Tom released a novel library for interacting with relational databases this year - [Opaleye](http://hackage.haskell.org/package/opaleye). While similar to HaskellDB in some respects, Opaleye is distinct in its extensive use of arrows in order to guarantee safety of queries. In this post, Tom's going to guide us through GHC's special syntax support for the `Arrow` type class.

> {-# LANGUAGE Arrows #-}
>
> import Control.Monad (guard)
> import Control.Monad.Identity (Identity, runIdentity)
> import Control.Arrow (returnA, Kleisli(Kleisli), runKleisli)

In Haskell we use the `Monad` typeclass to provide an interface for
encoding certain forms of computation.  The `Arrow` typeclass provides
an interface that is in many ways similar.  Everything that can be
made an instance of `Monad` (in a law-abiding way) can also be adapted
to be an `Arrow` (in a law-abiding way).  This means that the `Arrow`
interface is less powerful than `Monad`, but that more things can be
made instances of `Arrow`.

Working directly with `Monad` combinators like `>>=` and `>>` can
sometimes be awkward so Haskell provides "`do` notation" as an
alternative way of writing monad expressions in a way that can often
be clearer.  Likewise there is an "arrow notation" for writing arrow
expressions.  It is not enabled in GHC by default but can be turned on
with the `Arrows` language extension.

The subject of this blog post is how to understand and use arrow
notation.  It will also serve as an introduction to the `Arrow`
typeclass whilst keeping mention of the arrow combinators to a
minimum.  We will use develop our intuition for arrow computation
rather than learn any technical and formal definitions!

Let's kick off by refreshing our memory of `do` notation.  A very
basic way to think of `do` notation is that it is similar to a
sequence of let bindings.  For example, a basic Haskell expression to
perform a numerical calculation might be

> f :: Int -> (Int, Int)
> f = \x ->
>   let y  = 2 * x
>       z1 = y + 3
>       z2 = y - 5
>   in (z1, z2)
>
> -- ghci> f 10
> -- (23, 15)

`do` notation supports expressing the exact same computation inside
the `Identity` monad, that is, a monad that has no "side effects".

> fM :: Int -> Identity (Int, Int)
> fM = \x -> do
>   y  <- return (2 * x)
>   z1 <- return (y + 3)
>   z2 <- return (y - 5)
>   return (z1, z2)
>
> -- ghci> runIdentity (fM 10)
> -- (23,15)

The `let` bindings in `f` become `<-` under `do` notation. (For
technical reasons we have to wrap every intermediate expression in
`return` to lift them into the `Identity` monad.)  Arrow notation
supports a similar translation:

> fA :: Int -> (Int, Int)
> fA = proc x -> do
>   y  <- (2 *) -< x
>   z1 <- (+ 3) -< y
>   z2 <- (subtract 5) -< y
>   returnA -< (z1, z2)
>
> -- ghci> fA 10
> -- (23,15)

In arrow notation `proc` plays the part of "lambda", i.e. backslash,
`\`, `<-` plays the part of `=` and `-<` feeds an argument into a
"function".  We use `returnA` instead of `return`.

The benefit of `do` notation comes when we want to encode a
computation that can't be written using pure `let` bindings alone.
Here's an example that uses the list monad to generate all coordinates
within a given radius of the origin:

> range :: Int -> [Int]
> range r = [-r..r]
>
> cM :: Int -> [(Int, Int)]
> cM = \r -> do
>   x <- range 5
>   y <- range 5
>   guard (x*x + y*y <= r*r)
>   return (x, y)
>
> -- ghci> take 10 (cM 5)
> -- [(-5,0),(-4,-3),(-4,-2),(-4,-1),(-4,0),(-4,1),(-4,2),(-4,3),(-3,-4),(-3,-3)]

We read this as

* for each `x` in -5 to 5
* for each `y` in -5 to 5
* where `x*x + y*y <= r*r`
* return the pair of `x` and `y`

Now let's see how to use arrow notation to express the same
computation.  For trivial technical reasons we need to wrap the list
monad to make it suitable for use with arrow notation.  The wrapping
and unwrapping don't actually do anything except shuffle some type
parameters around.  In arrow computations we will use `K [] a b` where
instead of `a -> [b]`.  We'll use abbreviated versions of the relevant
wrapping and unwrapping functions:

> type K = Kleisli
>
> k :: (a -> m b) -> Kleisli m a b
> k = Kleisli
>
> runK :: Kleisli m a b -> (a -> m b)
> runK = runKleisli

Then we can use arrow notation to implement the radius list
computation as follows:

> cA :: Kleisli [] Int (Int, Int)
> cA = proc r -> do
>   x <- k range -< 5
>   y <- k range -< 5
>   k guard -< (x*x + y*y <= r*r)
>   returnA -< (x, y)
>
> -- ghci> take 10 (runK cA 5)
> -- [(-5,0),(-4,-3),(-4,-2),(-4,-1),(-4,0),(-4,1),(-4,2),(-4,3),(-3,-4),(-3,-3)]

What's the point of arrow notation?  So far we have only seen that it
is able to replicate some examples in `do` notation.  Well, the point
is that arrow notation forbids some computations that `do` notation
allows.  In particular all "arrow actions" must be "statically"
known".  That sentence was a mouthful!  What does it mean?  I am
calling the expression that comes between `<-` and `-<` in a row of
arrow notation the "arrow action".  "Statically known" means that if
we have a couple of rows of arrow notation

> --    y <- action1 -< x
> --    z <- action2 -< y

then the expression `action2` cannot depend on `x` or indeed anything
bound on the left hand side of an arrow notation row.

This restriction has important practical consequences.  For example,
our Haskell IO system might be based on the following primitives

> getLineM :: String -> IO String
> getLineM prompt = do
>   print prompt
>   getLine
>
> printM :: String -> IO ()
> printM = print
>
> writeFileM :: FilePath -> String -> IO ()
> writeFileM (filePath, string) = writeFile filePath string

then we could use `do` notation to write a procedure which reads a
line of user input and either prints something out or writes to a file
based on that input.

> procedureM :: String -> IO ()
> procedureM = \prompt -> do
>   input <- getLineM prompt
>   if input == "Hello"
>     then printM "You said 'Hello'"
>     else writeFileM ("/tmp/output", "The user said '" ++ input ++ "'")
>
> -- ghci> procedureM "Say something"
> -- "Say something"
> -- Hello
> -- "You said 'Hello'"
>
> -- ghci> procedureM "Say something"
> -- "Say something"
> -- Bye bye
> -- (Writes to /tmp/output)

However, there is no way to express this in arrow notation using only
the same primitives.

> -- procedureA :: K IO String ()
> -- procedureA = proc prompt -> do
> --  input <- k getLineM -< prompt
> --  if input == "Hello"
> --   then printM
> --   else writeFileM "/tmp/output" ...
>
> -- Oh no!  This won't work because we were trying to refer to a <-
> -- bound variable on the left hand side of a -<

Why do we want to use arrows when they have these restrictions?  Going
into details would take a whole other blog post of its own, but I will
mention briefly a few places where the full generality of monads is
too much.

Firstly, an arrow-only interface can often allow you to take advantage
of opmizations that a monadic interface could not. For example parsers
written using parser combinators can be made more memory efficient if
we know statically the parsing action they are going to perform.
Similarly it can help to reduce the chance of memory leaks in
functional reactive programming (e.g. with `netwire`) if actions
cannot depend in an unrestrained way on the result of previous
actions.

In embedded domain specific languages this forcing non-dependence can
make code generation more easily match a target language.  For example
in the SQL-generating relational query language Opaleye queries are
built up from arrows called `QueryArr`.  Using an arrow rather than a
monad allows the semantics of the domain specific language to more
closely match the semantics of the underlying SQL language.

So in summary, arrows are a Haskell abstraction, similar to monads,
and arrow notation is a way of writing arrow expressions which is
similar to to `do` notation.  However, there are some restrictions on
what you can do with arrows that are not shared by monads.  The
benefit of the restriction is you can often receive a performance
benefit or use your more specific knowledge about the structure of an
arrow computation to your advantage.

----

*This post is part of
[24 Days of GHC Extensions](/pages/2014-12-01-24-days-of-ghc-extensions.html) -
for more posts like this, check out the
[calendar](/pages/2014-12-01-24-days-of-ghc-extensions.html)*.
