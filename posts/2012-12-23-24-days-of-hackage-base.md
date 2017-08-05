---
title: "24 Days of Hackage: base"
---

As 24 Days of Hackage comes to a close, I see no better library to review than
`base` itself. [`base`](http://hackage.haskell.org/package/base) is a library
that all Haskell users will be familiar with, as it defines the `Prelude`, along
with a variety of other useful modules. `base` could be considered to be a
bare-bones standard library for Haskell, and while the `Prelude` is fairly
extensive, there's actually a lot of other useful functionality that is
provided.

Some of my favourite constructs lie in the `Control.Concurrent`
namespace. Concurrency is a pretty hard problem, but provided with good tools it
doesn't have to be a painful experience. The `Control.Concurrent.Chan` module
provides "unbounded" channels, which give you a tiny little message queue that
runs in memory.

```haskell
echo :: Chan String -> IO ()
echo c = forever $ readChan c >>= putStrLn

main :: IO ()
main = do
  c <- newChan
  forkIO (echo c)
  forever $ getLine >>= writeChan c
```

In `main` we create a new channel, then fork a separate thread to consume
messages from this channel. Finally, in the original thread we write messages
into the channel from user input. This gives us a concurrent application, with two
threads communicating almost transparently.

Concurrency is not all `base` has to offer, and recently I've been increasingly
intrested in `Data.Monoid`, after reading Brent Yorgey's excellent
[Monoids: Themes and Variations](http://www.cis.upenn.edu/~byorgey/pub/monoid-pearl.pdf)
paper. Monoids aren't particularly complicated beasts, they just have an "empty"
element, and an operation to combine two values into one larger value. This can
be really nice when combined with tuples, as you can have some really expressive
transformations. For example, we can use a variety of monoids to find the sum
and product of a list of integers:

```haskell
stats :: [Int] -> (Sum Int, Product Int)
stats = mconcat . map (\x -> (Sum x, Product x))
```

After a while, this has started to feel like a really natural way to aggregate
data. Combined with
[`semigroups`](http://hackage.haskell.org/package/semigroups), you can be
extremely expressive in very little typing.

If you've been following these blog posts, then it'll be no surprise that I love
`Control.Applicative`, but `Control.Monad` and `Data.Foldable` are also great
modules. They provide a few more abstractions, and a whole bunch of combinators
which can make working with these common data structures an absolute
breeze. Combinators like `mapM`, `forever`, `when` and `unless` frequently
appear in my code, and the combinatros in `Data.Traversable` are extremely
handy. I commonly have `Maybe` values that I want to apply an IO action to:

```haskell
lookupEnv "HOME" >>= traverse putStrLn
```

Of course, `traverse` is a lot more powerful than this - you only need to look
at recent papers, or the `lens` library to see what I mean.

All in all, `base` is fairly spartan, but still provides a lot of power. It's
not quite the Python standard library, but that was never it's aim - we have the
Haskell platform for that!
