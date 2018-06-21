---
title: "24 Days of GHC Extensions: Recursive Do"
---

Ordinarily, when we're programming computations under a monad, we're limited to
forming bindings in the order they appear in a `do` block. However, there are a
class of monads that support extra *value recursion* through the type class
`MonadFix`. This extra functionality has special syntax support in GHC, and can
be useful in a variety of situations.

`MonadFix` and Recursive Bindings
---------------------------------

First of all, we'll take a look at how `MonadFix` can be used to easily create
recursive data structures. Before we get to that, lets look at a little program
that works with rose trees. We'll begin with the definition of a rose tree:

> {-# LANGUAGE RecursiveDo #-}
> import Control.Monad.Fix

> data RoseTree a = RoseTree a [RoseTree a]
>   deriving (Show)

A rose tree is a node paired up with a list of child trees. Thus this data
structure is a simple model of a multi-way tree. One thing we'd like to be able
to do with our rose trees is to pair each element in the tree with the largest
element in the *entire* tree. For example, if we have the following:

> exampleTree :: RoseTree Int
> exampleTree = RoseTree 5 [RoseTree 4 [], RoseTree 6 []]

then we can pair each item in the tree with the biggest element - 6:

```
.> pureMax exampleTree
RoseTree (5, 6) [RoseTree (4, 6) [], RoseTree (4, 6) []]
```

On the surface, we might think that this operation should take two traversals of
the tree - one to find out what the largest element is, and then another pass
through the tree to change all the elements. Interestingly, this can actually be
done in a single pass, if we exploit laziness! Here's how:

> pureMax :: Ord a => RoseTree a -> RoseTree (a, a)
> pureMax tree =
>   let (t, largest) = go largest tree
>   in t
>  where
>   go :: Ord a => a -> RoseTree a -> (RoseTree (a, a), a)
>   go biggest (RoseTree x []) = (RoseTree (x, biggest) [], x)
>   go biggest (RoseTree x xs) =
>       let sub = map (go biggest) xs
>           (xs', largests) = unzip sub
>       in (RoseTree (x, biggest) xs', max x (maximum largests))

If you've not seen this before - this may seem a little cryptic! Let's take it
slowly. We walk through our tree with the `go` function. `go` takes two
paremeters - one is the tree that we're working on, while the other is the known
largest element. But wait... don't we need to calculate the largest element?
You're right - and that's exactly what we do in `go`.

Notice that `go` returns two values - the relabelled rose tree, along with the
known largest element in that tree. The magic happens right at the start of
`pureMax` - notice that we pattern match to bind the variable `largest`, though
we also feed in `largest` to relabel the trees.

What's happening is that we're exploiting lazy evaluation - what we actually do
is relabel the tree with a thunk at every node. Once we've finished recursing
the entire tree, we've got enough information, should we need to force that
node. This technique is known as *circular programming*, and Richard Bird has a
lovely write up on a similar problem - the *repmin problem*.

So far, we've seen a pure solution to the problem, but what happens if we need
to use effectful computations to determine the largest value? For example, let's
work with an exclusive secret santa. In this exclusive secret santa, people can
invite others into the game - forming a tree of invites. Furthermore, we also
have a database lookup function that will tell us what their budget is. We'd
like to be able to relabel the invite tree with the minimum budget for
fairness. If we had a tree of budgets, that would be easy - we could just use a
variation of `pureMax` above. However, determining the budget requires a
database lookup.

It looks like we're stuck, but what we can do is make an effectful variation of
`pureMax`:

> impureMin :: (MonadFix m, Ord b) => (a -> m b) -> RoseTree a -> m (RoseTree (a, b))
> impureMin f tree = do
>   rec (t, largest) <- go largest tree
>   return t
>  where
>   go smallest (RoseTree x []) = do
>     b <- f x
>     return (RoseTree (x, smallest) [], b)
>
>   go smallest (RoseTree x xs) = do
>     sub <- mapM (go smallest) xs
>     b <- f x
>     let (xs', bs) = unzip sub
>     return (RoseTree (x, smallest) xs', min b (minimum bs))

If you compare this to `pureMax` you should notice that the programs are very
similar. Infact, all we've had to do is replace the pure `let x = y` bindings
with effectful `x <- y` bindings, and call out to our effectful
function. Finally, the magic sauce at the top is to use a new piece of syntax
`rec`. `rec` comes from the
[RecursiveDo](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#recursive-do-notation)
extension, and while the details are beyond the scope of this post (there's a
whole thesis on it!), we can see here that it serves the same purpose as forming
recursive bindings, as we did with `let`.
 
To wrap up this example, let's work with some test data and see how it all plays out:

> budget :: String -> IO Int
> budget "Ada"      = return 10 -- A struggling startup programmer
> budget "Curry"    = return 50 -- A big-earner in finance
> budget "Dijkstra" = return 20 -- Teaching is the real reward
> budget "Howard"   = return 5  -- An frugile undergraduate!

> inviteTree = RoseTree "Ada" [ RoseTree "Dijkstra" []
>                             , RoseTree "Curry" [ RoseTree "Howard" []]
>                             ]

Now we can ask our system what budget we should use

```
.> impureMin budget inviteTree
RoseTree ("Ada",5) [RoseTree ("Dijkstra",5) [],RoseTree ("Curry",5) [RoseTree ("Howard",5) []]]
```

Alright, 5 gold coins it is!

In this post I've shown just one of many things that can be done with
`RecursiveDo` syntax. Another common usage of this extension is in
`reactive-banana` - here it's possible for reactive behaviors to depend on
events, where the events sample the same behavior. A more extreme example (if
you really want a headache!) is the [`Tardis`
monad](http://hackage.haskell.org/package/tardis-0.3.0.0/docs/Control-Monad-Tardis.html)
- a monad where data can travel forwards and backwards in time!

Edits:

* [Dan Fornika](https://twitter.com/dfornika) [points out](https://twitter.com/dfornika/status/542422590313091073) that
  another cool use case of this extension is in implementing [jump labels for assembly code](http://wall.org/~lewis/2013/10/15/asm-monad.html).

----

*This post is part of
[24 Days of GHC Extensions](/pages/2014-12-01-24-days-of-ghc-extensions.html) -
for more posts like this, check out the
[calendar](/pages/2014-12-01-24-days-of-ghc-extensions.html)*.

 
