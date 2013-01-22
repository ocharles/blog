---
title: 24 Days of Hackage: dlist
---

Today we look at a single module that clocks in at a mere 200 lines of code,
including comments. In these few lines though is an optimisation that has a wide
range of use cases - an optimisation taking an operation from O(n) to
O(1). Now that's my kinda optimisation!

The [`dlist`](http://hackage.haskell.org/package/dlist) library provides an API
that allows list appending to run in O(1) rather than O(n) time. Before we look
at the magic that makes this unbelievable feat possible, let's take a quick look
at how the API works.

```haskell
      > toList $ append (fromList [1..5]) (fromList [1..10])
[1,2,3,4,5,1,2,3,4,5,6,7,8,9,10]
```

We use `fromList` to convert a Haskell list into a dlist. Next, we use the
`dlist` specific `append` to combine 2 lists. Note that `dlist` is a monoid so
we could also use the more general `<>` operator. Finally, to get back out of
`dlist` by calling `toList`. There's not much more to it!

So, now that we've seen this at work, we can ask ourselves how this works. The
key to `dlist` lies in function composition and partially applied
functions. Rather than storing complete lists, and building new lists every time
we call `append`, we instead store a *function* that given a list will the
original list prepended to the input to the function. This now lets us use
function composition to build up our final list, and function composition is
O(1). Let's see this is a bit of code:

```haskell
      > :t ([1, 2] ++)
([1, 2] ++) :: Num a => [a] -> [a]
> :t ([1, 2] ++)  . ([3, 4] ++)
([1, 2] ++)  . ([3, 4] ++) :: Num a => [a] -> [a]
> ([1, 2] ++)  . ([3, 4] ++) $ []
[1,2,3,4]
```

That's almost all there is to it. Dlist simply wraps this up in a new type and
provides instances for `Monad`, `Monoid`, and a few other type classes we'd
expect. It should that `dlist` doesn't make *everything* O(1) - at some point
you will have to construct the list, which will be at least O(n). However, for
[specific usage patterns](http://www.haskell.org/haskellwiki/Difference_list)
`dlist` can give you a good boost in performance.
