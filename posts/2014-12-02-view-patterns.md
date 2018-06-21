---
title: "24 Days of GHC Extensions: View Patterns"
---

I'd like to start this series by focussing on what I call *binding
extensions*. These are extensions that are used in conjuction with forming
bindings - such as top-level function definitions, `where` clauses, and `let`
bindings. Today, we'll begin by looking at a simple yet powerful extension -
*view patterns*.

[View patterns](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#view-patterns)
extend our ability to pattern match on variables by also allowing us to pattern
match on the result of function application. To take a simple example, lets work
with a `Map` from Haskell packages on Hackage, to the amount of downloads. To
start with, we'll look at extracting the amount of downloads for the `lens`
library. Ordinarily, we might write something like:

```haskell
lensDownloadsOld :: Map HaskellPackage Int -> Int
lensDownloadsOld packages =
  case M.lookup "lens" packages of
    Just n -> n
    Nothing -> 0
```

Notice that the first thing this function does is to immediately pattern match
on a function call. Arguably, this obscures the definition of the
`lensDownloads` function which we expect to have *two* equations defining it - one
when the package has a download count, and another for when the package hasn't
been download (for example, when collecting a new batch of statistics). Using
view patterns, we can move this lookup from the right-hand side to the left hand
side:

```haskell
lensDownloads :: Map HaskellPackage Int -> Int
lensDownloads (M.lookup "lens" -> Just n) = n
lensDownloads _                           = 0
```

Now our lookup function is defined by the two equations we would expect. View patterns allows us to "view" the download statistics as a different data type - in this case we view the map as the sum type `Maybe Int`, by focussing on the value for the key `lens`.

As we can see, a view pattern is defined by two parts - the view itself, which is a partially applied function; and the pattern match to perform on the result of that function application. In this case, we are given a `Map HaskellPackage Int`, and our view is `M.lookup "lens" :: Map HaskellPackage Int -> Maybe Int`. We pattern match on this `Maybe Int` for the `Just` case, and this allows us to bind the download count to the variable `n`. Notice also that if the pattern match against `Just` fails, we fall through to the next pattern of `lensDownloads`. GHC will carefully check patterns for exhaustivity, so we're still forced to consider all possibilites.

Finally, it would be tedious to have to write a function like this for *every* package - so we would like to abstract the package name out. With view patterns, our view function is able to depend on variables to the left of the view pattern. Thus we are able to write a general download-lookup function as

```haskell
downloadsFor :: HaskellPackage -> Map HaskellPackage Int -> Int
downloadsFor pkg (M.lookup pkg -> Just downloads) = downloads
downloadsFor _   _                                = 0
```

## View Patterns as a Tool for Abstraction

The functions we've seen so far haven't really benefit from view patterns. The case analysis in the original example isn't particularly cumbersome, and `downloadsFor` doesn't necessarily benefit from the use of view patterns. However, a key benefit to view patterns is that they allow us to view a data type as a definition that is easy to pattern match on, while using a very different data type for the underlying representation.

Take for example, the [finger tree](http://apfelmus.nfshost.com/articles/monoid-fingertree.html) - a general purpose data structure suitable for a wide variety of applications, one of which is as a [sequence](http://hackage.haskell.org/package/containers-0.5.5.1/docs/Data-Sequence.html). In Haskell, the `Prelude` gives us a basic list data type, defined essentially as:

```haskell
data List a = Nil | Cons a (List a)
```

However, this data structure has terrible performance for just about anything - it's just a linked list. Viewing the last element of  the list here is *O(n)* - quite a cost! `Seq` can be used as a drop in replacement to lists here, but looking up the last element is *O(1)* - much better! To give such high performance, `Seq` uses a finger tree, which is a data type which has much better performance characteristics than linked lists. To do so, `Seq` uses a more complex data definition - a definition that is completely abstract to us, forcing us to use functions to inspect it.

The use of functions moves us away from perhaps more idiomatic Haskell programming, where would like to define our functions in terms of various equations. By using view patterns, we regain much of this style of programming.

As an example, let's consider analysing a time series. Our time series is simple, and we'll store a list of data points. To operate on this time series, we'd like to be able to view the last data point in the series - if such a value exists. Intuitively, we know there are two possibilities: the time series is empty, in which case we return `Nothing`; or the time series is non-empty, in which case we return `Just` the last value:

```haskell
last :: Seq a -> Maybe a
last ?? = Nothing
last ?? = Just _
```

While we can't pattern match directly on a `Seq`, we can *view* it as a list from the right by using [`viewr`](http://hackage.haskell.org/package/containers-0.5.5.1/docs/Data-Sequence.html#v:viewr):

```haskell
data ViewR a = EmptyR | (Seq a) :> a

viewr :: Seq a -> ViewR a
```

Notice that `ViewR` is similar to a linked list as before, but we have the
ability to look at any `Seq` as a list from the right. Either the sequence is
empty, or it's a smaller sequence with a single element appended. This inductive
structure fits perfectly for our purposes:

```haskell
last :: Seq a -> Maybe a
last (viewr -> xs :> x) = Just x
last (viewr -> EmptyR) = Nothing
```

This type of separation is very powerful, and you'll find it used in many of the
high-performance data structures on Hackage.

However, one qualm with this approach is that it brings new syntax - a syntax
that it took the author a while to get comfortable with. With new syntax there
is always a balance between the overhead of the syntax (which adds something of
a context switch), and the productivity gains the extension begets. What would
be *really* nice would be similar functionality of this extension, without the
need for new syntax. Thankfully, GHC can do just that. How, you ask? Well,
you'll just have to wait and see...

----

*This post is part of
[24 Days of GHC Extensions](/pages/2014-12-01-24-days-of-ghc-extensions.html) -
for more posts like this, check out the
[calendar](/pages/2014-12-01-24-days-of-ghc-extensions.html)*.
