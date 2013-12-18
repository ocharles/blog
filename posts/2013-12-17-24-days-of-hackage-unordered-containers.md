---
title: 24 Days of Hackage: unordered-containers
---

[Last year](/posts/2012-12-06-containers.html) we looked at the
[`containers`](http://hackage.haskell.org/package/containers) library - a
library that provides some of the most essential data structures in
programming. While `containers` are extremely efficient, sometimes you're just
need to go the extra mile, and that's where
[`unordered-containers`](http://hackage.haskell.org/package/unordered-containers)
comes in.

If you have a look at the `insert` methods in `containers`, you'll see type
signatures like:

```haskell
insert :: Ord k => k -> a -> Map k a -> Map k a
insert :: Ord a => a -> Set a -> Set a
```

The only requirement on insertions is that we can order the keys in a map, or
the elements of a set, respectively. This gives us an indication of the
underlying algorithm - the ordering is probably exploited to form some type of
balanced tree. However, there's another way to build up these data structures,
and that's via the familiar process of
[*hashing*](https://en.wikipedia.org/wiki/Hash_function).

Hashing is available to us in Haskell via the
[`hashable`](http://hackage.haskell.org/package/hashable) library, currently
maintained by [Johan Tibell](http://blog.johantibell.com/). `hashable` gives us
a new type class - `Hashable` - which does what it says on the tin. Given some
value that has a `Hashable` instance, we can turn those values into
hashes. `unordered-containers` then builds on top of this to provide us with
hash-maps and hash-sets.

Armed with `Hashable`, working with hash-maps and hash-sets in
`unordered-containers` is a breeze. For example, we could easily store a
hash-map associating children with a list of presents they want for
Christmas. First, we define our data types:

```haskell
data Child = Child { childName :: String
                   , childLocation :: String
                   } deriving (Eq, Generic, Show)

data Priority = Please | PrettyPlease | PleasePleasePlease
  deriving (Eq, Generic)

data Request = Request { requestPresent :: String
                       , requestPriority :: Priority
                       } deriving (Eq, Generic, Show)
```

Off the bat, we can't use these data types in any maps. They don't have `Ord`
instances, and they also don't have `Hashable` instances. Thankfully, it's a
doddle to add a `Hashable` instance thanks to GHC's generic deriving
support[^1]:

[^1]: This requires `hashable >= 1.2`

```haskell
instance Hashable Child
instance Hashable Priority
instance Hashable Request
```

Now we can populate our hash-map:

```haskell
olliesWishList :: HashMap Child [Request]
olliesWishList =
  let ollie = Child { childName = "ocharles"
                    , childLocation = "London"
                    } fromList
  in [(ollie, [ Request "Artisan Coffee" Please
              , Request "Dependent Types in Haskell" PleasePleasePlease
              , Request "Lambda Fridge Magnets" PrettyPlease
              ])]
```

And query it just as you would query a map in `containers`:

```haskell
main = do
  void $ traverseWithKey showWishList olliesWishList
 where
  showWishList child wants = do
    putStrLn (childName child ++ " wants...")
    mapM_ (putStrLn . requestPresent) wants
```

Which produces the output:

```
ocharles wants...
Artisan Coffee
Dependent Types in Haskell
Lambda Fridge Magnets
```

A modest wishlist, I'm sure you'll agree.

The API provided by `unordered-containers` is not quite as extensive as what we
have available to us in `containers` - but it's still perfectly usable. The real
benefit from `unordered-containers` is, of course, in the numbers. Johan has
already done a good deal of benchmarking and comparison, and
[wrote up his findings](http://blog.johantibell.com/2012/03/announcing-unordered-containers-02.html)
on his blog, and hash-maps consistently out-perform regular `Ord`-based maps.

As to which you should use, my personal preference is still `containers` as that
API spoils me. However, sometimes these data structures are at the heart of what
I'm building and entirely internal - in these cases I'm happy to know that a
`Hashable` instance exists and I might as well make use of it! Another
consideration is that `containers` requires `Ord` instances, and sometimes
ordering just doesn't make sense for the data I'm working with. In these cases,
I can usually write a `Hashable` instance, so in that situation the choice is
clear.
