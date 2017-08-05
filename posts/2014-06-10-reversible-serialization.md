---
title: "A Category for Correct-By-Construction Serializers and Deserializers"
---

Frequently in computer programming we need to work with data in different
representations, and we need to work with the data on both sides of said
representation. For example, we might have some Haskell data types in memory,
which we later serialize to disk. When the user restarts our application, we
need to reload this data back into Haskell data types, to allow them to resume
work.

Haskell provides us with machinery for doing this serialization via the
[`binary`](http://hackage.haskell.org/package/binary) library, which gives us
the `Binary` type class with two methods:

```haskell
class Binary t where
  put :: t -> Put
  get :: Get t

```

`get` deserializes a sequence of bytes into Haskell values, while `put` operates
in the reverse direction - transforming Haskell values to a sequence of bytes.

Usually, we want the implementations of these methods to be mutual inverses -
the computation in `get` should restore data serialized with `put`, and
vice versa. Unfortunately, nothing in the type system nor the structure of these
methods gives us this guarantee - it's all down to the programmer. I don't trust
myself, so I set out to investigate a more reliable approach.

Ideally, we would like to build up serializers and deserializers from smaller
pieces, such that each piece carries its own inverse. For example, we could pair
up serialization for a `String` with its own inverse:

```haskell
type Serializer a = (Get a, a -> Put)

string :: Serializer String
string = (get, put)
```

As long as `String` has a `Binary` instance where `get` and `put` correctly
specified, we know that `string` is going to function as we expect in both
directions.

We're on to something here, but currently this only works for single
`String`s. What if I have a pair of `String`s that I want to serialize? From
what we've seen so far, there's no way to combine our bidirectional
serializers. Earlier I mentioned that we would like to work with small pieces
and compose them - lets see if we can solve this problem for just serialization
first.

Serialization *consumes* data. If we have data to serialize, the application of
one serializer should consume some of this data, leaving us with slightly less
data that we have to continue serializing. By repeated application of
serializers, we will eventually have considered the entire structure and will
have nothing left to do. This consumption of a structure bit-by-bit suggests
that serialization will be a type changing operation, as a record with a field
removed is certainly not the same type as its larger record. So let's try and
incorporate that:

```haskell
data Serializing a b = Serializing (a -> PutM b)

pString2 :: Serializing (String, String) String
pString2 = Serializing $ \(a, b) -> do put a; return b

pString1 :: Serializing String ()
pString1 = Serializing $ \a -> put a
```

Composition of `Serializing` should now be clear - we just need to compose them
one after another:

```haskell
(.) :: Serializing b c -> Serializing a b -> Serializing a c
(Serializing g) . (Serializing f) = Serializing (f >=> g)

putTwoStrings :: Serializing (String, String) ()
putTwoStrings = pString1 . pString2
```

We've built a serializer that can serialize a tuple of two strings, and we did
so piece-by-piece. While what we have so far is not entirely satisfactory, it
seems like we're heading in the right direction. We'll come back to this later,
but first let's see if the same ideas translate to deserializers.

Our serializer consumed data, so deserialization is naturally the opposite of
consuming data - that is, deserialization *produces* data. When we deserialize
we'll start with nothing, and we'll deserialize a few bytes into part of our
structure one step at a time. Each step of deserialization should take the
smaller structure and expand it into a larger structure - eventually leading us
to the desired structure. Again, this will be a type changing operation, and we
can encode all of this just as we did with `Serializing`:

```haskell
data Deserializing a b = Deserializing (a -> Get b)

getString1 :: Deserializing () String
getString1 = Deserializing $ \() -> get

getString2 :: Deserializing String (String, String)
getString2 = Deserializing $ \s -> do { s' <- get; return (s, s') }

(.) :: Deserializing b c -> Deserializing a b -> Deserializing a c
(Deserializing g) . (Deserializing f) = Deserializing (f >=> g)

getTwoStrings :: Deserializing () (String, String)
getTwoStrings = getString2 . getString1
```

As you can see, it's pretty much exactly the same idea. The only difference is
that now each of our deserializers return a slightly *bigger* structure, whereas
our serializers would move our structure to something *smaller*.

Just to prove that what we have so far works, we can try this in GHCI:

```
> let bytes = case putTwoStrings of Serializing p -> runPut (p ("Hello", "World!"))
> case getTwoStrings of Deserializing g -> runGet (g ()) (LBS.pack bytes)
("Hello","World!")
```

To carry on working towards our goal, we need to pair the `Serializer` up with
its `Deserializer`. Unfortunately, what we have so far won't work:

```haskell
data Serializer a b = Serializer (a -> Get b) (a -> Put b)
```

Notice here how the types *both* move from `a` to `b` - that's certainly not
going to work, as the shape of the data is changing in opposite directions! In
`Get`, `a` is "smaller" than `b`, whereas for `Put` `a` is "larger" then `b`. In
order to work around this, we just need to swap the order of types in one of
these functions - I've swapped the order for `Put`:

```haskell
data Serializer a b = Serializer (a -> Get b) (b -> PutM a)
```

This makes sense - if `put` will shrink our structure, then `get` can move from
this smaller structure back to the original structure. We can express our
`string1` and `string2` serializers now:

```haskell
string2 :: Serializer (String, String) String
string2 = Serializer (\(a, b) -> do put a; return b)
                     (\s -> do { s' <- get; return (s', s) })

string1 :: Serializer String ()
string1 = Serializer put (\() -> get)
```

We were able to compose things before, and we can certainly compose things here...

```haskell
(.) :: Serializer b c -> Serializer a b -> Serializer a c
(Serializer g g') . (Serializer f f') = Serializer (f >=> g) (g' >=> f')

twoStrings :: Serializer (String, String) ()
twoStrings = string1 . string2
```

However, this has a rather significant problem - can you spot it? Take time to
think about this and see if you can work out what's going wrong.

Did you find it? If we fire up GHCI and have a play with our `twoStrings`
serializer, lets see what we get...

```haskell
> let bytes = case twoStrings of Serializer _ p -> runPut (p ("A", "B"))
> case twoStrings of Serializer g _ -> runGet (g ()) bytes
("B","A")
```

Oh no - that's not what we wanted at all! The problem is that the order of
effects are being reversed. When we `put` data, we put the first tuple element
first, and then the second. However, we're reading data in the opposite order -
expecting the *second* element to be first in the stream, which is clearly not
correct. For `(String, String)` the deserializer works but the tuple is in the
wrong order - for other data types this would lead to a runtime exception.

With the current definition of `Serializer`, there's simply no way around this -
the types won't let us run effects in different orders. The reason for this is
that we can only access the underlying `Get` computation by having the smaller
structure around first. However, we can be sneaky and changes things around just
enough to let us run `Get` in a different order. Now the `Get` computation is no
longer a function, but is a computation that *returns* a function:

```haskell
data Serializer a b = Serializer (Get (a -> b)) (b -> PutM a)
```

With this change we *do* have access to any `Get` computation we like, and we
are free to run them in a different order:

```haskell
(.) :: Serializer b c -> Serializer a b -> Serializer a c
(Serializer g g') . (Serializer f f') =
  Serializer (do buildF <- g
                 buildG <- f
                 return (buildF . buildG))
             (g' >=> f')
```

Now it's clear that both our `Put` and our `Get` computations are sequenced in
the same order - nice! It turns out that our composition comes with a sane
definition of identity too, which means our `Serializer` can be used with
`Category`:

```haskell
instance Category Serializer where
  (Serializer g g') . (Serializer f f') =
    Serializer (do buildF <- g
                   buildG <- f
                   return (buildF . buildG))
               (g' >=> f')

  id = Serializer (return id) return
```

## Serializing Through Heterogeneous Lists

We have finally reached a nice core to our solution, but the surface API isn't
really working out. We had to write different `Serializer`s for both `(String,
String)` and `String`, which is certainly not desirable. Ultimately, we would
like to be able to work with just one `Serializer` for `String`, and compose
them however we please.

Unfortunately, working with tuples is causing us the real pain here. The reason
for this is that tuples don't really have any structure that would allow us to
work with them in any sort of principled manner. Instead, what we can do is use
a heterogeneous list, which we can recurse on just like an ordinary linked
list. So, we introduce a type for heterogeneous lists:

```haskell
data List :: [*] -> * where
  Nil :: List '[]
  Cons :: a -> List as -> List (a ': as)
```

And now we can use the new poly-kinded `Category` to upgrade `Serializer` to
work with these lists:

```haskell
data Serializer :: [*] -> [*] -> * where
  Serializer :: (Get (List a -> List b))
             -> (List b -> PutM (List a))
             -> Serializer a b

instance Category Serializer where
  (Serializer g g') . (Serializer f f') =
    Serializer (do mkB <- g
                   mkA <- f
                   return (mkB . mkA))
               (g' >=> f')

  id = Serializer (return id) return
```

This was quite a detour, and has this really helped us? Indeed it has, as we can
now we can write a much more general `Serializer String`:

```haskell
string :: Serializer as (String ': as)
string = Serializer (do a <- get; return (Cons a))
                    (\(Cons a as) -> do put a; return as)
```

The type of `string` now indicates that this `Serializer` can serialize anything
that starts with a `String`, and likewise when deserializing it expects a
`String` to be the first element. This composes exactly as we'd expect:

```haskell
twoStrings :: Serializer as (String ': String ': as)
twoStrings = string . string
```

All we need to do is unwrap the `List` resulting from a `Get` or wrap up data in
a `List` for `Put` and we're good to go:

```haskell
> let bytes = case twoStrings of
                Serializer _ p -> runPut (void $ p ("A" `Cons` ("B" `Cons` Nil)))

> case twoStrings of
    Serializer g _ -> runGet (($ Nil) <$> g) bytes

Cons "A" (Cons "B" Nil)
```

## Destructuring Data Via Prisms

The API we've built works really well if we already have data decomposed into a
`List`, but we don't normally have this luxury. This means we need a way to
convert from a data type to it's constituent parts, and this is exactly the
functionality that `Prism`s in the
[`lens`](http://hackage.haskell.org/package/lens) library provide us with. While
`Prism`s can be a little hard to get your head around, it can be illuminating to
experiment with them in GHCI:

```haskell
> review _Cons (10, [])
[10]

> review _Cons (10, [20])
[10,20]

> review _Cons (1, [2..5])
[1,2,3,4,5]

> preview _Cons [10, 20, 30]
Just (10,[20,30])

> preview _Cons [10]
Just (10,[])

> preview _Cons []
Nothing
```

`Prisms` have two main operations: `review` and `preview`. `review` lets us
construct some data out of its parts - above we use `_Cons` with `(10, [20])`,
which is the same as `(10 : [20])` - resulting in the list `[10, 20]`. `preview`
lets us go the other way, which is the same idea as pattern matching on a
constructor. If we `preview _Cons` on non-empty lists, then the pattern matching
succeeds and the list is separated into its head and tail. However, we can't
pattern match with `_Cons` on an empty list, so `preview` returns `Nothing` -
which corresponds to a pattern match failure.

Armed with `Prism`, we're almost entirely ready to go! The only problem is that
`Prism` normally works with tuples, which we've already seen aren't a great data
for our needs. It's entirely mechanical to convert between tuples and `List`, so
we simply move between them with a type class. Combining this all together, we
have the following:

```haskell
class ListIso a b | a -> b, b -> a where
  _HList :: Iso' b (List a)

usePrism :: ListIso a b  => Prism' d b -> Serializer a '[d]
usePrism p = Serializer get put
  where
  put (Cons d Nil) = do
    Just tuple <- return (preview p d)
    return (tuple ^. _HList)

  get =
    return $ \hlist -> Cons (review p (hlist ^. from _HList)) Nil
```

Now we are free to use this on our data types, just as we'd expect:

```haskell
instance ListIso '[a, b] (a, b) where
  _HList = iso (\(a, b) -> Cons a (Cons b Nil)) (\(Cons a (Cons b Nil)) -> (a, b))

data PairOfStrings = PairOfStrings String String
makePrisms ''PairOfStrings

pairOfStrings :: Serializer '[] '[PairOfStrings]
pairOfStrings = usePrism _PairOfStrings . string . string
```

## Choices

If you look closely at our definition of `usePrism` you might have seen
something suspicious. Here's the relevant code:

```haskell
usePrism = ...
  where
  put (Cons d Nil) = do
    Just tuple <- return (Lens.preview p d)
    return (tuple ^. _HList)
```

In our `put` definition, we are assuming that `Lens.preview` is always returning
a `Just` value. However, we saw earlier that this isn't necessarily the case -
the Prism corresponds to one of potentially many constructors. If we try and use
`usePrism` with a prism that doesn't match our expectations, then things go
horribly wrong:

```haskell
data Strings = PairOfStrings String String | ThreeStrings String String String
makePrims ''Strings
```

```
> case pairOfStrings of
    Serializer _ p -> runPut (void $ p (ThreeStrings "Uh" "Oh" "!" `Cons` Nil))

"*** Exception: Pattern match failure in do expression at ...
```

What we need to do is to allow for choice - if we have multiple possible prisms,
then we need to consider each one. This corresponds to exhaustive pattern
matching in case analysis.

It turns out choice is relatively straight forward to add in. `Get` is already
an instance of `MonadPlus`, so we get choice there for free. `Put` however is a
little more involved, as it doesn't have an instance of `MonadPlus`. The best
solution I've found thus far is to wrap up our `Put` computation inside `Maybe`,
but this isn't entirely satisfactory. Unfortunately `binary` doesn't quite
export enough to have a less expensive solution (`PairS` doesn't have its
oconstructor exported).

`Monoid` is a sensible type class to use for alternatives - choice is
associative, and there is a sane identity (an always-failing `Serializer`). Thus
the final definition of `Serializer` and its associated type classes are:

```haskell
data Serializer :: [*] -> [*] -> * where
  Serializer :: (Get (List a -> List b)) -> (List b -> Maybe (PutM (List a))) -> Serializer a b

instance Category Serializer where
  (Serializer g g') . (Serializer f f') =
    Serializer (g >>= \b -> f >>= \a -> return (b . a))
               (\a -> do putF <- g' a
                         let (b, lbs) = runPutM putF
                         putG <- f' b
                         return (putLazyByteString lbs >> putG))

  id = Serializer (return id) (return . return)

instance Monoid (Serializer a b) where
  mempty = Serializer mzero (const mzero)
  (Serializer g p) `mappend` (Serializer g' p') =
    Serializer (g `mplus` g') (\i -> p i `mplus` p' i)
```

Armed with this final definition of `Serializer`, we're almost ready to provide
a complete definition of serializing our `Strings` type. We need to provide a
little extra information however, which allows us to disambiguate
constructors. This is because if we are deserializing, if I read two strings I
don't necessarily know constructor to choose (yes, if we considered EOF this
could be done, I'm going for brevity). You can find the definition of
`disambiguate` in the full code listing.

Thus the final user-facing code is just:

```haskell
strings :: Serializer '[] '[Strings]
strings = mconcat
  [ usePrism _PairOfStrings . disambiguate 1 . string . string
  , usePrism _ThreeStrings . disambiguate 2 . string . string . string
  ]
```

And just to prove it all works...

```
> let Just putter = case strings of
        Serializer _ p -> p (ThreeStrings "A" "B" "C" `Cons` Nil)
      bytes = runPut (void putter)

> case strings of Serializer g _ -> runGet (($ Nil) <$> g) bytes
Cons (ThreeStrings "A" "B" "C") Nil


> let Just putter = case strings of
        Serializer _ p -> p (PairOfStrings  "Hello" "World!" `Cons` Nil)
      bytes = runPut (void putter)

> case strings of Serializer g _ -> runGet (($ Nil) <$> g) bytes
Cons (PairOfStrings "Hello" "World!") Nil
```

## Final Thoughts

We've seen that it's possible to build correct-by-construction serializers and
deserializers, and we got there by breaking down our problem into small parts
and finding a good way to combine the parts together. Hopefully, I've
illustrated some of the problems that arise from a naive solution, and how these
problems guided us towards an implementation that is both more correct and more
flexible.

`Serializer` is still not perfect however. With the idea of choice above,
there's no way to indicate exhaustive pattern matching. For example, in
`strings` we are considering both constructors, yet `put strings` returns `Maybe
Put`. This isn't particularly satisfactory, because it should now always be
possible to serialize this data type! On a similar note, it becomes harder to
get compile time checks about exhaustive pattern matching, because we're no
longer doing `case` analysis explicitly. This is an interesting problem to me,
and one that I would still like to solve.

There is also a bit more work that we might want to consider doing with `Get`
and `Put`, which is to use a different concept of choice. There are other
options than using `Maybe` - for example we could use lists which would inform
us of *all* possible serializations for a data type, which might provide better
debugging information than simply using the first one that matches.

I'd like to conclude by mentioning that the ideas here aren't particularly
new. In 2010
[Rendel and Ostermann](http://www.tillmann-rendel.com/p/publications.html)
presented a solution using a category of partial isomorphisms and product
functors from this category to Hask, which lead to various libraries on Hackage
such as
[`invertible-syntax`](http://hackage.haskell.org/package/invertible-syntax), and
[`boomerang`](http://hackage.haskell.org/package/boomerang). At ZuriHack,
[Martijn van Steenbergen](http://martijn.van.steenbergen.nl) presented the
latest version of
[`JsonGrammar`](http://hackage.haskell.org/package/JsonGrammar), which uses a
free category to describe operations on a JSON AST, and also illustrated how one
can use prisms to provide a modern vocabulary for partial
isomorphisms. `json-grammar` uses a `stack-prism` data type, which achieves the
same goal as using heterogeneous lists, but does require another Template
Haskell call (`makeStackPrisms`).

While I'm happy with the solution so far, I haven't finished playing around with
this. It's unclear to me how this plays with recursive data types (for example,
does this work for lists? What about trees?), and I need to learn more about
`stack-prism` to see if using heterogenous lists impedes composition (as
[Sjoerd Visscher](https://github.com/sjoerdvisscher/blog) has warned
me!). Hopefully I'll be able to start using what I have so far in production,
iron out the last problems, and release this to Hackage in the near future.

Thanks for reading, a full code listing can be found
[on Github](https://github.com/ocharles/blog/blob/master/code/2014-06-10-reversible-serialization.hs)
