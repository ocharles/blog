---
title: Using indexed free monads to QuickCheck JSON
---

One of the most satisfying parts of my job is the moment when, after months of devout
studying into abstract constructions, I can finally apply what I have learnt to
solve problems in the real world. One of these rare moments occured last week,
and I'm eager to share what I've learnt with you. I'll walk you through a
definition of the problem, my first attempts at solving it, and then at the end
we'll see how I used indexed free monads to easily construct a DSL that produces
an either more powerful solution. On with the show!

## The Problem

I have been tasked with building a web service for our frontend AngularJS
application to talk to. The frontend developer is going to be manually binding
to the output of my web service, so it's paramount that the encoding never
changes. Being JavaScript, the obvious choice of encoding is JSON. For example,
we may have the following data type:

```haskell
data Action = Action { actionType :: Text
                     , actionActor :: Text
                     }
```

Which has the following JSON encoding:

```haskell
instance ToJSON Action where
  toJSON Action{..} = object [ "type" .= actionType
                             , "actor" .= actionActor
                             ]
```

Now that we've done the easy part, we should make sure that we have sufficiently
tested this encoding to ensure we are always using the *correct*
encoding. QuickCheck would be a great tool for this: we simply generate
`Arbitrary` `Action`s, and convert them to JSON, finally verifying that certain
paths in the JSON encoding meet our expectations. Note it's not enough to
witness that `fromJSON . toJSON = id`, because that would only witness that
*there exists* an isomorphism - but we need to be sure that we are using a *specific*
encoding.

## A First Attempt at Testing

My first attempt looked something like this:

```haskell
testAction :: Action -> TestTree
testAction = testGroup "Action" [ testType, testActor ]
 where
  testType = testProperty "type" $ \action ->
    toJSON action ^? key "type" == Just (actionType action)

  testActor = testProperty "actor" $ \action ->
    toJSON action ^? key "actor" == Just (actionActor action)
```

(I'm using `lens-aeson` to traverse the AST that `aeson` produces).

This is a start, but it certainly looks a bit clunky. There's a lot of repetition
going on here - we have to repeat `toJSON action` and we have to make sure we
access sub-parts of the input `action` correctly. Furthermore, when things go
wrong - this doesn't really explain *why* things have gone wrong - only that the
property is not always satisfied, because our QuickCheck property only returns a
boolean.

Nevertheless, in the spirit of getting the job done, and we carry on.

The next data type I had to approach had a rather awkward encoding:

```haskell
newtype Sections = Sections { sections :: Vector Section }

data Section = Section { sectionName :: Text
                       , sectionUrl :: Text
                       }

instance ToJSON Sections where
  toJSON (Sections v) = object $ Vector.toList $
    let indices = Vector.enumFromN 0 (Vector.length v)
        encodeSection i Section{..} =
          object
            [ "sections" .=
                object [ sectionName .= object
                           [ "sort-order" .= i
                           , "url" .= sectionUrl
                           ]
                       ]
            ]
    in Vector.zipWith encodeSection indices v
```

The encoding of a list of sections is an object from the section name to the
section itself, but also containing the sort-order of this section inside the
vector. QuickChecking this becomes a lot more involved - we would have to find
the sections themselves, pull out the JSON for each section and keep a reference
for that, then run tests against it - finally joining everything back
together. Also, note that we have to test against arbitrary `Sections` - which
means a single test is very big. Just having a boolean result is really not
going to cut it now.

## Declarative Tests

The problem, at least to me, felt like I was explaining *how* to perform the
testing, rather than *what* tests I needed to take. I would have rather said
"for each section, expect a JSON object to exist under that key, and then expect
these properties". Armed with a description of the test, I would then be "free"
to interpret these tests separately (get it? eh?). I'd heard a lot about free
monads as a way to write DSLs recently, so this seemed like a perfect excuse to
see just how much I understood them.

There appear to be three main tasks that we will perform in our JSON testing:

* Traversing into the value under a key of an object
* Traversing into the value at an index in an array
* Performed a test on the value we are currently looking at

For the first two, movement in the JSON itself should correspond with a movement
in the Haskell value we used to encode the JSON. For example, if I have an
`Action` object above, traversing into the `"actionActor"` key should be matched
by applying the `Action` we are encoding to the `actionActor` field
accessor. Thus it seems like we can begin writing the functor that will make up
our free monad:

```haskell
data JSONF a = Key String (i -> j) (j -> a)
```

But what a minute, where are these `i` and `j` type parameters coming from? We
can think of `i` as being the initial value we are encoding, and `j` is a
smaller part of `i` that resides under the key we are traversing into. There
seems to be no reason to hide these, so we'll introduce them as type parameters:

```haskell
data JSONF i j a = Key String (i -> j) (j -> a)
```

To recap, we have the name of the JSON key, an accessor function, and a
continuation for the next step of the computation. This continuation receives
the smaller structure we have traversed into (on the Haskell side).

A `Functor` instance here is easy enough:

```haskell
instance Functor (JSONF i j) where
  fmap f (Key key accessor k) = Key key accessor (f . k)
```

Which means we are now ready to start experimenting with a free monad built from
this functor. We introduce a smart constructor for layers in the free monad:

```haskell
key :: String -> (i -> j) -> Free (JSONF i j) j
key k f = Free (Key k f Pure)
```

And an interpreter:

```haskell
performTests :: ToJSON i => Free (JSONF i j) a -> i -> Bool
performTests f =
  let encoded = toJSON subject
      go (Pure _) _ _ = True
      go (Free (Key keyName f k)) actual expected =
        case actual ^? key keyName of
          Just subJSON -> go (k $ f expected) subJSON (f expected)
          Nothing      -> False
  in go f encoded subject
```

This so far gives us the ability to check the presence of keys in a JSON
structure, but unfortunately we hit a snag. Lets say we have a `Sections`
vector, and we just want to check the URL of one section:

```haskell
testSections :: ??
testSections s = do
  key (sectionName $ Vector.head $ sections s)
      (Vector.head . sections)

  key "url" sectionUrl
```

```
    Couldn't match type `Section' with `Sections'
    Expected type: Free (JSONF Sections Section) Text
      Actual type: Free (JSONF Section Text) Text
```

Bummer! The reason this doesn't type check is a normal free monad builds on top
of an un-indexed functor. Notice that the first accessor is from `Sections` to
`Section`, while the second accessor is from `Section` to `Text`. Thus the
functors are `JSONF Sections Section a` and `JSONF Section Text a`. These are
not the same type of functor, thus we cannot use free monads to really do
anything meaningful.

## Indexed Free Monads

Our `JSONF` is actually an *indexed* functor, because the different constructors
permit us to change the index as we go. The index in this case is the state of
the Haskell value before and after applying some sort of traversal into the JSON
structure. Thankfully, it turns out the idea of an "indexed free monad" is
perfectly natural, and Fumiaki Kinoshita and Edward Kmett have already done
the hard work for us (thanks!).

The one draw back of this approach is that we can no longer use the `Monad` type
class in the `Prelude`. If we use `-XRebindableSyntax` we can at least use `do`
notation though:

```haskell
testSections :: IxFree JSONF Sections Text Text
testSections s = do
  key (sectionName $ Vector.head $ sections s)
      (Vector.head . sections)

  key "url" sectionUrl
```

Alright! No changes to the implementation were necessary, just a change to the
type signature. We're on the right track. Now all that remains is to expand our
vocabulary a bit more. Here's a richer language for tests:

```haskell
data JSONF i j a where
  Key :: String -> (i -> j) -> (j -> a) -> JSON i j a
  Index :: Int -> (i -> j) -> (j -> a) -> JSON i j a
  Assert :: (Value -> Either String ()) -> a -> JSON i i a

key :: String -> (i -> j) -> JSONF i j j
key key f = Free (Key key f Pure)

nth :: String -> (i -> j) -> JSONF i j j
nth n f = Free (Index nth f Pure)

assertEq :: ToJSON a => a -> JSONF i i ()
assertEq expected =
  let p actual
        | actual == (toJSON expected) = Right ()
        | otherwise = unlines [ "Expected: " ++ show expected
                              , "     Got: " ++ show actual
                              ]
  in Free (Assert p (Pure ())
```

`Key` and `Index` move us deeper into the JSON structure, while `Assert` takes
the current JSON `Value` and checks it against an arbitrary predicate. The
predicate can fail with a string indicating why the assertion failed. I've added
one smart constructor for predicates, which assumes that the current JSON
matches the `ToJSON` encoding of a value. We can now rephrase the initial
`Action` tests in our new DSL:

```haskell
testAction = testGroup "Action" [ testType, testActor ]
 where
  testType = testProperty "type" $ performTests $ do
    actual <- key "type" actionType
    assertEq actual

  testActor = testProperty "actor" $ performTests $ do
    actual <- key "actor" actionActor
    assertEq actual
```

Great - we're at least as capable as before! It turns out we can go further,
with just a little more work. If we upgrade to an indexed `MonadPlus` monad, we
also gain the ability to perform multiple tests at once. This is the key part
for testing arrays, as it permits us to backtrack our JSON traversal. This
requires little work on our part - we simply switch out
`Control.Monad.Indexed.Free.IxFree` for `Control.Monad.Indexed.Free.Plus.IxFree`
and modify the interpreter to deal with the `Plus` constructor of `IxFree`. This
would let us combine our two tests into one:

```haskell
testAction = testProperty "Action" $ performTests $
  isum [ key "type" actionType   >>>= assertEq
       , key "actor" actionActor >>>= assertEq
       ]

 where isum :: [IxFree i j a] -> IxFree i j a
```

I've also demonstrated here that now that we have used a monad as our underlying
test representation, we get to make use of all the monadic combinators to
structure our tests (or at least the indexed-monad equivalents).

## Tell Me What's Wrong

Now that our tests are more extensive, we really need to work on making it easy
to respond to test failures. In order to do so, we need better diagnostics. Now
that we have separate the test specification from the test running, it's easy to
add diagnostics - we just extend the test interpreter.

Each traversal - be it into an object by key or an array by index - is described
in full in our functor. Thus when we interpret, we can also build up a human
readable string of where we are, and say which properties fail to meet our
expectations. The extended interpretor can be found
[here](https://github.com/ocharles/json-assertions/blob/master/src/Test/JSON/Assertions.hs) -
I won't go into details now, but hopefully you can follow it easily enough.

## Yak: Shaved. Now Back to the Problem

Finally, lets look at testing that horrible `Sections` vector that got us here
in the first place. Our tests can now be expressed as:

```haskell
testSections :: JSONTest Sections 
testSections = performTests $ do
  sections <- key "sections" sections
  isum $ flip map [0 .. Vector.length sections] $ \i -> do
    let s = sections Vector.! sections
    key (sectionName s) (const s)

    isum [ jsonTest $ key "url" sectionUrl >>>= assertEq
         , jsonTest $ key "sort-order" (const i) >>>= assertEq 
         ]
```

We just need one more combinator - `jsonTest` - which simply discards the final
state. The reason for this is because `isum` expects that all alternative
actions end in the same state. We can easily end in the same state regardless of
how we got there if we just always end with `()` - so `jsonTest :: JSONTest i j a ->
jsonTest i () a`.

I think this is a really concise way to explain the expectations of JSON
serialisation for a fairly convoluted encoding. We move into the "sections" key,
which is akin to pulling out the `Vector Section` from a `Sections`. Then, we
access each element of this vector by its index (zipping each element with it's
index would also be appropriate), and for each `Section` we attempt to traverse
into a key that matches the `sectionName`. I use `const s` as I already know the
`Section` that I'm expecting. I complete my tests by summing a series of tests
to be performed on this individual section. My tests can refer back to any
previous variable that we bound earlier - so the test for the `"sort-order"` can
elegantly refer back to the index of the `Section` in the original `sections`
`Vector`.


## Conclusion and Next Steps

I feel that I've mostly taken logical steps from the initial problem to *a*
solution, but it's not necessarily the only solution. The indexed monad felt
natural due to the changing type of environment as I traverse the JSON. However,
this comes with a cost for the user - as they now have to enable
`RebindableSyntax` and bring appropriate definitions for `>>=` into scope. Is it
possible that there are other types that we could use? I expect so! For example,
things that move from `i` to `j` look suspiciously like arrows which we also get
special notation in Haskell. Or maybe there's a way to use existential types to
hide the before or after types. I don't know, but it may lead to yet simpler
tests with less exotic structures underneath them.

Moving away from the underlying representation, this work could go further in
terms of functionality too. So far it only tests that the JSON contains the
right keys and values, but it doesn't test *exhaustively*. It wouldn't be much
work to extend the interpreter to fail the test if there are unexpected keys
too.

Either way, what I have at the moment feels useful enough to me that the real
next step is to get this stuff onto Hackage. In the meantime, you can find this
code on [my Github account](http://github.com/ocharles/json-assertions). I just
have a few pull requests to get merged, and then I'll release this.
