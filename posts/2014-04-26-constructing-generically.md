---
title: Building data constructors with GHC Generics
---

Let's imagine for a moment that we are in the presence of the following data
type:

```haskell
data Coffee = MkCoffee { coffeeBeans :: String
                       , coffeeOriginCountry :: Country
                       , coffeeBrewMethod :: BrewMethod
                       }
  deriving (Generic)
```

However, we have only been given the type itself - and not a constructor for
this value. That is, we *don't* have access to this function:

```haskell
MkCoffee :: String -> Country -> BrewMethod -> Coffee
```

Is it still possible to make `Coffee`? As we'll see, by making use of the
`Generics` type class it is indeed possible to make `Coffee`, and with a little
bit of functor tricky, we can derive an implementation of the `MkCoffee` data
value constructor ourselves.

## GHC Generics

Before we get going, it's worth discussing briefly what is meant be the idea of
a generic representation, specifically GHC generics. The term "generic
programming" is fairly overloaded, but for this post we're interested in the idea
of *shape* generics. This (and the other types of generic programming) are
discussed in [José Pedro Magalhães](http://dreixel.net)' thesis
"[Less is More](http://dreixel.net/research/pdf/thesis.pdf‎)", but loosely
speaking we're interested in abstracting over the "shape" of data types. This
means we find another value that isomorphic to our, but is made out of a smaller
(and importantly, closed) set of primitives.

GHC Generics is one such approach to shape genericity - `Rep` type is the type
of isomorphic data, and we have `to` and `from` to move between both sides of
the isomorphism. Let's see what this all means for `Coffee`:

```haskell
> :kind! (Rep Coffee)
Rep Coffee :: * -> *
= M1
    D
    Main.D1Coffee
    (M1
       C
       Main.C1_0Coffee
       (M1 S Main.S1_0_0Coffee (K1 R [Char])
        :*: (M1 S Main.S1_0_1Coffee (K1 R Country)
             :*: M1 S Main.S1_0_2Coffee (K1 R BrewMethod))))
```

This is neither pretty nor succinct, but thankfully a lot of this is noise (for
our purposes). Here's another look at just the essential structure of `Rep
Coffee`:

```
K1 [Char] :*: (K1 R Country :*: K1 R BrewMethod)
```

Now we can see that `Coffee` is isomorphic to the product of a string, a country
and a `BrewMethod`. These correspond to the three fields in the above `Coffee`
data type.

Using these generic representations, we can construct new `Coffee`s:

```haskell
> to (M1 (M1 (M1 (K1 "Single Origin") :*: (M1 (K1 (Country "Rwanda")) :*: M1 (K1 V60))))) :: Coffee
MkCoffee { coffeeBeans = "Single Origin"
         , coffeeOriginCountry = Country "Rwanda"
         , coffeeBrewMethod = V60
         }
```

That's pretty cool, no? It's this idea that is at the heart of generic
programming. We could imagine doing a similar thing by building these values by
reading a binary serialisation from a file, or walking a JSON AST.

However, for our purposes we've not yet reached our goal. To recap, we really
want to be able to build a function like `MkCoffee`, keeping the generic
representation behind the scenes. To do this, we'll need to *interpret* the
generic representation into a function.

## Interpretting Generic Representations

The standard way to work with a generic representation is to walk the tree using
instances of a type class. We'll do the same, and walk the generic
representation to reach a functor that will contain `Rep Coffee`. Later, we'll
be able to use `fmap to`, turning `Rep Coffee` into real `Coffee`.

Our workhorse is the following type class.

```haskell
class Functor f => Mk rep f | rep -> f where
  mk :: f (rep a)
```

As you can see, it's a multi-parameter type class, taking the generic `Rep`
type, and also indicating which functor can "build" this `Rep`. Each `Rep`
uniquely determines the constructing functor, which we indicate with a
functional dependency. This is essential for having sane type inference.

Starting "at the bottom", we can begin by constructing a single field. In our
`Coffee` example, we need to construct `String`s, `Country`s, and
`BrewMethod`s. In GHC Generics, each of these is represented with `K1`. To
actually construct `K1` we need a value, so our constructing functor will be a
one-argument function:

```haskell
-- Remember that ((->) c) means (c ->)
instance Mk (K1 i c) ((->) c) where
  mk = \x -> K1 x
```

A `Coffee` is more than just a single field though, and we need a way to combine
individual fields together. This is done by the use of the `:*:` constructor,
which we can think of as having "fields on the left" and "fields on the
right". To construct `:*:` we need to compose a builder on the left with
the builder on the right, so we use `Compose` to join the left and right
functors into one. The definition of `mk` itself is a little cumbersome, but
does the job:

```haskell
instance (Mk l fl, Mk r fr) => Mk (l :*: r) (Compose fl fr) where
  mk = Compose (fmap (\l -> fmap (\r -> l :*: r) mk) mk)
```

Finally, we just need to construct the `M1` layers, which just hold
meta-data. These don't have any effect on what we're trying to do, so their type
class instance simply proxy through to other instances:

```haskell
instance (Mk f f') => Mk (M1 i c f) f' where
  mk = M1 <$> mk
```

Believe it or not, we're now a good step closer to getting what we want. Let's
have a look and see what we get if we try and `mk` some `Coffee`:

```haskell
fmap (to :: Rep Coffee a -> Coffee) mk
  :: Compose
       ((->) String) (Compose ((->) Country) ((->) BrewMethod)) Coffee
```

Hey, that's actually pretty close! If we squint, this is some sort of function
that takes a `String`, a `Country` and a `BrewMethod` and yields some
`Coffee`. All that we have to do is somehow get rid of all the `Compose` noise.

## Unwrapping Compose

At this point, I was originally stuck, but then I remembered smart people have
already come before me to work on very similar problems. Specifically,
[Ralf Hinze](http://www.cs.ox.ac.uk/people/ralf.hinze/publications/index.html)
published a Functional Pearl a while ago called "Formatting: A Class Act", which
uses an identical construction (which I can honestly say was a happy
accident!). Hinze then goes a step further with this magic little type class:

```haskell
class Functor f => Apply f a b | f a -> b where
  apply :: f a -> b

instance Apply ((->) a) b (a -> b) where
  apply = id

instance (Apply g a b, Apply f b c) => Apply (Compose f g) a c where
  apply (Compose x) = apply (fmap apply x)
```

This time we have a type class of three types (!), where the first two types
determine the third. However, it's not so bad - `f` is the functor we need to
expand, `a` is the type of data under the functor and `b` is the final type
after expansion. If you look at the specific instances, it should be clear how
this all plays out.

Now we can use `apply` with `mk` to generate a function!

```haskell
> apply (fmap to mk) :: String -> Country -> BrewMethod -> Coffee
<interactive>:1:2:
    No instance for (Apply
                       f0 a0 (String -> Country -> BrewMethod -> Coffee))
      arising from a use of ‘apply’
    The type variables ‘f0’, ‘a0’ are ambiguous
    Note: there are several potential instances:
      instance (Apply g a b, Apply f b c) => Apply (Compose f g) a c
        -- Defined at 2014-04-26-coffee.hs:39:10
      instance Apply ((->) a) b (a -> b)
        -- Defined at 2014-04-26-coffee.hs:36:10
```

Damn! What went wrong?

Unfortunately, if we use `apply` with `mk` we lose parametricity on `f`
itself. There could be many different ways to reach the type we desire
(especially as type classes are open), and it turns out that the crucial
ingredient is having `rep` available.

However, from our perspective it *should* be possible to infer all of this from
the return type of the function we are building. Of course, GHC can't guess, so
we will need to encode this information somehow. With GHC 7.8 we can easily
express this with a closed type family:

```haskell
type family Returns (f :: *) :: * where
  Returns (a -> b) = Returns b
  Returns r = r
```

`Returns` lets us figure out what the final type of a function is, and now we
can complete the loop and build our final `make` function:

```haskell
make :: forall b f z. (Generic (Returns b), Apply f (Returns b) b, Mk (Rep (Returns b)) f) => b
make = apply (fmap (to :: Rep (Returns b) z -> (Returns b)) (mk :: f (Rep (Returns b) z)))
```

We need to use `ScopedTypeVariables` to carry a bit of extra type information
around, but luckily this is all behind the scenes.

*Finally*, we can now write `mkCoffee`:

```haskell
mkCoffee :: String -> Country -> BrewMethod -> Coffee
mkCoffee = make
```

Does it work?

```haskell
> mkCoffee "Single Origin" (Country "Rwanda") V60
Coffee { coffeeBeans = "Single Origin"
       , coffeeOriginCountry = Country "Rwanda"
       , coffeeBrewMethod = V60
       }
```

Tada!
