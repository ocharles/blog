---
title: 24 Days of Hackage: lens
---

One of my favourite aspects of functional programming is the emphasis on
immutability - rather than mutating things and making direct changes to them,
you work with copies and the differences are reflected in the construction. I
won't harp on about the benefits of doing this, but instead I'll note that it
isn't all ponies and unicorns. Working with purely functional data can be
difficult, especially when you have nested data structures; you often have to
clone something deep inside and then reassemble everything by hand - yuck!

[`Edward Kmett`](http://comonad.com/)'s [`lens`](http://lens.github.com/)
package aims to solve this problem. `lens` provides "families of lenses,
isomorphisms, folds, traversals, getters and setters". This is probably still a
little unclear, and it's easiest to see what `lens` gives you from an
example:

```haskell
      data Point = Point
  { _x, _y :: Double } deriving (Show)

data Monster = Monster
  { _monsterLocation  :: Point
  } deriving (Show)

makeLenses ''Point
makeLenses ''Monster

ogre = Monster (Point 0 0)
```

I've got 2 data structures for my little game - a 2D `Point`, and a
`Monster`. I've made an example `Monster` - the `ogre`.  All `Monster`s have a
location, and we presumably want them to move around. To move the `ogre` without
`lens`, it might look like:

```haskell
      λ> ogre { _monsterLocation = (_monsterLocation ogre) {
            _x = _x (_monsterLocation ogre) + 1
        } }
Monster {_monsterLocation = Point {_x = 1.0, _y = 0.0}}
```

URGH! All of that, just to move 1 to the right?! Lets see how this looks with
`lens`:

```haskell
      λ> monsterLocation.x +~ 1 $ ogre
Monster {_monsterLocation = Point {_x = 1.0, _y = 0.0}}
```

That's much better! We've composed the `monsterLocation` and `x` lens to move
into the `x` part of the `Point` of a `Monster`, and then used `+~ 1` to add one
to it. Almost magically, these changes gets applied and our new `Monster` is
rebuild and returned to us.

`lens` considers itself to come with "batteries included", but this seems like
it's selling itself short - it really gives you a whole power station! As of
this time of writing, lens has 99 operators and covers a huge range of
operations you might wish to do on data. A recent addition is "prisms", which
are 0-or-1 target traversals, meaning they could give you either 1 answer or no
answer at all, depending on the data that is viewed through them.

Natural numbers are a good example of this:

```haskell
      nat :: SimplePrism Integer Natural
nat = prism toInteger $ \ i ->
   if i < 0
   then Left i
   else Right (fromInteger i)
```

Now we can ask if an `Int` is a `Natural`, by trying to view an `Int` through
the `nat` prism:

```haskell
      λ> 5 ^? nat
Just 5

λ> (-5) ^? nat
Nothing
```

Prisms fit in perfectly with the lens API, which means that we can compose them
like any other lens - just like we did with `Monster`s and `Point`s above. In
this example, we've got a pair of `Int`s, perhaps from user input. We want to
multiple each side of this input by 2, but *only* if it's already a natural
number. It sounds tricky, and that we'd likely need conditionals to pull it off,
but not so with `lens`!

```haskell
      λ> both.nat *~ 2 $ (-3,4)
(-3,8)

λ> both.nat *~ 2 $ (8,4)
(16,8)
```

When `lens` views the `(-3)` through the `nat` prism, it doesn't view anything,
because `(-3)` is of course, not a natural number. As such, we just return
`(-3)`, unchanged.  We've composed the `nat` prism with `both` to view "both
sides" of a tuple through the `nat` prism. This example is a variant of the
[official prism documentation](http://hackage.haskell.org/packages/archive/lens/3.7.0.2/doc/html/Control-Lens-Prism.html),
so be sure to check that out if you like this!

As always, I've only covered a tiny portion of the full API. `lens` can also do
traversals, monadic actions, folds, along with including a bunch of lens for
common data structures such as `Set`, `Text`, `Vector` and much more.
