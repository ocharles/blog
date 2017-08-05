---
title: "24 Days of Hackage: containers"
---
The basic linked list is a data structure that every aspiring computer
programmer comes to know and love at some point early in their career. Haskell
is no different here - the linked list is baked right into the language,
including its own special syntax for construction and pattern matching. In fact,
Haskell's linked list could be seen as even more powerful than those in other
languages as lazy evaluation allows us to construct lists of infinite length.

With all that said, lists are not the end all of data structures - often we have
specific needs, such as *O(1)* retrieval, specific data access patterns to
optimize for, or perhaps just more speed. The `containers` library is one of the
answers to this problem.

[`Containers`](http://hackage.haskell.org/package/containers) is a library in
the Haskell platform which provides implementations of maps, sets, sequences,
graphs and trees. The current maintainer is
[Johan Tibbel](http://blog.johantibell.com/) - so you know its going to be fast!
(Johan has
[blogged](http://blog.johantibell.com/2011/06/memory-footprints-of-some-common-data.html)
about performance a few times, and regularly talks on the topic)

So, how does the API look? After all - we spent all this time learning how to
`fold` and `map` in our Haskell infancy, it would be a pain if this knowledge
doesn't carry forward. Luckily, it does. All the structures provided by
`containers` feature instances of various Haskell type classes, such as
`Functor`, `Traversable` and `Foldable`. Of course, each structure offers
something new to the table, so while there are a few more functions to be aware
of the documentation is fantastic and easy to navigate.

As a quick example, lets assume we have a function that does some IO to give us
a mapping from a `Person` to a `Set` of their favourite `Colour`s.

```haskell
peopleFavColours :: IO (Map Person (Set Colour))
peopleFavColours = ...
```

Now, what if we want to find a `Set` of all favourite colours? Presumably, we
just take all the `Set`s of `Colours` for all people and "smash" them together
into a new set. Hmm, this "smashing" together sounds exactly like something a
`fold`, perhaps with a `Monoid` instance...

```haskell
allFavColours :: IO (Set Colour)
allFavColours = fold <$> peopleFavColours
```

Because `Map k` is an instance of `Foldable`, and `Set` is an instance of
`Monoid`, we can simply `fold` our `Map Person (Set Colour)` down into a single
`Set Colour`; the `Monoid Set` instance for combining multiple `Sets` together
is simply the union of them.

Finally, what if we wanted to print out each of these colours, one per line?
Iteration with side effects sounds like something we could do with a function
like `mapM_`. Again, the trusty `Foldable` type class can help us out, with
`mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()`:

```haskell
showColours :: IO ()
showColours = allFavColours >>= mapM_ print
```

`Set` is `Foldable` - so this code is *identical* to the code to do this for a
list! Talk about reusable knowledge.

As mentioned before, `containers` is part of the Haskell platform and you most
likely already have it installed. It takes very little effort to get started,
and almost every application will find a need for these data structures at some
point.

Go on, have a play!
