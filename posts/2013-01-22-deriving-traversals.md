---
title: Composing Traversals: A Worked Example of Deriving a Non-Trivial Traversal
---

If you’ve seen me comment on [reddit](http://reddit.com/r/haskell), then you’ve
probably heard me harping on about the
[Essence of the Iterator Pattern](http://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf). However,
while I’m happy to tell people to read this magnificant paper, I haven’t
actually written about it myself. Time to rectify that!

In today’s post we’ll look at lists of *artist credits*, a concept from
MusicBrainz, and how we can build a traversal to do some basic cleaning up of
lists of artist credits. We’ll start with the obvious solution based on pattern
matching, and then see how we can express this as the composition of various
traversals, and what this gains us.

## Motivation

Today, I was working on some [MusicBrainz](http:/musicbrainz.org) stuff and was
frustrated at how un-Haskell it was. I suppose a big part of that was that it
*wasn’t* Haskell (most of MusicBrainz is written in Perl), but that didn’t stop me
from having a quick play at expressing the same idea in Haskell (on my lunch
break, boss, I swear!)

Allow me to set the scene. MusicBrainz is a meta-database that captures metadata about music. Who produced which albums - that sort of thing. We recently added the idea of artist ‘credits’, where multiple artists can be used as the producer of an album - e.g., for collaborations. Artist credits are a list of individual credits, where a credit consists of a pointer to an artist, a name that artist was credited as (to permit name variations), and a join phrase - a snippet of text that is used to intercalate multiple credits into a single string.

So we could say that if we have an individual ArtistCredit, such that

```haskell
data ArtistCredit = ArtistCredit
  { acArtist :: Int
  , acName :: String
  , acJoinPhrase :: String
  }
```

Then an album is a produced by a list of ArtistCredits - that is, [ArtistCredit].

The collaboration between Slayer and Justin Bieber would be:

```
> let slayer = ArtistCredit { acArtist = 1
                            , acName = "Slayer"
                            , acJoinPhrase = " & "
                            }

> let bieber = ArtistCredit { acArtist = 666
                            , acName = "Justin Bieber"
                            , acJoinPhrase = ""
                            }

> let collaboration = [slayer, bieber]

> putStrLn (render collaboration)
Slayer & Justin Bieber
```

All well and good so far, but we need a little bit of validation, and this is
where things can become a little bit tricky. As the join phrase of the last
`ArtistCredit` is the final string, something that we don’t want is trailing
whitespace. However, some of the join phrases *can* have trailing whitespace. In
fact, every join phrase but the last one can have trailing whitespace. How can
we go about making sure that this is so?

Here’s one way that we could do that:

```haskell
trimArtistCredits :: [ArtistCredit] -> [ArtistCredit]
trimArtistCredits [] = []
trimArtistCredits [a] = [a { acJoinPhrase = rtrim $ acJoinPhrase a }]
trimArtistCredits (x:xs) = x : trimArtistCredits xs
```

(I’m assuming `rtrim :: String -> String` exists).

It’s alright, but I can’t help but feel the signal to noise there is quite
low. Not to mention this whole idea of ‘transform the last element’ is
intertwined with this code. Generally, it feels very un-Haskelly. Can we do
better?

## Composing Traversals

You bet we can! Firstly, a game plan. We know that we have a function that we
want to apply conditionally to the list, namely to the last element of the
list. But first, lets ignore this whole traversal business, and concentrate on a
single ArtistCredit, stealing that transformation from the code earlier.

```haskell
trimArtistCredit :: ArtistCredit -> ArtistCredit
trimArtistCredit a = a { acJoinPhrase = rtrim $ acJoinPhrase a }
```

Marvelous. This was a good idea, as we can now combine this transformation with
combinators and get more powerful functions. For example:

```haskell
trimAllArtistCredits :: [ArtistCredit] -> [ArtistCredit]
trimAllArtistCredits = map trimArtistCredit
```

The type signature is right, but the behavior is wrong - this will trim trailing
whitespace from *all* join phrases, but as we established, we want to trim only
the final join phrase. We seem to be lacking the right combinator for the
job. Before we begin our search forthis combinator, let me show you another way
of writing `trimAllArtistCredits`:

```haskell
trimAllArtistCredits :: [ArtistCredit] -> [ArtistCredit]
trimAllArtistCredits =
  runIdentity . traverse (Identity . trimArtistCredit)
```

Here I’m using the
[`Identity`](http://hackage.haskell.org/package/transformers-0.3.0.0/docs/Data-Functor-Identity.html)
applicative functor inside a `Traversal`. This is equivilent to our previous
definition, at the cost of a bunch more typing. So why am I showing you this?

The key point here is that we have the ability to *enrich* our pure function,
`trimArtistCredit`, with more context. We wrapped it inside `Identity` here,
which adds absolutely nothing, but now the stage is open to a whole lot
more. And with that, we’re finally ready to start tackling the meat of the
problem!

We know that we need a combinator that will act on the last element of a
list. Determining when you’re at the end of something is tricky though - you
need to ‘look ahead’ to see if there’s more data. But we can turn this problem
on its head, quite literally, by acting on the first element of the reversed
list, and then reversing all of that.

So we’ve broken down our problem into two smaller problems now - we need a way
to act on the first element of something, and we need to do this reversing. It
seems like to do this “first element” stuff we’ll need some sort of stateful
function - where the state is whether or not we’ve seen the first element.

Here’s one approach:

```haskell
actOnFirst :: (a -> a) -> a -> State Bool a
actOnFirst f x = state go
  where go False = (f x, True)
        go True = pure x

runFirst :: State Bool a -> a
runFirst = flip evalState False
```

My `actOnFirst` combinator is something that takes a function, and gives you back
a new function that takes some input and if the state is `False` (that is, hasn’t
yet ran), runs the function on your input, transitioning the state to
`True`. Otherwise, it’s already ran, so it just behaves as `id`.

We can now combine this with `trimArtistCredit` and traverse to trim the
whitespace from the first `ArtistCredit`:

```haskell
trimArtistCredits =
  runFirst . traverse (actOnFirst trimArtistCredit)
```

And if we reverse the input and output, we’re finally back to a working
implementation!

```haskell
trimArtistCredits =
  reverse .
    runFirst . traverse (actOnFirst trimArtistCredit) .
      reverse
```

Happy? Well, guess what - we can do better.

## Moving Backwards

The [`transformers`](http://hackage.haskell.org/package/transformers) library is
well known for providing monad transformers, but I think people forget that it
provides functor combinators too. One that will come in very handy for us is the
`Backwards` applicative functor combinator. As the documentation says:

> `Backwards f a`: The same functor, but with an `Applicative` instance that
> performs actions in the reverse order.

Inteeersting. So... if I give this some sort of traversal, it will run it in the
opposite order. That is, where I was previously traversing a list from
left-to-right, I could now traverse from right-to-left. Having a quick play with
`Identity`, shows that it preserves the input ordering:

```haskell
>  runIdentity $ forwards $ traverse (Backwards . Identity) [1, 2, 3]
[1, 2, 3]
```

Backwards just means that we first process 3, then 2, then 1. That’s exactly
what we were doing above! So we don’t need to `reverse` and `reverse` again, we
can just transform our traversal. This brings us round to:

```haskell
trimArtistCredits = runFirst . forwards .
  traverse (Backwards . actOnFirst trimArtistCredit)
```

And we might wish to finally tidy that up by introducing the `actOnLast`
combinator:

```haskell
actOnLast :: (a -> a) -> a -> Backwards (State Bool) a
actOnLast f = Backwards . actOnFirst f

runLast :: Backwards (State Bool) a -> a
runLast = runFirst . forwards
```

Which brings us to our final implementation:

```haskell
trimArtistCredits = runLast . traverse (actOnLast trimArtistCredit)
```

Very cool.

## Concluding Thoughts

What I’ve tried to convey in this post is that if we force ourselves to think in
a compositional manner, it’s possible to write code that is readable and
extremely easy to reason about. In our final implementation, we have a few
little building blocks that need to be correct - `actOnFirst` needs to do the
right thing, and `trimArtistCredit` does too. However, they can do the right
thing in isolation. I know that if the parts are right, they fit together, and I
put them together in the right order, my program does the right thing.

Here’s our final implementation, without the helper combinators (assume they
were packaged up on Hackage):

```haskell
trimArtistCredits :: [ArtistCredit] -> [ArtistCredit]
trimArtistCredits = runLast . traverse (actOnLast trimArtistCredit)
```

Contrast this with our original solution:

```haskell
trimArtistCredits :: [ArtistCredit] -> [ArtistCredit]
trimArtistCredits [] = []
trimArtistCredits [a] = [trimArtistCredit a]
trimArtistCredits (x:xs) = x : trimArtistCredits xs
```

Can you reason about this? Sure you can, but I’d argue only if you execute it in
your head.

Furthermore, we ended up with a few abstractions that are useful outside of this
problem. `actOnFirst` could be useful for lots of other things, so you might
even consider shipping that outside of your project for other people to use.

Also, look what happens if we ask Haskell what the inferred type signature of
`trimArtistCredits` would be:

```haskell
> :t trimArtistCredits
(Traversable t) => t ArtistCredit -> t ArtistCredit
```

It’s not even limited to lists! If we later decide to switch over to `Vector` or
`Seq` to represent lists of `ArtistCredit`s, we just change the type and this
code needs absolutely no extra work - it just does the right thing.
