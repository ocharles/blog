----
title: Well Typed's "The Haskell Type System" Course
----

Last Friday, I attended [Well Typed](http://well-typed.com)'s training course on
[the various extensions available in Haskell](https://skillsmatter.com/courses/504-well-typed-s-guide-to-the-haskell-type-system) -
specifically, those available in GHC. I had a terrific time, and as I feel Well
Typed's courses go somewhat un-noticed, it deserves a write up. Despite having a
fairly wide knowledge of type level magic, I always felt my knowledge was a
little... ad hoc. I can confidently say that I no longer feel that is the case.

First though, what was this course about? As you no doubt know, Haskell is well
known for being a very type safe language. However, if we limit ourselves to
"vanilla" Haskell, we can quickly run into problems. Andres began with a
motivating example: a basic quiz application, where we have a list of
`Question`s and a corresponding list of `Answer`s. We can model these in Haskell
as `[Question]` and `[Answer]` - but there's very little that the type system is
doing to aid us build programs manipulating this data. For example, scoring the
questions and answers should be a case of zipping the two lists together - but
if the two lists aren't the same length, then we certainly won't calculate the
right score!

Andres is fantastic at breaking complicated topics apart into small pieces. If
you haven't seen his talk
[explaining free monads](https://skillsmatter.com/skillscasts/4430-monads-for-free)
from last year's Haskell eXchange, I highly recommend it. The Well Typed course
progressed in a very similar way. From the original problem statement, we first
tried to write length-indexed lists using `newtype`s with phantom types, but
noticed that this isn't a particularly useful abstraction. We quickly moved to
GADTs and rephrased our data as `Vec n Question` and `Vec n Answer`, which
already let's us write a much sounder form of `zipWith` for scoring.  This
material alone is well worth learning, as GADTs are a good solution for a wide
range of problems (Andres nicely explained that GADTs are a good way to model
relations between types).

However, we can go further with this data type. One problem we noticed was that
this type wasn't restrictive enough - GHC will quite happily accept types like
of `Vec Bool Char`, which is completely meaningless! Even though we can't
construct any terms of this type, it would be good if we were prevented from
making such mistakes as soon as possible. Using the recent data type promotion
functionality, we addressed this problem, and considered `Vec :: Nat -> * -> *`
a good solution for our application so far.

We then extended the application to support multiple types of questions and
answers (true/false vs. quantity questions), and reached the limit of `Vec`. We
generalised a step further to a type of heterogeneous list called `Env`:

```haskell
data Env :: [k] -> (k -> *) -> * where
  Nil  :: Env '[] f
  (:*) :: f a -> Env as f -> Env (a ': as) f
```

We moved machinery from `Vec` to `Env` to extend our application, still
maintaining a huge amount of type safety. However, the more general type
introduced problems, and we diverged out to understand how type class derivation
works, and observed the need for higher-rank polymorphism to write `zipWith` for
this data type.

After a short break, we explored operations on `Env` in further detail,
comparing against the following "weakly typed Haskell" code[^1]:

[^1]: Of course, we'd normally write this with `zipWith`, so it's not particularly idiomatic Haskell. However, code like this does crop up, and this was an easy example for educational purposes.


```haskell
task :: [Question] -> [Answer] -> (Text -> Bool) -> Maybe String
task qs as p = do
  i <- findIndex p qs
  let a = as !! i
  return (show a)
```

This code is clearly very dangerous - `!!` can fail at runtime if the we have
the wrong index, how do we move this over to `Env`? We'd like to keep the shape
of the program the same, so how would we write a type safe `!!` function? Here
we began to understand how functions that return `Int` and `Bool` are throwing
information away, and that we need to somehow preserve information when we work
with richer types. Rather than pointing into a list with `Int`, we built a `Ptr`
object that gives us a type safe way to point into `Env`, and then we compared
this against the standard implementation of Peano numbers to build more
intuition.

All of this is great, but it's not very practical - we often have data that
lives outside our lovely type-safe sandbox. For example, we'd probably want to
store the questions in a database, and receive answers from website form
submissions. Here we learnt how we can move from a weakly typed setting to
stronger types through decision procedures, and how our "check" functions
actually witness more type information in the process. We saw a need for
existential types and how these can give us one way of encoding type information
that we don't know statically.

With a movement towards proofs, we saw how we can use `:~:` and the new
`Data.Type.Equality` module to introduce new information into the type system -
specifically constructing proofs on natural numbers to implement a type safe
`reverse :: Vec n a -> Vec n a` function. This is a technique I was somewhat
aware of and had briefly seen in Agda, but had certainly never seen done in
Haskell. I must say, I'm quite impressed with how natural it is with the new
`Data.Type.Equality` module!

Finally, we wrapped the day up with a look at type families to perform type
level addition of natural numbers, and saw how associated types can be used with
type classes.

There's a lot of material I've left out, such as singleton types and various
caveats on the extensions we were using; it's remarkable just how much was
squeezed into a day's studying. As I said before, Andres has a very systematic
and reasoned approach to teaching, which lets the student see the small
incremental steps while also being able to check progress against a bigger
picture.

Well Typed offer a wide range of courses - not just courses on the plethora of
extensions to GHC. If you're studying Haskell and want to truly solidify your
knowledge, I can definitely recommend Well Typed's work. Thanks again, Andres!
