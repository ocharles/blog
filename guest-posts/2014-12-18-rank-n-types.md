---
title: 24 Days of GHC Extensions: Rank N Types
---

It's been a while since 24 Days of GHC Extensions looked at an extension that radically altered the landscape of programs we can write. Today, [ertes](https://twitter.com/ertesx) is going to walk us through (extensively!) GHC's *rank n types* feature.

---

Let's talk about polymorphism today, in particular higher-rank
polymorphism using GHC's `RankNTypes` extension.  This is the one -- the
type system extension to rule them all.

We will start with a quick recap of what exactly (regular) polymorphism
is and how we might interpret it.  Then we will find out what
higher-rank polymorphism adds and how we can use it to give our programs
a boost in expressivity, safety and even efficiency.

So what is polymorphism in the first place?  To understand it we should
understand concrete (*monomorphic*) values first.  Okay, so what is a
*concrete* value?  Here is an example:

    intId :: Integer -> Integer
    intId x = x

This is a concrete value, a function.  When we refer to `intId` we refer
to a certain fully defined value (which is a function) of a certain
fully defined type (`Integer -> Integer`).  Note that the function
itself is the concrete value we refer to.  Here is a second example:

    doubleId :: Double -> Double
    doubleId x = x

Now these two values are of different types, but their definitions are
exactly the same.  Can we save some typing and perhaps even get
additional safety along the way?  Indeed, we can.  Like many languages
Haskell allows us to provide a single definition to cover the above two
cases and also infinitely many more:

    id :: a -> a
    id x = x

You have probably seen this before.  As you can see, the definition is
still the same.  This kind of polymorphism is called *parametric
polymorphism*, and in other languages you will usually find it under the
name *generics*.  One thing to note at this point is that Haskell will
only allow this if there is indeed a single definition.  In other words
you cannot choose the definition of a value based on its type (for now).

It also adds safety through a property called *parametricity*.  If we
pretend that there are no infinite loops or exceptions
([it's okay to do that][fast-and-loose], so we will do it throughout
this article), then the function is actually fully determined by its
type.  In other words, if we see the type `a -> a`, we know that the
corresponding value *must* be the identity function.

[fast-and-loose]: <http://citeseer.ist.psu.edu/viewdoc/summary?doi=10.1.1.59.8232>


Rank-1 polymorphism
-------------------

Commonly the above definition is called *the* identity function.  But in
fact we should think of it as a whole family of functions.  We should
really say that `id` is *an* identity function *for all* types `a`.  In
other words, for every type `T` you might come up with, there is an
identity function called `id`, which is of type `T -> T`.  This is the
type-checker's view anyway, and by turning on the `RankNTypes` extension
we can be explicit about that in our code:

    {-# LANGUAGE RankNTypes #-}

    id :: forall a. a -> a
    id x = x

Now it is much clearer that `id` is really a family of infinitely many
functions.  It is fair to say that it is an abstract function (as
opposed to a concrete one), because its type abstracts over the type
variable `a`.  The common and proper mathematical wording is that the
type is *universally quantified* (or often just *quantified*) over `a`.

When we apply the identity function to a value of a concrete type, then
we *instantiate* the type variable `a` to that concrete type:

    id (3 :: Integer)

At that application site the type variable `a` becomes a concrete type,
namely `Integer`.  It is valid to apply `id` with different
instantiations of its type variable:

    print (id (3 :: Integer),
           id "blah")

Another way to look at this is in terms of promise and demand.  You
could say that the type signature of the `id` function *promises* that
the definition works *for all* types `a`.  When you actually apply the
identity function you *demand* a certain type.  This is a useful
interpretation when we move to higher-rank polymorphism.


Rank-2 and higher polymorphism
------------------------------

So far we have only enabled the extension to allow us to be more
explicit about the "for all" part.  This alone is just a syntactic
change and adds no new expressivity.  However, we can use this new
syntax within a type alias:

    type IdFunc = forall a. a -> a

Remember that the type fully determines the corresponding function?  So
any value of type `IdFunc` must be the identity function.  But `IdFunc`
is just a plain old regular type alias, isn't it?  That means of course
we can use it in type signatures.  For example we could have written:

    id :: IdFunc
    id x = x

Notice that the type variable is gone entirely.  A much more interesting
way to use `IdFunc` is as the domain of a function:

    someInt :: IdFunc -> Integer

Isn't this curious?  Since any value of type `IdFunc` must be the
identity function the `someInt` function is a function that expects the
identity function as its argument and returns an integer.  Let's give it
some (arbitrary) definition:

    someInt id' = id' 3

This is something new that we didn't have before:  `someInt` has
received a function `id'` about which it knows that it is the fully
fledged polymorphic identity function.  So it can instantiate its type
variable as it likes, and it does so.

The `someInt` function isn't even polymorphic!  Rather it expects a
polymorphic function as its argument.  This becomes clear when we expand
the type alias:

    someInt :: (forall a. a -> a) -> Integer

This function is completely monomorphic.  Its type is not quantified.
When we apply a polymorphic function like `id` we get to choose which
types to instantiate as.  The `someInt` function does not give us such a
choice.  In fact it requires us to pass a sufficiently polymorphic
function to it such that *it* can make that choice.  When we apply it,
we need to *give it* choice.

If this does not make sense, look at it using the promise/demand
interpretation.  The identity function makes a promise.  It promises to
work *for all* `a`.  When you apply it, you demand `a` to be a certain
type.  However, the `someInt` function makes no such promise.  It wants
us to pass it a function that makes a promise, such that it gets to
demand something from it.  We don't get to demand anything.

This is called rank-2 polymorphism.  You can have arbitrary-rank
polymorphism by burying the quantifier in more levels of necessary
parentheses.  Example:

    type SomeInt = IdFunc -> Integer

    someOtherInt :: SomeInt -> Integer
    someOtherInt someInt' =
        someInt' id + someInt' id

This function is rank-3-polymorphic, because the quantifier is in the
third level of necessary parentheses:

    someOtherInt :: ((forall a. a -> a) -> Integer) -> Integer


Example: random numbers
-----------------------

Suppose that you want to initialise a potentially large and recursive
data structure with random values of different types.  We will use a
very simple one, which is sufficient for demonstration:

    import System.Random

    data Player =
        Player {
          playerName :: String,
          playerPos  :: (Double, Double)
        }
        deriving (Eq, Ord, Show)

We want to construct a random player.  They should get a randomly
generated name of a random length and also a random position.  One way
to do this is to pass around a random number generator explicitly:

    randomPlayer :: (RandomGen g) => g -> (Player, g)

But we want to do more.  Since the data structure is so huge, we want to
print some progress information while we're generating it.  This
requires `IO` of course.  Rather than enforcing a certain transformer
stack we would just request a sufficiently featureful monad by using
effect classes:

    import Control.Monad.State

    randomPlayer
        :: (MonadIO m, MonadState g m, RandomGen g)
        => m Player

However, the user of `randomPlayer` may already be using a state monad
for something else, or the random number generator may actually live in
a mutable variable.  You might even run into a case where the random
number generator is completely hidden global state, so all you get is
monadic actions.  At this point things start to become really awkward.

But with higher-rank polymorphism there is actually a very simple
solution.  The first step is *not* to request an explicit functional
representation of the random-number generator, but rather just *some*
monad `m` that provides *some* means of random number generation.  Then
generating a random number (or really anything else with a `Random`
instance) is a matter of performing a certain `m`-action.  We can write
type aliases for these `m`-actions:

    type GenAction m = forall a. (Random a) => m a

    type GenActionR m = forall a. (Random a) => (a, a) -> m a

A value of type `GenAction m` is an `m`-action that supposedly produces
a random element of whatever type we request, as long as there is a
`Random` instance.  The `GenActionR` type represents the ranged
variants.

One simple example is the action that generates a
random number in a state monad, when the state is a generator:

    genRandom :: (RandomGen g) => GenAction (State g)
    genRandom = state random

    genRandomR :: (RandomGen g) => GenActionR (State g)
    genRandomR range = state (randomR range)

If we expand the `GenAction` alias and simplify (we will learn how to do
that later), then the type of `genRandom` becomes:

    genRandom :: (Random a, RandomGen g) => State g a

Now we can write a function that requests such a random number generator
as its argument:

    randomPlayer :: (MonadIO m) => GenActionR m -> m Player
    randomPlayer genR = do
        liftIO (putStrLn "Generating random player...")

        len <- genR (8, 12)
        name <- replicateM len (genR ('a', 'z'))
        x <- genR (-100, 100)
        y <- genR (-100, 100)

        liftIO (putStrLn "Done.")
        return (Player name (x, y))

Notice how the function uses the fact that it *receives* a polymorphic
function as its argument.  It instantiates its type variable as various
different types, including `Int` (for `len`) and `Char` (for `name`).
If you have some global-state random number generator, then this
function is actually surprisingly easy to use.  The `randomRIO` function
from `System.Random` is such a function:

    randomRIO :: (Random a) => (a, a) -> IO a

This type signature fits the `GenActionR` type,

    randomRIO :: GenActionR IO

so we can pass it to `randomPlayer`:

    main :: IO ()
    main = randomPlayer randomRIO >>= print


Scott encoding
--------------

The regular list data type is defined as a sum type.  We will write a
custom version of it:

    data List a
        = Cons a (List a)
        | Nil

There are two ways to deconstruct this type in a principled fashion.
The first one is called pattern-matching, which means removing one layer
of constructors.  You could use the usual `case` construct to do this,
but it is syntactically heavy and does not compose well.  That's why we
like to write a function to do it:

    uncons :: (a -> List a -> r) -> r -> List a -> r
    uncons co ni (Cons x xs) = co x xs
    uncons co ni Nil         = ni

This function takes two *continuations* and a list.  The continuations
determine what we reduce the list into depending on which constructor is
found.  Here is a simple example:

    listNull :: List a -> Bool
    listNull = uncons (\_ _ -> False) True

When we find that the list is a cons, then we know that the list is not
empty, so we reduce it to `False`.  When we find that it is the nil, we
reduce it to `True`.  The following is a slightly more interesting
example, but you will find that it's really just pattern-matching in a
functional style:

    listMap :: (a -> b) -> List a -> List b
    listMap f =
        uncons (\x xs -> Cons (f x) (listMap f xs))
               Nil

So we have a way to construct lists by using the `List` constructors,
and we have a way to deconstruct lists by *unconsing*, by using the
pattern-matching combinator `uncons`.  However, this is actually an
indirection.  Interestingly a list is actually fully determined by what
happens when you uncons it.  That means we can represent a list in terms
of its uncons operator, which is called *Scott encoding* and requires a
rank-2 type:

    newtype ListS a =
        ListS {
          unconsS :: forall r. (a -> ListS a -> r) -> r -> r
        }

You may have noticed that the list argument is missing from `unconsS`,
but actually it is not.  It is implicit, because it is an accessor
function,

    unconsS :: ListS a -> (forall r. (a -> ListS a -> r) -> r -> r)

which is equivalent to:

    unconsS :: ListS a -> (a -> ListS a -> r) -> r -> r

The only difference is that the list argument has jumped to the front.
This type is sufficient to represent lists.  There is no reference to
the earlier defined `List` type.  How do we construct lists of this
type?  We just need to consider what happens when we pattern-match on
such a list.  For example unconsing the empty list would cause the nil
continuation to be used.  This is how we construct the empty list:

    nilS :: ListS a
    nilS = ListS (\co ni -> ni)

Now when we uncons this list, we give it two continuations.  It ignores
our cons continuation and just uses the nil continuation.  That's how
`nilS` represents the empty list.  The cons constructor is not much
different.  This time we ignore the nil continuation and apply the cons
continuation:

    consS :: a -> ListS a -> ListS a
    consS x xs = ListS (\co ni -> co x xs)

Let's write the mapping function for `ListS` to see it in action.  First
it is usually much more convenient to have the list argument be the last
argument to the uncons function, so let's write a custom combinator:

    unconsS' :: (a -> ListS a -> r) -> r -> ListS a -> r
    unconsS' co ni (ListS f) = f co ni

Okay, let's write the mapping function.  In fact this time let's do it
properly.  We will write a `Functor` instance instead of a standalone
function:

    instance Functor ListS where
        fmap f =
            unconsS' (\x xs -> consS (f x) (fmap f xs))
                     nilS

Compare this definition to `listMap` above.

You might ask why the `ListS` type actually requires rank-2
polymorphism.  Looking at the operators we have defined so far
everything seems to be rank-1.  However, we haven't had a closer look at
the `ListS` constructor itself:

    ListS :: (forall r. (a -> ListS a -> r) -> r -> r) -> ListS a

That's where the rank-2 type is hidden.


Church encoding
---------------

We have defined lists in terms of what happens when we uncons them.
Alternatively we can define lists in terms of what happens when we fold
them completely.  This is the second principled way to deconstruct
lists.  The fold combinator for lists is called the *right fold*:

    foldr :: (a -> r -> r) -> r -> [a] -> r

We know how to construct lists, and we know how to fold them.  But again
a list is fully determined by its fold, so we can *identify* it with its
fold.  This is called *Church encoding*.

    newtype ListC a =
        ListC {
          foldC :: forall r. (a -> r -> r) -> r -> r
        }

Notice the difference?  One interesting fact about Church encoding is
that the type recursion is gone, so we could use a plain old type alias
here.  We will prefer the safety of a separate type though, and also we
want our `Functor` instance.  Since this is a fold, it is actually easy
to write a mapping function:

    instance Functor ListC where
        fmap f = foldC' (\x xs -> consC (f x) xs) nilC

Notice again how the recursion is gone, not only on the type level, but
also on the value level, because the recursion is implicitly encoded in
the fold.

Exercise:  Write the uncons operator for `ListC`.  If you find this
surprisingly difficult, that's because it *is* surprisingly
difficult. =)


How `runST` works
-----------------

Let's step out of the rabbit hole for a moment and return to the real
world.  I promise that we will come back soon. =)

The `ST` type represents a family of monads for embedding a stateful
imperative program into a regular pure Haskell program safely.  `IO`
allows arbitrary effects, including observable side effects, so you
cannot run an IO action from within a pure program.  It takes one type
argument, the result type.  However, the `ST` type takes *two*
arguments.  An `ST` action might look like this:

    writeSTRef :: STRef s a -> a -> ST s ()

What is this extra argument `s`?  To find that out we have to have a
look at the big glue between the imperative `ST` world and the pure
Haskell world:

    runST :: (forall s. ST s a) -> a

This enforces that the `ST` action we would like to run satisfies two
requirements.  The first requirement is that `s` is fully polymorphic,
which is important because of the way `IO` is implemented in GHC, but
that's just an implementation detail we don't care about.  The main
restriction is that a higher rank quantified type will not be allowed to
leak out of its scope.  Only the result of type `a` is communicated out
of the action, but `s` is not communicated.  This allows the
type-checker to enforce that you cannot leak stateful resources out of
the `ST` action.  Everything the action does is fully deterministic and
repeatable.  This will make a lot more sense when we talk about the type
algebra later.


GADTs and continuation passing style
------------------------------------

This is your last chance.  After this there is no turning back.  Either
you close the browser tab, wake up in your chair and believe whatever
you want to believe; or you read on, you stay in Wonderland, and I show
you how deep the rabbit hole goes.

GHC supports type equality constraints, which are enabled when you turn
on the `TypeFamilies` extension:

    {-# LANGUAGE TypeFamilies #-}

We are not interested in type families in this article.  All we want is
those equality constraints, which enable you to write

    X ~ Y

in the context of a type.  This expresses that we require `X` and `Y` to
be the same type.  Example:

    15 :: Int                   -- Okay.
    15 :: (Char ~ Char) => Int  -- Okay.
    15 :: (Int ~ Int) => Int    -- Okay.
    15 :: (Char ~ Int) => Int   -- Type error!

The first expression is obviously well-typed.  The second expression is
well-typed, because `Char` is indeed equal to `Char`, so the constraint
is satisfied.  The third expression is also well-typed.  The fourth one
is ill-typed.  Since `Char` is not equal to `Int`, it results in a type
error.

This extension together with higher-rank polymorphism is actually
sufficient to encode types that are more general than what you can
normally define with algebraic data types.  They give us generalised
algebraic data types (GADTs) in continuation passing style.  Simple
example:

    {-# LANGUAGE GADTs #-}
    {-# LANGUAGE KindSignatures #-}

    data Some :: * -> * where
        SomeInt  :: Int -> Some Int
        SomeChar :: Char -> Some Char
        Anything :: a -> Some a

Nothing special here.  The magic happens when we pattern-match on values
of this type:

    import Data.Char

    unSome :: Some a -> a
    unSome (SomeInt x) = x + 3
    unSome (SomeChar c) = toLower c
    unSome (Anything x) = x

See what's going on in the `SomeInt` and `SomeChar` cases?  The function
is fully polymorphic in its type variable, yet somehow we managed to
convince the compiler that in the `SomeInt` case it's safe to add three
to whatever was in the value.  This is called *type refinement*.

The question we're interested in is:  In so many cases it is useful to
use Scott or Church encoding or some other form of continuation passing
style, but how can we actually do that?  Enter type equality
constraints:

    newtype SomeC a =
        SomeC {
          runSomeC ::
              forall r.
              ((a ~ Int) => Int -> r) ->
              ((a ~ Char) => Char -> r) ->
              (a -> r) ->
              r
        }

This may look a bit scary, but be brave!  Again we started with a sum
type, this time with three constructors, so again we have three
continuations corresponding to each of those constructors.  The first
continuation corresponds to the `SomeInt` constructor.  Let's look at it
more closely:

    (a ~ Int) => Int -> r

This continuation takes an `Int`.  Sure, that's the argument of the
constructor.  But it requests a second piece of information.  It demands
that whenever it is applied, it receives a proof that `a` is actually
equal to `Int`.

That's exactly what type refinement is!  When the user of a GADT
pattern-matches, they want to learn something new about the type
arguments of the type.  When the user of a Scott-encoded GADT
pattern-matches (passes a bunch of continuations), they expect to learn
something new as well, and they do by virtue of the type equality
constraint.

There we go -- GADTs without -XGADTs! =)


Dependent types
---------------

Thrilling title, isn't it?  Higher-rank polymorphism is in fact related
to dependent types, more specifically the dependent function arrow.  We
are still in Haskell, so our types cannot depend on values
([yet][dep-haskell]).  However, `RankNTypes` gives us *some* of the
expressivity.  In fact I have [demonstrated][dep-demo] that with a few
more extensions Haskell is as expressive as a full dependently typed
language.

[dep-demo]:    https://twitter.com/ertesx/status/500034598042996736
[dep-haskell]: https://www.youtube.com/watch?v=O805YjOsQjI

So which part does higher-rank polymorphism give us?  Let's see how we
would express polymorphism in a dependently typed language like Agda:

    id : {A : Set} → A → A
    id x = x

This syntax says that `id` is a function of two arguments.  The first
argument is a *type* (the type of types is called `Set` in Agda -- it
corresponds to the `*` kind in Haskell).  The second argument is the
value the function is going to give back.  The important thing to note
here is that the first argument, a type, is not used on the value level,
but it is used on the type level, *within* the type signature right
away.  In other words, the dependent function arrow brings the argument
itself into scope for the remainder of the type signature.  That's why
it can refer to that argument `A`.

The first argument is passed implicitly (that's the curly braces).  It
is inferred from the other arguments, if not explicitly given.  Agda
optionally allows us to write a quantification sign there:

    id : ∀ {A : Set} → A → A
    id x = x

Haskell on the other hand allows us to write explicit *kind* signatures
when we enable the `KindSignatures` extension:

    {-# LANGUAGE KindSignatures #-}

    id :: forall (a :: *). a -> a
    id x = x

Now these two definitions, the Agda and the Haskell one, look almost the
same, don't they?  That's because in fact they *are* the same.  Indeed,
`forall` is the dependent function arrow with the constraint that it can
only communicate types and what it communicates is *always* passed
implicitly.  So formally in Haskell the identity function is really a
function of two arguments, but one of them is always passed implicitly
by the type system (and has no run-time representation).  This makes it
even clearer that *the* identity function is really a whole family of
functions indexed by the type argument.

As a nice bonus Agda allows us to omit the type when it can be inferred
from context:

    id : ∀ {A} → A → A

And this looks very close to the Haskell version without the kind
signature:

    id :: forall a. a -> a

With this new insight we can explain more formally why `runST` is
defined the way it is.  Here is the equivalent definition in Agda:

    runST : ∀ {A} → (∀ {S} → ST S A) → A

The second (the first explicit) argument is actually a function that
receives the type `S` from `runST`.  However, it has no way to *return*
the type, because `S` cannot unify in any way with `A`.  That's
impossible, because the scope of `A` is broader than the scope of `S`.
In other words, the type `A` is determined before the type `S` is, so it
cannot in any way depend on `S`.


A useful quantifier law
-----------------------

This is a more formal section, which allows you to manipulate types with
quantifiers.  It explains some of the transformations we have done
earlier.  The phrase "for all" sounds a lot like it might actually come
from logic, and that is indeed the case.  Considering the Curry-Howard
correspondence the identity function is not just a handy function.  It
is also a proof:

    id :: forall a. a -> a

The type of `id` is a proposition, namely:  "a proof for $a$ implies a
proof for $a$, for all propositions $a$".  This sounds true, and it is.
The fact that you can write a total value of the given type is a proof
of the proposition.  Since there is a one-to-one correspondence between
types and propositions, we can transfer some of the laws as well.  The
most important one is the following, which is true for all `X` and `Y`:

    X -> forall a. Y a = forall a. X -> Y a

If this seems a bit cryptic, don't worry.  It really just means that as
long as a quantifier is on the right hand side of a function arrow, we
can pull it out and wrap the whole function type with the quantifier.
In fact we have already done this:

    type GenAction m = forall a. (Random a) => m a

    genRandom :: (RandomGen g) => GenAction (State g)

Let's expand the type alias, which gives us the following scary type:

    genRandom :: (RandomGen g) => (forall a. (Random a) => State g a)

Firstly the context arrow `(=>)` is really just another way to pass
implicit arguments via type classes.  So for the purpose of applying our
transformations we can simply read it like the regular function arrow
`(->)`.  Now we see that to the right hand side of the outer arrow is a
quantified type, so we can apply our rule from above and pull it out of
the arrow:

    genRandom :: forall a. ((RandomGen g) => ((Random a) => State g a))

I have added some parentheses for the sake of clarity, but they aren't
technically necessary, so we simply remove them now:

    genRandom :: forall a. (RandomGen g) => (Random a) => State g a

While Haskell allows it, it is uncommon in everyday code to have two
contexts passed separately.  Haskell simply merges them, so we can do
that as well:

    genRandom :: forall a. (Random a, RandomGen g) => State g a

Finally since this is a regular rank-1-polymorphic value, we can omit
the quantifier altogether:

    genRandom :: (Random a, RandomGen g) => State g a


Conclusion
----------

My experience is that `RankNTypes` is one of the least appreciated, most
confusing and most misunderstood extensions.  In fact I myself
originally thought that it's really only there to make `ST` safe.  I was
wrong, and very wrong.

Today I believe it is one of the most powerful and versatile extensions.
Even this article does not cover half of what you can do with it.  The
`lens` library many of us love so much would not nearly be as beautiful
without the power of higher-rank polymorphism. =)

I realise that this might be the longest post in this series, but I hope
that it was useful and to some of you even eye-opening.  Every little
step towards my mastery of this seemingly innocent extension felt like
an epiphany of its own, so I really wanted to share it.

Thank you for reading and happy holidays! =)

Ertugrul "ertes" Söylemez

----

*This post is part of
[24 Days of GHC Extensions](/pages/2014-12-01-24-days-of-ghc-extensions.html) -
for more posts like this, check out the
[calendar](/pages/2014-12-01-24-days-of-ghc-extensions.html)*.
