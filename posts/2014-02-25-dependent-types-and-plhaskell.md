---
title: "Interpreting Haskell Inside PostgreSQL With Singleton Types"
---

I am currently in the early stages of a new project,
[`plhaskell`](http://github.com/ocharles/plhaskell). `plhaskell` is a
[language handler](http://www.postgresql.org/docs/current/static/sql-createlanguage.html)
for PostgreSQL that will allow you to write PostgreSQL functions in Haskell. The
project has already taught me a lot, but recently I've had a somewhat mind
bending encounter with what is almost dependently typed programming in Haskell -
and I've just got to share it with you.

Here's the problem. At runtime, we have a string containing Haskell code,
metadata about the function call which includes the function signature, and a
list of argument values. Is it possible to interpret this string as a Haskell
function and call it with the provided arguments?

Let's start by interpreting the string. The `hint` library provides a high-level
interface to GHC's API, which allows us to interpret strings as values of a
given Haskell type. The most useful function for us is:

```haskell
interpret :: Typeable a => String -> a -> m a
```

Given a `String` containing Haskell code and an expected type, `interpret` will
attempt to construct a value of that type by interpreting the given string. We
can run the interpreter using `runInterpreter`. For example:

```
> runInterpreter $ setImports ["Prelude"] >> interpret "True" (as :: Bool)
Right True

> runInterpreter $ setImports ["Prelude"] >> interpret "True" (as :: Int)
Left (WontCompile [GhcError {errMsg = "
Couldn't match expected type `GHC.Types.Int'
            with actual type `GHC.Types.Bool'"}])
```

We're making good progress, but now we hit a problem. Notice that we are
deciding the type that `interpret` should return statically, at compile
time. The problem statement above indicates that the type information isn't
known at compile time, we only learn about that at runtime. At first this seems
impossible - after all, Haskell is all about static types!

The dependent types crowd are laughing at this point. Surely we just want to
write a function with this type:

```haskell
interpret :: String -> (sig : Signature) -> (FunctionType sig)
```

Alas, we can't write this in Haskell. Or can we?

## A Type of Types

A good first step is to clear up what we mean by "types". PostgreSQL has an open
universe of types, as types can be defined at runtime. However, we'll keep
things a little simpler by considering a closed universe of the following types:

```haskell
data PgType = PgInt | PgString
```

Combining GHC's new(ish) abilities to promote data types along with type
families, we can form a mapping from PostgreSQL types to Haskell types:

```haskell
type family InterpretType (t :: PgType) :: *
type instance InterpretType 'PgInt = Int
type instance InterpretType 'PgString = String
```

The type family takes types of kind `PgType` to types of kind `*` - the kind of
types of Haskell values. It's important to remember that this is all at the type
level - we transform the *type* `PgInt` to the *type* `Int`. Working at the
type level is not enough to solve the overarching problem, because runtime
information is at the value level. In order to link types and values, we can use
*singleton types*.

Intermediate Haskell programmers should be familiar with phantom types - type
variables whose sole purpose is to carry type information. Singleton types work
in a similar way - we add a type parameter to carry information up from values
to their types. For `PgType`, there is a corresponding singleton type:

```haskell
data SPgType :: PgType -> * where
  SPgInt :: SPgType 'PgInt
  SPgString :: SPgType 'PgString
```

By using a GADT, we can now learn more information about our types by pattern
matching. For example, we can write a function who's return type depends on the
value of the first parameter:

```haskell
exampleValues :: SPgType t -> InterpretType t
exampleValues SPgInt = 42
exampleValues SPgString = "Forty two"
```

At first glance this looks like nonsense, but you should take time to convince
yourself that it makes sense. If you give `exampleValues` a `SPgInt`, then by
pattern matching on that we learn that the `t` in the type signature is actually
a `'PgInt`. The type family `InterpretType` maps `PgInt` to `Int`, thus 42
is a perfectly valid return type. Similar reasoning shows that `"Forty two"` is
equally valid, provided we have evidence that `t ~ 'PgString` (this is notation
to indicate type equality).

It may help to play around with this in GHCI:

```
> :t exampleValues SPgString
exampleValues SPgString :: InterpretType 'PgString

> :t exampleValues SPgInt
exampleValues SPgInt :: InterpretType 'PgInt

> :kind! InterpretType 'PgInt
InterpretType 'PgInt :: *
= Int

```

## From Types to Function Arrows

So far, we have only considered working with single types. However, a function
contains multiple types - one for each argument and another for the return
type. We need to introduce a data type to capture this. We make the observation
that any function has *at least one* type, so we introduce a variant of
non-empty lists:

```haskell
data PgFunction
  = PgReturn PgType
  | PgArrow PgType PgFunction
```

This data type is also amenable to promotion to the type level, so we can
map entire `PgFunction`s to their corresponding Haskell function type:

```haskell
type family InterpretFunction (t :: PgFunction) :: *
type instance InterpretFunction ('PgReturn t) = InterpretType t
type instance InterpretFunction ('PgArrow t ts) = InterpretType t -> InterpretFunction ts
```

This type family is essentially folding `PgFunction` down to a function type -
we replace `PgArrow` with `->`, and all `PgType`s with their interpretation.

Not only can we do promotion to the type level, we can play a similar trick with
singleton types:

```haskell
data SPgFunction :: PgFunction -> * where
  SPgReturn :: SPgType t -> SPgFunction ('PgReturn t)
  SPgArrow :: SPgType t -> SPgFunction ts -> SPgFunction ('PgArrow t ts)
```

This singleton type has singleton constructors that make use of the singleton
types for `PgType`. To try and get a better handle on what this gives us, here's
a similar set of examples, but this time for functions:

```haskell
exampleFunctions :: SPgFunction t -> InterpretFunction t
exampleFunctions (SPgReturn SPgInt) = 42
exampleFunctions (SPgArrow SPgString (SPgReturn SPgInt)) = length
```

```
> exampleFunctions (SPgReturn SPgInt)
42

> exampleFunctions (SPgArrow SPgString (SPgReturn SPgInt)) "I <3 Types"
10
```

## The `singletons` Library

Hopefully you now have a rudimentary understanding of singleton types. Writing
out singleton types by hand is both tedious and error prone, especially when the
translation is purely
mechanical. [Richard Eisenberg](http://typesandkinds.wordpress.com/) is the
author of the [`singletons`](http://hackage.haskell.org/package/singletons)
library, which contains Template Haskell to do this for us. We'll now switch to
the following source code:

```haskell
$(singletons [d|
  data PgType = PgInt | PgString

  data PgFunction = PgReturn PgType | PgArrow PgType PgFunction
  |])

-- These are exactly as before
type family InterpretType (t :: PgType) :: *
type family InterpretFunction (t :: PgFunction) :: *
```

## Call Yourself Typeable? Prove it!

We're a good part of the way towards our goal now. To recap, we have defined a
type of PostgreSQL types, and we can use these types to represent function
signatures. Singleton types let us work at both the value and the type level,
and type families let us turn these PostgreSQL types into their corresponding
Haskell types.

Let's recap the type of `interpret`:


```haskell
interpret :: Typeable a => String -> a -> m a
```

It would appear we have all the machinery we need now to construct that type
`a`. Given a `SPgFunction` we can use `InterpretFunction` to decide the return
type, and we can use the magic `undefined` value to provide a value. (Really we
should be using `Proxy`, but we have to work with what `hint` wants):

```
funType :: SPgFunction t -> InterpretFunction t
funType (SPgReturn _) = undefined
funType (SPgArrow _ ts) = \_ -> funType ts
```

If we have `SPgReturn` then we use `undefined` as our value. Otherwise, we need
to form a function, which we can do by simply ignoring the argument and
recursing with `funType`.

So far we've been working with singleton types, but we won't be constructing
these directly at runtime - rather we'll be constructing `PgFunction`
values. It's easy to get a singleton out of this though, by using the
`withSomeSing` combinator in the singletons library.

Brimming with excitement, we hack out a bit of code to try this all out!

```haskell
firstAttempt :: String -> PgFunction -> IO ()
firstAttempt code sig = withSomeSing sig $ \s ->
  runInterpreter $ do
    setImports [("Prelude")]
    interpret code (funType s)

  putStrLn "It didn't crash!"
```

```
    No instance for (Typeable (InterpretFunction a))
      arising from a use of `interpret'
    Possible fix:
      add an instance declaration for (Typeable (InterpretFunction a))
    In a stmt of a 'do' block: interpret code (funType s)
```

Unfortunately, `hint` is requiring that there is a `Typeable` instance for `a`,
but there is no evidence that our interpretation of PostgreSQL types as a
Haskell function will by `Typeable`. Of course, *we know* it is - it's made up
from `String`, `Int` and `(->)`, all of which are `Typeable`. How do we prove to
GHC that we really do have a `Typeable` instance?

Edward Kmett
[blogged about a useful trick](http://comonad.com/reader/2011/what-constraints-entail-part-1/)
in 2011 (more information on this at
[the Joy of Types](http://joyoftypes.blogspot.co.uk/2012/02/haskell-supports-first-class-instances.html))
which is exactly what we need. If we define the following data type...

```haskell
data Dict a where
  Dict :: a => Dict a
```

Then we get *first class* type instances! By having first class type instances,
we can pass them around as values. Furthermore, pattern matching on them teaches
us about our types - specifically teaching us that certain type class instances
are present.

To get our feet wet, we can show that all interpretations of single PostgreSQL
types are `Typeable`:

```haskell
pgTypeTypeable :: SPgType t -> Dict (Typeable (InterpretType t))
pgTypeTypeable SPgInt = Dict
pgTypeTypeable SPgString = Dict
```

Admittedly, it's a rather queer function. However, we can make use of this as a
lemma to build an inductive proof that all interpretations of PostgreSQL
*functions* are `Typeable`. Our base case is that a function of no arguments is
`Typeable`. Our inductive hypothesis is that a function of *n* arguments
`Typeable`, and we can use this to show that a function of *n+1* arguments is
also `Typeable`:

```haskell
pgFunTypeTypeable :: SPgFunction t -> Dict (Typeable (InterpretFunction t))
pgFunTypeTypeable (SPgReturn t) = pgTypeTypeable t
pgFunTypeTypeable (SPgArrow t ts) =
  case pgTypeTypeable t of
    Dict ->
      case pgFunTypeTypeable ts of
        Dict -> Dict
```

I found `pgTypeTypeable` mind-bending, but this is mind-bending in an extra
dimension. However! It does actually do the job, as we're now ready to finally
reach our goal, and the following function type checks:

```haskell
goal :: String -> PgFunType -> IO ()
goal code signature = withSomeSing signature $ \s ->
  case pgFunTypeTypeable s of Dict ->
    f <- runInterpreter $ do
        setImports [("Prelude")]
        interpret code (funType s)

    case f of
      Left error ->
        print error

      Right f' ->
        -- Apply arguments to f'
        return ()
```

It's not a particularly *useful* goal, because it doesn't actually call `f`. To
do so, we need a supply of arguments. The topic for this post was to demonstrate
how to create the function type, so we'll just cheat here and use
`Data.Dynamic`. (In `plhaskell`, we recurse through the `SPgFunction` and peek
into pointers that PostgreSQL provides). See the
[full code listing](https://github.com/ocharles/blog/blob/master/code/2014-02-25-dtypes-2.hs)
for source code to the below REPL session:

```
> final "42" (PgReturn PgInt) []
42

> final "(+)" (PgArrow PgInt (PgArrow PgInt (PgReturn PgInt)))
>   [ toDyn (1 :: Int), toDyn (2 :: Int) ]
3

> final "\"Hello\"" (PgArrow PgInt (PgReturn PgInt)) []
WontCompile [GhcError {errMsg = "Couldn't match expected type `GHC.Types.Int -> GHC.Types.Int'
                                             with actual type `[GHC.Types.Char]'"}]
```

Voilà.

## Conclusion & Acknowledgements

Dependent types are fantastic, but you don't have to switch languages to benefit
from them. When combined with the `singletons` library, GHC 7.6 can do a
significant amount of work that only seems possible in Agda or Idris. With all
of that said, it's not without pain points. Other languages that are
intentionally dependently typed don't need singletons, and a lot of this will
feel a lot more natural.

I want to send a massive thank you out to Richard Eisenberg - once for his work
on the `singletons` library and associated research, but also for providing a
last bit of hand-holding and teaching me the trick with `Dict` to get `Typeable`
working.

To close with, we can dispel one more myth:

```haskell
ocharles> @faq Can I use Haskell with fake dependent types to interpret code with type
information at runtime?
lambdabot> The answer is: Yes! Haskell can do that.
```

I love this language.
