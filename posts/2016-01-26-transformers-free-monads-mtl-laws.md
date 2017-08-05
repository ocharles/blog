---
title: "Monad transformers, free monads, mtl, laws and a new approach"
---

If you've been following the hot topics of Haskell over the last few years,
you'll probably have noticed a lot of energy around the concepts of effects. By
effects, we are generally talking about the types of computations we traditionally
express using monads in Haskell -- IO, non-determinism, exceptions, and so on.
I believe the main reason that this has been a popular topic is that none of the
existing solutions are particularly nice. Now "nice" isn't a particularly
well defined concept, but for something to fit in well with Haskell's philosophy
we're looking for a system that is:

1. **Extensible**. The approach we take should be *open*, allowing us to define
   new effects.
2. **Composable**. It should be possible to mix different effects with well
   defined, predictable behaviour.
3. **Efficient**. We should only have to pay a minimal cost for the use of the
   abstraction.
4. **Terse**. Haskell is generally not verbose, and whatever system we use should
   allow us to avoid excessive verbosity. The system should work with us, we
   should not have to work for it.

I would also add in a 5th point

5. **Inferable**. Type annotations should not be required for successful
   compilation.

With this list in mind, what are the current solutions, and how do they measure
up?

## Monad Transformers

Starting with the most basic, we can simply choose a concrete monad that
does everything we need and work entirely in that -- which is usually going to be
`IO`. In a sense this is composable -- certainly all programs in one
monad compose together -- but it's composable in the same sense that dynamically
typed languages fit together. Often choosing a single monad for each individual
computation is too much, and it becomes very difficult to work out exactly what
effects are being used in our individual functions: does this computation use
`IO`? Will it throw exceptions? Fork threads? You don't know without reading the
source code.

Building a concrete monad can also be a lot of work. Consider a computation that
needs access to some local state, a fixed environment and arbitrary `IO`. This
has a type such as

```haskell
newtype M a = M (Environment -> State -> IO (a, State))
```

However, to actually interact with the rest of the Haskell ecosystem we need to
define (at least) instances of `Functor`, `Applicative` and `Monad`. This is
boilerplate code and entirely determined by the choice of effects -- and that
means we should strive to have the compiler write it for us.

To combat this, we can make use of monad transformers. Unlike monads, monad
transformers compose, which means we can build larger monads by stacking a
collection of monad transformers together. The above monad `M` can now be
defined using off-the-shelf components, but crucially we can derive all the
necessary type classes in one fell swoop with the `GeneralizedNewtypeDeriving`
language extension

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

newtype M a = M (ReaderT Environment (StateT State IO) a)
  deriving (Functor, Applicative, Monad)
```

This saves typing considerably, and is a definite improvement. We've achieved
more of points 1 and 2 (extenability and composability) by having both programs
*and* effects compose. Point 4 (terseness) is improved by the use of
`GeneralizedNewtypeDeriving`. There is a slight risk in terms of efficiency, but
I believe if `transformers` would just `INLINE` a few more definitions, the cost
can be entirely erased. All of this code will infer as we'd expect, as we're
working entirely with explicit types

However, while we had to type less to *define* the effects, we have to type more
to *use* the effects! If we want to access the environment for example, we can use
the `ask` operation from `Control.Monad.Trans.Reader`, but we have to wrap this
up in the `M` `newtype`:

```haskell
env :: M Environment
env = M ask
```

However, if we want to retrieve the current state in the computation, we can use
`get` from `Control.Monad.Trans.State`, but we also have to `lift` that into the
`ReaderT` monad that is wrapping `StateT`:

```haskell
currentState :: M State
currentState = M (lift get)
```

This is unfortunate -- `lift` is mostly noise that we don't want to be concerned
with. There is also the problem in that the amount of `lift`s to perform is tied
directly to the underlying definition of `M`. If I later decide I want to layer
in the chance of failure (perhaps with `MaybeT`), I now have to change almost
*all* code using `lift`, by adding an extra one in!

`lift` is a mechanical operation that is determined by the type of monad
transformer stack and the operation that we want to perform. As we noted, for
different stacks, the amount of `lift`ing will vary, but it is determined by the
type of stack. This suggests that these `lift`s could be inferred by the use of
type classes, and this is the purpose of the monad transformer library -- `mtl`.

## The Monad Transformer Library (mtl)

The `mtl` is a library consisting of type classes that abstract over the
operations provided by each monad transformer. For `ReaderT`, we have the `ask`
operation, and likewise for `StateT` we have `get` and `put` operations. The
novelty in this library is that the instances for these type classes are defined
inductively over monad transformer stacks. A subset of the instances for
`MonadReader` for example, show

```haskell
class MonadReader r m | m -> r where
  ask :: m r

instance Monad m => MonadReader r (ReaderT r m) where
  ask = Control.Monad.Trans.ReaderT.ask

instance (MonadReader r m) => MonadReader r (StateT s) where
  ask = lift ask
```

We can read this as:

- a *base case* if the outermost transformer is `ReaderT`, in which case no
  `lift`ing has to be performed.

- an *inductive case*, stating that *if* we know there is a `MonadReader`
  instance somewhere within the stack (that is, somewhere in the stack we are
  using `ReaderT`), then the outer monad transformer (in this case `StateT`) is
  also an instance of `MonadReader` by simply passing those operations through
  to the underlying instance via one application of `lift`.

With these instances the lifting now becomes automatic entirely at the use of
the respective operations. But not only does it become easier to use the
operations, our programs also become more generic and easier to reason about.
For example, while `env` previously had the type `M Environment`, it could now
generalise to simply

```haskell
env :: (MonadReader Environment m) => m Environment
env = ask
```

Stating that `env` is reusable in *any* computation that has access to
`Environment`. This leads to both more options for composition (we're not tied
to working in `M`), but also types that are more expressive of what effects are
actually being used by the computation. In this case, we didn't use `StateT`, so we
didn't incur a `MonadState` type class constraint on `m`.

Type classes open up a risk of losing type inference, and the approach in `mtl`
is to use functional dependencies. `mtl` makes use of functional
dependencies in order to retain type inference, but this comes at a
compositional cost -- the selected effect proceeds by induction from the outer
most monad transformer until we reach the *first* matching instance. This means
that even if there are multiple possible matches, the first one encountered will
be selected. The following program demonstrates this, and will fail to type
check:

```haskell
getTheString :: ReaderT Int (ReaderT String IO) String
getTheString = ask
```

```
    Couldn't match type ‘Int’ with ‘[Char]’
    arising from a functional dependency between:
      constraint ‘MonadReader String (ReaderT Int (ReaderT String IO))’
        arising from a use of ‘ask’
```

When we used `ask` induction proceeded from the outermost transformer - `ReaderT
Int`. This is an instance of `MonadReader`, and due to the functional dependency
will be selected even though it doesn't contain the `String` that we're looking
for. This manifests as a type error, which can be frustrating.

In practice, I'm not convinced this is really a problem, but in the scenario
where environments don't match up we have a few options:

1. Adapt the environment with tools like `mapReaderT` or `magnify` (from
   `lens`).

2. Use `monad-classes` which uses a little more type level computation to allow
   this to work. I'm not entirely sure what the story for inference is here, but
   I think there *may* be a risk of less inference.

3. Forgo the functional dependencies, as in `mtl-unleashed`. In this case you
   really do give up type inference, and I don't consider it a viable option (it
   fails to satisfy point 5 in my criteria in the intro).

Interestingly, the generality we gained by being polymorphic over our choice of
monad also opens the door to something we couldn't do with monad transformers,
which is to choose a different implementation of the type class. For example,
here's a different implementation of `MonadReader` for `M`:

```haskell
instance MonadReader Environment M where
  ask = do
    env <- M ask
    liftIO (putStrLn "Requesting environment")
    liftIO (putStrLn ("It is currently " ++ show env)
    return env
```

While a slightly contrived example, we see that we now have the ability to
provide a different interpretation for `ask` which makes use of the underlying
`IO` in `M` by logging whenever a computation looks at the environment. This
technique is even more useful when you start defining domain specific effects,
as it gives you the option to provide a pure variant that uses mock data, which
can be useful for unit testing.

## Free monads

Let's move away from monad transformer stacks and see what the other options are.
One option that's getting a lot of attention is the use of *free monads*. A
free monad is essentially a type of construction that adds just enough structure
over some data in order to have the structure of a monad -- and nothing extra. We
spend our days working with monads, and the reason the approach afforded by free
monads is appealing is due to the way that we build them -- namely, we just
specify the syntax! To illustrate this, let me the consider the almost
traditional example of free monads, the syntax of "teletype" programs.

To begin with, I have to define the syntax of teletype programs. These programs
have access to two operations - printing a line to the screen, and reading a
line from the operator.

```haskell
data TeletypeF a = PrintLine String a
                 | GetLine (String -> a)
  deriving (Functor)
```

This functor defines the syntax of our programs - namely programs that read and
write to the terminal. The parameter `a` allows us to chain programs together,
such as this `echo` program that prints whatever the user types:

```haskell
echo :: TeletypeF (TeletypeF ())
echo = GetLine (\line -> PrintLine line ())
```

However, this is kind of messy. The free monad construction allows us to
generate a monad out of this functor, which provides the following presentation:

```haskell
echo :: Free TeletypeF ()
echo = do
  l <- getLine
  printLine l

getLine :: Free TeletypeF String
getLine = liftF (GetLine id)

printLine :: String -> Free TeletypeF ()
printLine l = liftF (PrintLine l ())
```

This definition of `echo` looks much more like the programs we are used to writing.

The remaining step is to provide an interpretation of these programs, which
means we can actually run them. We can interpret our teletype programs by using
`STDOUT` and `STDIN` from `IO`:

```haskell
runTeletype :: Free TeletypeF a -> IO a
runTeletype =
  iterM (\op ->
           case op of
             GetLine k -> readLine >>= k
             PrintLine l k -> putStrLn l >> k)
```

This rather elegant separation between syntax and semantics suggests a new
approach to writing programs -- rather than working under a specific monad, we
can instead work under a free monad for some suitable functor that encodes all
the operations we can perform in our programs.

That said, the approach we've looked at so far is not particularly extensible
between different classes of effects, as everything is currently required to be
in a single functor. Knowing that free monads are generated by functors, we can
start to look at the constructions we can perform on functors. One very nice
property of functors is that given *any* two functors, we can compose them. The
following functors below witness three possible ways to compose functors:

```haskell
data Sum f g a = InL (f a) | InR (g a) deriving (Functor)
data Product f g a = Product (f a) (g a) deriving (Functor)
data Compose f g a = g (f a) deriving (Functor)
```

Assuming `f` and `g` are `Functor`s, all of these are also `Functor`s - which
means we can use them to build monads with `Free`.

The most interesting of these constructions (for our purposes) is `Sum`, which
lets us choose between two different `Functor`s. Taking a more concrete example,
I'll repeat part of
[John A. De Goes "Modern FP"](http://degoes.net/articles/modern-fp) article. In
this, he defines two independent functors for programs that can access files in
the cloud, and another for programs that can perform basic logging.

```haskell
data CloudFilesF a
  = SaveFile Path Bytes a
  | ListFiles Path (List Path -> a)
  deriving (Functor)

data LoggingF a
  = Log Level String a
  deriving (Functor)
```

Both of these can now be turned into monads with `Free` as we saw before, but we
can also combine both of these to write programs that have access to both the
`CloudFilesF` API *and* `LoggingF`:

```haskell
type M a = Free (Sum CloudFilesF LoggingF) a
```

However, in order to use our previous API, we'll have to perform another round of lifting:

```haskell
-- API specific to individual functors
log :: Level -> String -> Free LoggingF ()
log l s = liftF (Log l s ())

saveFile :: Path -> Bytes -> Free CloudFilesF ()
saveFile p b = lift (SaveFile p b ())

-- A program using multiple effects
saveAndLog :: Free (Sum CloudFilesF LoggingF) ()
saveAndLog = do
  liftLeft (log Info "Saving...")
  liftRight (saveFile "/data" "\0x42")

-- Lifting operations
liftLeft :: Free f a -> Free (Sum f g) a
liftLeft = hoistFree InL

liftRight :: Free g a -> Free (Sum f g) a
liftRight = hoistFree InR
```

This is a slightly unfortunate outcome - while we've witnessed that there is
extensiblity, without more work the approaches don't compose particularly well.

To solve the problem of having to lift everything leads us to the need for an
`mtl`-like solution in the realm of free monads - that is, a system that
automatically knows how to lift individual functors into our composite functor.
This is essentially what's happening in the `extensible-effects` library - as a
user you define each individual `Functor`, and then `extensible-effects`
provides the necessary type class magic to combine everything together.

We should also mention something on efficiency while we're here. Free monads
have at least two presentations that have different use cases. One of these is
extremely easy to inspect (that is, write interpters) but has a costly
implementation of `>>=`. We know how to solve this problem, but the trade off
switches over to being costly to inspect. Recently, we learnt how to perform
reads and binds in linear time, but the constant factors are apparently a little
too high to be competative with raw `transformers`. So all in all, there is an
efficiency cost of *just working with* a free monad approach.

## `mtl` and laws

I want to now return to the monad transformer library. To recap, the
definition of `MonadReader` is --

```haskell
class MonadReader r m | m -> r where
  ask :: m r
```

But this alone makes me a little uneasy. Why? I am in the class of Haskellers
who consider a type class without a law a smell, as it leaves us unable to
reason about what the type class is even doing. For example, it doesn't require
much imagination to come up with nonsense implementations of `ask`:

```haskell
newtype SomeM a = SomeM (StateT Int IO a)
  deriving (Functor, Applicative, Monad)

instance MonadReader Int SomeM where
  ask = SomeM $ do
    i <- get
    put (i + 1)
    return i
```

But then again -- who's to say this is nonsense? Given that we were never given a
specification for what `ask` should do in the first place, this is actually
perfectly reasonable! For this reason, I set out searching for a way to reason
about `mtl`-style effects, such that we could at least get *some* laws.

## A different approach

The `transformers` library also give us `mtl`-like type classes, one of which is
`MonadIO`. However, this type class does have laws as well:

```haskell
-- liftIO . return = return
-- liftIO (f >>= g) = liftIO f >>= liftIO . g
class MonadIO m where
  liftIO :: IO a -> m a
```

This law is an example of a *homomorphism*. To quote
[Wikipedia on the subject](https://en.wikipedia.org/wiki/Homomorphism):

> A homomorphism is a structure-preserving map between two algebraic structures
> (such as groups, rings, or vector spaces).

In this case the algebraic structure is the monad structure of `IO`. We see that
any monad that is an instance of `MonadIO` has the ability to lift `IO`
operations, and as this is a homomorphism, the laws state that it will preserve
the underlying structure of `IO`.

It's currently unclear how to apply this type of reasing to `MonadReader`, given
its current definition -- `ask` is just a value, it doesn't even take an
argument -- so how can we even try and preserve anything?

Let's take some inspiration from free monads, and consider the effect language
for `MonadReader`. If we only have `(Monad m, MonadReader r m)`, then the only
thing we can do on top of the normal monad operations is `ask` the environment.
This suggests a suitable functor would be:

```haskell
data AskF r a = Ask (r -> a)
  deriving (Functor)
```

I can now wrap this up in `Free` in order to write programs with the ability to
`ask`:

```haskell
type Ask r a = Free (AskF r) a
```

Now we have an algebraic structure with properties (`Ask r` is a `Monad`) that
we would like to preserve, so we can write this alternative form of
`MonadReader`:

```haskell
-- liftAsk . return = return
-- liftAsk (f >>= g) = liftAsk f >>= liftAsk . g
class Monad m => MonadReader r m | m -> r where
  liftAsk :: Ask r a -> m a

ask :: MonadReader r m => m r
ask = liftAsk (liftF (Ask id))
```

Et voilà! We now have an equally powerful `MonadReader` type class, except this
time we have the ability to reason about it and its instances. If we return to
the instance that I was questioning earlier, we can redefine it under the new
API:

```haskell
instance MonadReader Int SomeM where
  liftAsk askProgram = SomeM $ do
    x <- get
    out <- iterM (\(Ask k) -> return (k t)) askProgram
    put (x + 1)
    return out
```

Now that we have some laws, we can ask: is this a *valid* definition of
`MonadReader`? To check, we'll use equational reasoning. Working through the
first law, we have

```
liftAsk (return a)
  = { definition of return for Free }
liftAsk (Pure a)
  = { definition of liftAsk for SomeM }
SomeM $ do
  x <- get
  out <- iterM (\(Ask k) -> return (k t)) (Pure a)
  put (x + 1)
  return out
  = { evaluate iterM for Pure a }
SomeM $ do
  x <- get
  out <- return a
  put (x + 1)
  return out
  = { monad laws }
SomeM $ do
  x <- get
  put (x + 1)
  return a
```

Already we have a problem. While we can see that this does return the original
`a` it was given, it does so in a way that also incurred some side effects. That
is, `liftAsk (return a)` is *not* the same as `return a`, so this isn't a valid
definition of `MonadReader`. Back to the drawing board... Now, it's worth noting
that there is an instance that *is* law abiding, but might still be considered
as surprising:

```haskell
instance MonadReader Int SomeM where
  liftAsk askProgram =
    iterM (\(Ask k) -> SomeM $ do
      x <- get
      put (x + 1)
      k x )
```

Applying the same equational reasoning to this is much easier, and shows that
the first law is satisfied

```
liftAsk (return a)
  = { definition of liftAsk }
iterM (\(Ask k) -> SomeM $ do
  x <- get
  put (x + 1)
  k x)
  (return a)
  = { definition of return for Free }
iterM (\(Ask k) -> SomeM $ do
  x <- get
  put (x + 1)
  k x)
  (Pure a)
  = { definition of iterM given Pure}
return a
```

For the second law, I'll omit the proof, but I want to demonstrate to sessions
in GHCI:

```
> let runSomeM (M m) = evalState m 0

> runSomeM (liftAsk (ask >>= \r1 -> ask >>= \r2 -> return (r1, r2))
(1, 2)

> runSomeM (liftAsk ask >>= \r1 -> liftAsk >>= \r2 -> return (r1, r2)
(1, 2)
```

So while the answers agree - they probably don't agree with your intuition! This
is only surprising in that we have some assumption of how =Ask= programs should
behave. Knowing more about =Ask=, we might seek this further law:

> `ask >> ask = ask`

This law can also be seen as a reduction step in the classification of our `Ask`
programs, but a `Free` monad is not powerful enough to capture that. Indeed, the
documentation of `Free` mentions exactly this:

> A free `Monad` is one that does no work during the normalisation step beyond
> simply grafting the two monadic values together. `[]` is not a free `Monad`
> (in this sense) because `join [[a]]` smashes the lists flat.

The law `ask >> ask = ask` follows by normalisation of our "reader" programs, so
a free monad will be unable to capture that by construction -- the best we can do
is add an extra law to our type class. However, what we can also do is play a
game of
[normalisation by evaluation](http://gallium.inria.fr/blog/lawvere-theories-and-monads/).
First, we write an evaluator for `Free (AskF r)` programs:

```haskell
runAsk :: Free (AskF r) a -> (r -> a)
runAsk f r = iterM (\(AskF k) -> k r) f
```

and then witness that we can reify these `r -> a` terms back into
`Free (Ask r) a`:

```haskell
reify :: (r -> a) -> Free (Ask r) a
reify = AskF
```

You should also convince yourself that `(r -> a)` really is a normal form, and
you may find the above linked article on this useful for formal proofs (search for
"normalisation"). What we've essentially shown is that *every* `Free (AskF r) a`
program can be expressed as a single `r -> a` function. The normal form of `ask
>> ask` is now - by definition - a single `ask`, which is the law we were
originally having to state.

As we've witnessed that `r -> a` is the normal form of `Free (AskF r) a`, this
suggests that we could just as well write:

```haskell
-- liftAsk . return = return
-- liftAsk (f >>= g) = liftAsk f >>= liftAsk . g
class MonadReader r m | m -> r where
  liftAsk :: (r -> a) -> m a
```

(The structure being preserved by the homomorphism is assuming that `(r -> a)`
is a reader monad).

Our strange instance now becomes

```haskell
instance MonadReader UTCTime SomeM where
  liftAsk f = SomeM $ do
    x <- get
    put (x + 1)
    return (f x)
```

With a little scrutiny, we can see that this is not going to satisfy the
homomorphism laws. Not only does it fail to satisfy the `return` law (for the
same reason), the second law states that `liftAsk (f >>= g) = liftAsk f >>=
liftAsk . g`. Looking at our implementation this would mean that we would have
to increase the state based on the amount of binds performed in `f >>= g`.
However, we also know that `>>=` for `r -> a` simply reduces to another `r -> a`
function - the implication being that it's impossible to know how many binds
were performed.

Here a counter example will help convince us that the above is wrong. First, we know

> `liftAsk (ask >> ask) = liftAsk ask`

because `ask >> ask = ask` by definition.

By the homomorphism laws, we must also have

> `liftAsk (ask >> ask) = liftAsk ask >> liftAsk ask`

Combining these, we expect

> `liftAsk ask = liftAsk (ask >> ask) = liftAsk ask >> liftAsk ask`

However...

```
> runSomeM (liftAsk ask)
1

> runSomeM (liftAsk (ask >> ask))
1

> runSomeM (liftAsk ask >> liftAsk ask)
2
```

Now we can see that `SomeM`'s current definition of `MonadReader` fails. It's
much harder to write a law abiding form of `MonadReader Int SomeM` - but it
will essentially require some *fixed* data throughout the scope of the
computation. The easiest is of course to change the definition of `SomeM`:

```haskell
newtype SomeM a = SomeM (ReaderT Int IO a)

instance MonadReader UTCTime SomeM where
  liftAsk f = SomeM (fmap f ask)
```

You should convince yourself that this instance is now law abiding - for example
by considering the above counter-example, or by performing equational reasoning.

## A pattern for effect design

The process we underwent to reach the new form of a =MonadReader= type class,
extends well to many different type classes and suggests a new pattern for
`mtl`-like type class operations. Here's a rough framework that I'm having a lot
of success with:

### 1. Define the operations as data

To begin, think about the language that your effect will talk about. For the reader
monad, we defined the `AskF` functor, and the same can be done for the exception
monad, the failure monad, the state monad, and so on. For more "domain specific"
operations, a free monad also scales well - one could imagine a language for
interacting with general relational databases, with operations to `SELECT`,
`UPDATE`, `DELETE`, and so on.

### 2. Find a suitable way to compose operations

Individual operations are not enough, we also need a way to write programs using
this language. This amounts to finding a suitable way to compose these
operations together. An easy first approximation is to use a free structure,
again -- as we started with for the reader monad. In the case of the
aforementioned domain specific relational database example, the free monad might
be as far as we want to go.

It's also worth exploring if there is a normal form that more succinctly
captures the operations in your language along with equational reasoning. We saw
that the normal form of `Free (AskF r) a` was `r -> a`, and the same process can
be ran for `Free (StateF s) a` - reaching `s -> (a, s)` as a normal form. It's
important to note that if you go through the process of normalisation by
evaluation, that you also make sure you can reify your evaluation
result back into the original language. To illustrate why, consider the
hypothetical relational database language:

```haskell
data DatabaseF a = Query SqlQuery (Results -> a)

runDb :: Free DatabaseF a -> (DatabaseHandle -> IO a)
runDb h = iterM (\(Query q k) -> query h q >>= k)
```

This is fine for an interpreter, but `DatabaseHandle -> IO a` is not a normal
form because we can't reify these terms *back* into `DatabaseF`. This is
important, because by working with a normal form it means that you can define a
whole range of interpreters that see the necessary structure of the original
programs. To illustrate one problem with `DatabaseHandle -> IO a`, if we
attempted to write a pure interpreter, we would be unable to see which queries
were performed in order to produce the data under `a` (not to mention the
limitation that working in `IO` would cause).

### 3. Introduce a type class for homomorphisms

With your effect language defined, the next step is to define a type class for
homomorphisms from this effect language into larger monad stacks. Often this
will be a monad homomorphism -- much as we saw with `MonadReader` and `MonadIO`
-- but the homomorphism need not be a monad homomorphism. For example, if your
source effect language is a simple monoid, then the homomorphism will be a
monoid homomorphism. We'll see an example of this shortly.

### 4. Export polymorphic operations

With a type class of homomorphisms, we can now export a cleaner API. For
`MonadReader`, this means exporting convenience `ask` operations that are
defined in terms of `liftAsk` with the appropriate program in our `AskF`
language.

### 5. Provide a reference implementation

I also suggest providing a "reference" implementation of this type class. For
`MonadReader`, this reference implementation is `ReaderT`. The idea is that
users can immediately take advantage of the effect we're defining by introducing
the appropriate monad transformer into their monad stack.

The type class allows them to more efficiently define the operations in terms of
existing monadic capabilities (e.g., `IO`), but for many simply reusing a
transformer will be sufficient.

## A worked example for logging

To conclude this article I want to explore one more application of this pattern
applied to building a logging effect. In fact, it is this very problem that
motivated the research for this blog post, and so we'll end up building the
foundations of my `logging-effect` library.

The first step is to identify a language for programs that can perform logging.
There's not much involved here, simply the ability to append to the log at any
point in time. Let's formalise that idea with the appropriate functor:

```haskell
data LoggingF message a = AppendLogMessage message a
  deriving (Functor)
```

This functor is parameterised by the type of log messages. The only constructor
for `LoggingF` takes a log message and the rest of the computation to run. We
could stop here and lift `Free (LoggingF message) a` programs, but I want to go
a bit further and see are any other ways to express this. I'll use normalisation
by evaluation again, and see what happens.

```haskell
runFreeLogging :: Free (LoggingF message) a -> (a, [message])
runFreeLogging (Pure a) = (a, [])
runFreeLogging (Free (AppendLogMessage m next)) =
  case runFreeLogging next of
    (a, messages) -> (a, m:messages)
```

We can also take a `(a, [message])` and turn it back into the equivalent
`Free (LoggingF message) a`, so `(a, [message])` is another candidate for the
language of our logging programs.

But this `a` bothers me. It occurs only in `LoggingF message` to capture the
rest of the computation, but never does the result of logging affect the choice
of what that next computation is. This suggests that it's mostly noise, and
maybe we can just erase it. This would lead us to have logging programs of the
type `[message]`. This type is no longer the right kind for our lifting
operation to be a monad homomorphism, which means we have to identify another
algebraic structure. Well, lists are certainly a composable structure - they
have all the properties of a *monoid*.

With that in mind, we need to consider what it means to be a monoid homomorphism
into some monad. First, observe that monads also have a monoid-like operations:

```haskell
monadMempty :: Monad m => ()
monadMempty = return ()

monadMappend :: Monad m => m () -> m () -> m ()
monadMappend l r = l >> r
```

We can now write our lifting type class with the laws of a monoid homomorphism:

```haskell
liftLog mempty   = mempty                 -- = return ()
liftLog (x <> y) = liftLog x <> liftLog y -- = liftLog x >> liftLog y
class MonadLog message m | m -> message where
  liftLog :: [message] -> m ()
```

While we reached this type by normalisation-by-evaluation and then a little bit
of fudging, there is another way we could have got here. In a sense, `[]` can be
seen as another construction like `Free` - given any type `a`, `[a]` is a free
monoid generated by `a`. An easier route to this type class would have been to
describe the individual operations in our logging programs by:

```haskell
data LoggingOp message = LogMessage message
```

and then using `[]` as our free construction. As `LoggingOp message` ~ `Identity
message` ~ `message`, we know we could also use `[message]`, and we're back at the
type class above.

(In my `logging-effect` library I chose a slightly different representation of
the free monoid.
[Theoretically, this is a sounder way to talk about free monoids](http://comonad.com/reader/2015/free-monoids-in-haskell/),
but I'm mostly interested in the slight efficiency win by not having to build up
lists only to immediately deconstruct them.)

The last steps are to provide polymorphic operations and a reference
implementation that satisfies the laws:

```haskell
logMessage :: (MonadLog message m) => message -> m ()
logMessage message = liftLog [message]

newtype LoggingT message m a = LoggingT (ReaderT (message -> IO ()) m a)

instance MonadIO m => MonadLog message (LoggingT message m) where
  liftLog messages = LoggingT (\dispatchLog -> liftIO (for_ messages dispatchLog))
```

Does this reference implementation satisfy the monoid homomorphism laws that is
required by `MonadLog`?

```
liftLog mempty
  = { definition of mempty for lists }
liftLog []
  = { definition of liftLog for LoggingT }
LoggingT (\dispatchLog -> liftIO (for_ [] dispatchLog))
  = { definition of for_ for [] }
LoggingT (\dispatchLog -> liftIO (return ()))
  = { liftIO . return = return }
LoggingT (\dispatchLog -> return ())
  = { definition of return for LoggingT }
return ()
```

So far so good!

```
liftLog (x <> y)
  = { definition of liftLog for LoggingT }
LoggingT (\dispatchLog -> liftIO (for_ (x ++ y) dispatchLog))
  = { for_ distributes over ++ }
LoggingT (\dispatchLog -> liftIO (for_ x dispatchLog >> for_ y dispatchLog)
  = { liftIO (f >>= g) = liftIO f >>= liftIO . g }
LoggingT (\dispatchLog -> liftIO (for_ x dispatchLog) >> liftIO (for_ y dispatchLog))
  = { definition of (>>=) for LoggingT }
LoggingT (\dispatchLog -> liftIO (for_ x dispatchLog)) >>
LoggingT (\dispatchLog -> liftIO (for_ y dispatchLog)) >>
  = { definition of liftLog for LoggingT }
liftLog x >> liftLog y
```

Bingo!

## Further thoughts

In this post I presented a pattern for building `mtl`-like type classes in a
mechanical fashion, and this suggests that maybe some of the details can be
automatically dealt with. In the next few days I'll be presenting my
`algebraic-transformers` library which will show exactly that.
