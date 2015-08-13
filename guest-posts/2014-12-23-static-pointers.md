---
title: 24 Days of GHC Extensions: Static Pointers
---

Today we start to wrap up 24 Days of GHC Extensions with the final guest post,
this time from Mathieu Boespflug of [Tweag I/O](http://www.tweag.io/).
Mathieu, along with his colleagues Facundo Domínguez and Alexander Vershilov,
has been working with the GHC team on a new extension - in fact, so new, you
won't even find this in a stable release of GHC! In today's post, we'll have a
look at a new type of pointer that can be useful for distributed programming.

## Distributed `Static` Programming

GHC already features quite the zoo of pointer types. There are bare
`Ptr`'s (for marshalling to and from foreign objects), `ForeignPtr`'s
(smart pointers that allow automatic memory management of the target
object), weak pointers (references to objects that are ignored by the
garbage collector), and `StablePtr`'s (pointers to objects that are
pinned to a specific location in memory). GHC 7.10 will add a new
beast to this list: `StaticPtr`, the type of pointers whose value
never changes across program runs, even across program runs on
different machines. The objects pointed to by static pointers are
*static*, much in the same sense of the word as in other programming
languages: their value is known at compile time and their lifetime
extends across the entire run of the program. GHC 7.10 also comes with
a new language extension to *safely* create static pointers:
`StaticPointers`.

Why yet another pointer type? And why grace it with yet another
extension?

Static pointers turn out to be incredibly useful for distributed
programming. Imagine that you have a fleet of networked computers,
abstractly called *nodes*. You'd like these nodes to collaborate, say
because you also have a fair amount of data you'd like to crunch
through, or because some of these nodes provide services to other
nodes. Static pointers help solve the age-old question of distributed
programming: how can nodes easily delegate tasks to each other?

For most programming languages, this is a thorny question to ask:
support for distributing computations comes as an afterthought, so
there is no first class support. But there are exceptions: Erlang is
one example of a language that has escaped from research labs one way
or another and natively speaks distributed. Erlang supports literally
sending the code for any native (non-foreign) function from node to
node. Delegating a task called `myfun` is a case of saying:

```Erlang
spawn(There, myfun)
```

where `There` is a variable containing some node identifier. This
capability comes at a cost, however. It is in general hard to share
optimized compiled code across a cluster of machines, which may not be
running the exact same operating system or have the same system
libraries available. So Erlang keeps to comparatively slow but easy to
handle and easy to distribute interpreted bytecode instead. Moreover,
if new code can be loaded into a running program at any moment or
existing code monkey patched on-the-go, what tools do we have to
reason about the resulting state of the program?

Haskell too natively speaks distributed, at least in its bleeding edge
GHC variant. But at much lower cost. In a world where complete systems
can be containerized using language agnostic technology, and shipped
and deployed within minutes across a full scale cluster, do we really
need our language runtimes to distribute *code*? Are we willing to
accept the compromises involved? Perhaps that is a problem best solved
once, for all programs in any language, using the likes of
[Docker][docker] or [Rocket][rocket]. And once our entire cluster is
running instances of the same program by dint of distributing
containers, all we need is a means to control which computations
happen when, and where, by sharing *references* to functions. This
works because, if all nodes are running the same program, then they
all have access to the same functions.

Turning on `-XStaticPointers` adds a new keyword `static` and a new
syntactic form to the language for *safely* creating such references:
if expression `e` has type `a`, then `static e` has type `StaticPtr
a`.

## Static pointers in practice

For example, here's a program that obtains a static pointer to `f`,
and prints the info record associated with it:

```Haskell
module Main where

import GHC.StaticPtr

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

main = do
  let sptr :: StaticPtr (Int -> Int)
      sptr = static fact
  print $ staticPtrInfo sptr
  print $ deRefStaticPtr sptr 10
```

The body of a static form can be any top-level identifier, but also
arbitrary expressions, *so long as the expression is closed*, meaning
that all variable names are either bound within the expression itself,
or are top-level identifiers. That is, so long as the value of the
expression could in principle be computed statically.

Given a static pointer, we can get back the value it points to using

```
deRefStaticPtr :: StaticPtr a -> a
```

Notice that we could as well have used a simple string to refer to
`fact` in the above program, construct a string table, so that if the
program were distributed we could have each process communicate
strings in lieu of functions to commuicate tasks to run remotely,
using the string table to map strings back to functions. Something
like this:

```Haskell
module Main where

import GHC.StaticPtr
import Data.Dynamic

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

computation1 :: IO ()
computation1 = print $ fact 10

stringTable =
  [ ("fact", toDynamic fact)
  , ("computation1", toDynamic computation1)
  ]

main = do
  send "some-node" "computation1"
```

where one could imagine node "some-node" running something like

```Haskell
serverLoop :: IO ()
serverLoop = forever $ do
  sptr <- expect
  fromDynamic (stringTable !! sptr)
```

assuming we have a `send` function for sending serializable values as
messages to nodes and a `expect` function to receive them available.

Values in the string table are wrapped into `Dynamic` to make them all
have uniform type (that way a simple homegeneous list can do just
fine as a datastructure). But there are three problems with this
approach:

1. Constructing the string table is error prone: we might accidentally
map the string `"fact"` to an entirely different function.

1. No type safety. `fromDynamic` performs a type cast. This cast might
fail if the type of value in the string table doesn't match the
expected type, making the program partial.

1. It is antimodular: each module needs its own string table, which we
then need to combine into a global string table for the whole program.
If we add a any new module anywhere in the program, we need to also
modify the construction of the string table, or accidentally forget to
do so, which would constitute a bug.

(Some of these properties can be obtained with some clever Template
Haskell hackery, but that solution is still fundamentally
anti-modular, as well as contrived to use.)

It is for these three reasons that the `StaticPointers` language
extension comes in handy. There is no need for manually constructing
tables. Constructing and dereferencing static pointers is type safe
because the type of a static pointer is related to the type of the
value that it points to. Separate modules are not a problem, because
the compiler takes care of collecting the set of all static pointers
in a program into its own internal table that it embeds in the binary.

## Pointer serialization

This all sounds rather nice, but the static pointer type is kept
abstract, as it should to ensure safety, so how can we serialize
a static pointer to send over the wire, and deserialize it on the
remote end to reconstruct the static pointer? The `GHC.StaticPtr`
module exports a few primitives to deal with just that. The idea is
that each static pointer in a program is assigned a unique key (a
`StaticKey`). We can obtain the key for a static pointer using

```Haskell
type StaticKey = Fingerprint

-- Defined in GHC.Fingerprint.
data Fingerprint = Fingerprint {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
  deriving (Generic, Typeable)

staticKey :: StaticPtr a -> StaticKey
```

The type of keys is concrete (a key is a 128-bit hash), so keys can
easily be encoded and decoded on the wire, using the `Binary` type
class provided by the [binary package][binary]:

```Haskell
-- Automatically derived instance, using `DeriveGeneric`.
instance Binary Fingerprint
```

Provided a key, we can map it to a `StaticPtr` using

```Haskell
unsafeLookupStaticPtr :: StaticKey -> Maybe (StaticPtr a)
```

Hold on a minute! This type is telling us that using
`unsafeLookupStaticPtr` we can map the key to a static pointer of any
type, which we can then `deRefStaticPtr` to a value of arbitrary
type... Have we just lost type safety? In GHC 7.10, yes we have! In
GHC 7.12, we will have a much safer lookup function:

```Haskell
lookupStaticPtr :: StaticKey
                -> (forall a. Typeable a => StaticPtr a -> b)
                -> Maybe b
```

(observe that this is a rank-2 type,) or equivalently

```Haskell
data DynStaticPtr = forall a. Typeable a => DynStaticPtr (StaticPtr a)

lookupStaticPtr :: StaticKey -> Maybe DynStaticPtr
```

This type says, provided a key and a continuation, `lookupStaticPtr`
will resolve the key to a static pointer and if successful feed it to
the continuation. The type of the static key is not known a priori,
but we can query the type inside the continuation using the supplied
`Typeable` constraint. The reason only the unsafe variant will ship in
GHC 7.10 is because the safe variant will require a change to the
`Data.Typeable` API to be truly safe (see [here][ttypeable] for
details), and because we do not yet store `Typeable` constraints in
the internal compiler-generated table mentioned above. In the
meantime, this shouldn't be a problem in practice: higher level
libraries like Cloud Haskell and HdPH hide all uses of
`lookupStaticPtr` behind an API that does guarantee type safety - it's
just that we have to *trust* that their implementations always call
`lookupStaticPtr` at the right type, when ideally we wouldn't need to
entrust type safety to any library code at all, just the compiler.

## Static closures

Static pointers turn out to be suprisingly powerful. As it stands, the
language extension nominally only allows sharing references to static
values across the wire. But it's easy to build a lot more power on
top. In particular, it would be nice if programs could transmit not
just static values over the wire, but indeed (nearly) any odd closure.
Consider the following `main` function:

```Haskell
main = do
  putStrLn "Hi! Give me a number..."
  x <- read <$> getLine
  send "some-node" $ closure (static fact) `closureAp` closurePure 10
```

The idea (first found in the "Towards Haskell in the Cloud"
[paper][towards-ch]) is to introduce a datatype of closures (which
we'll define concretely later), along with three combinators to create
`Closure`s from `StaticPtr`s and from other `Closure`s:

```
data Closure a

closure :: StaticPtr a -> Closure a
closurePure :: Serializable a => a -> Closure a
closureAp :: Closure (a -> b) -> Closure a -> Closure b
```

Notice that this datatype is nearly, but not quite, an applicative
functor. We can only lift "serializable" values to a closure, not just
any value. Given two existing `Closure`s, we can create a new
`Closure` by "applying" one to another. Morally, we are making it
possible to pass around not just static pointers to top-level values
or purely static expressions, but things that represent (partially)
applied static pointers. `Closure`s are not always static: their value
may depend on values known only at runtime, as in the example above.

Come to think of it, a `Closure` very much acts like the closures that
one would find deep in the bowels of GHC for representing partially
applied functions during program execution. A closure is morally
a code pointer paired with an *environment*, i.e. a list of actual
arguments. Closures accumulate arguments as they are applied. In our
case, the `StaticPtr` represents a code pointer, and the environment
grows everytime we `closureAp` a `Closure` to something else.

We'll turn to how `Closure` is defined in a minute, but first let's
talk about what it really means to be "serializable":

```Haskell
data Dict c = c => Dict

class (Binary a, Typeable a) => Serializable a where
  serializableDict :: StaticPtr (Dict (Serializable a))
```

This class definition says that if a value can be encoded/decoded to
a `ByteString` (see the [binary package][binary]), and it can be
queried for a representation of its type at runtime, then the value is
*serializable*. However, serializable values also need to make it
possible to obtain concrete "evidence" that the value really is
serializable, in the form of a *static dictionary*. The idea is a neat
trick. For all serializable values, we want to be able to obtain
a static pointer to the evidence (or "dictionary") associated with
a class constraint. Because if we do, then we can "send" class
dictionaries across the wire (or at least references to them)! But we
can only take the static pointer of a value, so how does one make
dictionary a first class value? The trick is to define a proxy
datatype of dictionaries, using the `ConstraintKinds` extension (the
`Dict` datatype). Any `Dict` value is a value like any other, but it
embeds a constraint in it, which at runtime corresponds to
a dictionary.

For example, any concrete value of `Dict (Eq Int)` carries
a dictionary that can be seen as providing evidence that values of
`Int` type can indeed be compared for equality. For any type `a`,
`Dict (Serializable a)` carries evidence that values of type `a` are
serializable. Any instance of `Serializable` makes it possible to
query for this evidence - for example:

```Haskell
instance Serializable Int where
  serializableDict = static Dict
```

Now we can turn to the definition of `Closure` and its combinators:

```Haskell
data Closure a where
  StaticPtr :: StaticPtr b -> Closure b
  Encoded :: ByteString -> Closure ByteString
  Ap :: Closure (b -> c) -> Closure b -> Closure c
  deriving (Typeable)

closure :: StaticPtr a -> Closure a
closure = StaticPtr

closureAp :: Closure (a -> b) -> Closure a -> Closure b
closureAp = Ap

closurePure :: Serializable a => a -> Closure a
closurePure x =
    StaticPtr (static decodeD) `closureAp`
    closure serializableDict `closureAp`
    Encoded (encode x)
  where
    decodeD :: Dict (Serializable a) -> ByteString -> a
    decodeD Dict = decode

```

(There are many ways to define `Closure`, but this definition is
perhaps most intuitive.)

As we can see from the definition, a `Closure` is not only a (quasi)
applicative functor, but in fact a (quasi) *free* applicative functor.
Using the `Ap` constructor, we can chain closures into long sequences
(i.e. build environments). Using `StaticPtr` and `Encoded`, we can
further make any serializable value a `Closure` of the following
shape:

```Haskell
Ap (Ap (StaticPtr sptr_decodeD) csdict) bs
```

where `sptr_decodeD` is the static pointer to `decodeD`, `csdict` is
a static serialization dictionary, and `bs` is a value encoded as
a byte string.

Notice that any concrete `Closure` type is itself serializable:

```
instance Binary (Closure a) where
  put (Ap (Ap (StaticPtr sptr) dict) (Encoded bs)) =
      putWord8 0 >> put sptr >> put dict >> put bs
  put (StaticPtr sptr) = putWord8 1 >> put sptr
  put (Ap cf cx) = putWord8 2 >> put cf >> put cx

  get = do
    hdr <- getWord8
    case hdr of
      0 -> do sptr <- get
              dict <- get
              bs <- get
              return $ Ap (Ap (StaticPtr sptr) dict) (Encoded bs)
      1 -> StaticPtr <$> get
      2 -> Ap <$> get <*> get

instance Serializable (Closure Int)
  serializableDict = static Dict
```

(Note that for most types, manually defined `Binary` instances as
above are unnecessary - any datatype with a `Generic` instance can
have its `Binary` instance derived automatically).

Therefore, suprisingly, adding just static pointers as a primitive
datatype in the compiler is all that's necessary to be able to
conveniently send even nearly arbitrary closures down the wire. It
turns out that we don't need to add full blown support for serializing
arbitrary closures as an extra primitive to the compiler. That can all
be done in user space, and with better control by the user on exactly
how. The only limitation is that in effect the environment part of the
closure needs to be serializable, but that's a feature: it means that
we can statically rule out accidentally serializing closures that
capture gnarly things that we *don't* want to serialize down the wire:
think file handles, locks, sockets and other system resources, none of
which the remote end would be able to make any sense of.

## Conclusion

Static pointers are a lightweight extension to GHC, with direct
applications to distributed programming, or in general, any form of
pointer sharing across processes with distinct address spaces. As
first observed in a [seminal paper][towards-ch] about distributed
programming in Haskell, this extension adds just enough power to the
GHC compiler and runtime to conveniently and safely send arbitrary
serializable closures across the wire.

Distributed programming in Haskell is a reality today: there are
[several][hdph] [frameworks][courier], most prominently [Cloud
Haskell][cloud-haskell], with several industrial applications. But the
`StaticPointers` extension is brand new, and in fact no compiler
release including it has shipped yet! Framework and application
support for it is still lagging behind, but you can help. In
particular, adding support to distributed-static and
distributed-process would be a great step forward in the usability
Cloud Haskell. Other next steps include: adding support for
interoperating multiple versions of a program in a cluster, fully
implementing `lookupStaticPtr` (see above), or improving the
robustness and speed of message serialization (see for example [these
great results][stronger-better-faster] for an idea of what's possible
here). Those are just some ideas. If you're interested in
participating, the [GHC wiki][ghc-wiki-dH] contains quite a few
pointers, and the cloud-haskell-developers@ and distributed-haskell@
mailing lists are good places to coordinate efforts. See you there!

[binary]: http://hackage.haskell.org/package/binary-0.7.2.3/docs/Data-Binary.html
[docker]: https://www.docker.com/
[rocket]: https://github.com/coreos/rocket
[towards-ch]: http://research.microsoft.com/en-us/um/people/simonpj/papers/parallel/remote.pdf
[ttypeable]: https://ghc.haskell.org/trac/ghc/wiki/Typeable
[cloud-haskell]: http://haskell-distributed.github.io/
[hdph]: https://hackage.haskell.org/package/hdph
[courier]: http://hackage.haskell.org/package/courier
[stronger-better-faster]: http://code.haskell.org/~duncan/binary-experiment/binary.pdf
