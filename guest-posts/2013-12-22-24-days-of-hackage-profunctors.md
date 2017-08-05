---
title: "24 Days of Hackage: profunctors"
---

[Yesterday](/guest-posts/2013-12-21-24-days-of-hackage-contravariant.html), Tom
showed us a different type of functor than the ordinary Haskell `Functor` - the
contravariant functor. Today, Tom's going to guide us through another type of
functor - the *profunctor*.

---

Yesterday, we considered the intuition that functors are producers of output,
and contravariant functors are consumers of input - and both functors can be
adapted to work with different types. What about a datatype that represents both
an "adaptable consumer of input" and an "adaptable producer of output" at the
same time, i.e. some sort of "pipe" structure?  This is exactly what a
`Profunctor` instance is, and again the function arrow `a -> b` gives us
our prototypical example of such a type.  A `Profunctor` has two type
parameters, `a` and `b`.  The first can be thought of as an "input of type `a`"
and can be adapted with a function of type `c -> a`, like `Contravariant`.  The
second can be thought of as an "output of type `b`" and can be adapted with a
function of type `b -> d`, like `Functor`. This gives us the following type
class:

```haskell
class Profunctor p where
  lmap :: (c -> a) -> p a b -> p c b
  rmap :: (b -> d) -> p a b -> p a d
  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d
```

`lmap` adapts the input (left hand type variable) and `rmap` adapts the output
(right hand type variable).  `dimap` adapts them both at the same time.

Much like `Functor` and `Contravariant`, `Profunctor` instances must satisfy some
laws.  The laws that a `Profunctor` must satisfy are a combination of the
`Functor` and `Contravariant` laws for its type parameters:

* `dimap id id = id`
* `dimap (h' . h) (f . f') = dimap h f . dimap h' f'`

Furthermore, `rmap f` should be equivalent to `dimap id f`, and `lmap f` should
be equivalent to `dimap f id`. Because of this, the minimal definition of a
`Profunctor` instance is either specifying `rmap` and `lmap` in terms of
`dimap`, or `dimap` in terms of `rmap` and `lmap`.

For functions, the `Profunctor` instance is:

```haskell
instance Profunctor (->) where
  lmap h g = g . h
  rmap f g = f . g
  dimap h f g = f . g . h
```

If you study this, you'll see that `lmap` adapts a function `g` by composing `h`
on the right (changing the "input"), while `rmap` adapts `g` by composing `f` on
the left (changing the "output"). Using equational reasoning, we can easily
prove that this instance does indeed satisfy the laws:

```haskell
dimap id id = \g -> id . g . id = \g -> g = id
dimap (h' . h) (f . f') = \g -> (f . f') . g . (h' . h)
                        = \g -> f . (f' . g . h') . h
                        = \g -> f . dimap h' f' g . h
                        = \g -> dimap h f (dimap h' f' g)
                        = dimap h f . dimap h' f'
```

So all is well.

The function arrow is the prototypical example of a profunctor, but it
is also probably the most boring one.  Here's a more interesting
example: a datatype that represents the concept of a left fold.

```haskell
data L a b = forall s. L (s -> b) (s -> a -> s) s
```

The left fold is a process that updates state according to its input, and can
transform this state into a final result.  The datatype contains some value of
type `s` representing the initial state, a map of type `s -> a -> s` which reads
a value `a` and updates the state accordingly, and a map of type `s -> b` which
converts the final state into the return value.  This is a `Profunctor` or
"adaptable pipe with input of type `a` and output of type `b`".  (You'll find
this definition in the [`folds`](http://hackage.haskell.org/package/folds)
package).

```haskell
instance Profunctor L where
  dimap h f (L result iterate initial) =
    L (f . result) (\s -> iterate s . h) initial

runFold :: L a b -> [a] -> b
runFold (L result iterate initial) = result . foldl iterate initial
```

Here's a left fold that represents a sum.

```haskell
summer :: Num a => L a a
summer = L id (+) 0

testSummer :: Int
testSummer = runFold summer [1..10]
```

```
> testSummer
55
```

This is fine if we're summing a list of `Num` instances, but can we use this to
fold other types of lists? Of course we can, and to do so we'll need to change
the input type to our fold. Here's an example of adapting the left fold to a
different input and output type.

```haskell
lengther :: L String String
lengther = dimap length (\s -> "The total length was " ++ show s) summer

testLengther :: String
testLengther = runFold lengther ["24", "days", "of", "hackage", "!"]
```

```
> testLengther
"The total length was 16"
```

There are not many `Profunctor` definitions on hackage, although they are used
in the internals of [`lens`](http://hackage.haskell.org/package/lens).
Personally I have used them heavily in a Haskell relation database EDSL that I
have developed (currently private, but which I hope will be open-sourced at some
point in the future).  In that library there are profunctors which act exactly
as the intuition about them suggests: they can be seen as consuming values of
one type and producing values of another.  For example there is a `Profunctor`
instance for "consuming" the rows returned by a query running on PostgreSQL and
"producing" the equivalent values in Haskell.

Defining a `Contravariant` or `Profunctor` instance for your datatype can give
you more certainty about the correctness of your code, just like defining a
`Functor` instance can.  Unless I am much mistaken, parametricity implies that
there is at most one valid `Contravariant` or `Profunctor` instance for your
datatype.  Thus defining such an instance cannot restrict you in any way, and
acts as an additional property for the type checker to check that your program
satisfies.  If you expected your type to be contravariant in its argument, or a
profunctor in two arguments, but you can't write a definition to satisfy the
compiler then perhaps you have a bug in your type definition!

Have a look through your code and if you find suitable datatypes, give yourself
the gift of `Contravariant` and `Profunctor` instances this Christmas.

Thanks to merijn on the #haskell IRC channel who suggested the
output/input/pipe adaptor analogy.
