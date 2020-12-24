---
title: Monad Transformers and Effects with Backpack
---

A good few years ago [Edward Yang](http://ezyang.com/) gifted us an implementation of Backpack - a way for us to essentially abstract modules over other modules, allowing us to write code independently of implementation. A big benefit of doing this is that it opens up new avenues for program optimization. When we provide concrete instantiations of signatures, GHC compiles it as if that were the original code we wrote, and we can benefit from a lot of specialization. So aside from organizational concerns, Backpack gives us the ability to write some really fast code. This benefit isn't just theoretical - Edward Kmett gave us [unpacked-containers](https://hackage.haskell.org/package/unpacked-containers), removing a level of indirection from all keys, and Oleg Grenrus showed as how we can use Backpack to ["unroll" fixed sized vectors](https://www.well-typed.com/blog/2019/11/unrolling-data-with-backpack/). In this post, I want to show how we can use Backpack to give us the performance benefits of explicit transformers, but without having library code commit to any specific stack. In short, we get the ability to have multiple interpretations of our program, but without paying the performance cost of abstraction.


# The Problem

Before we start looking at any code, let's look at some requirements, and understand the problems that come with some potential solutions. The main requirement is that we are able to write code that requires some effects (in essence, writing our code to an effect *interface*), and then run this code with different interpretations. For example, in production I might want to run as fast as possible, in local development I might want further diagnostics, and in testing I might want a pure or in memory solution. This change in representation shouldn't require me to change the underlying library code.

Seasoned Haskellers might be familiar with the use of effect systems to solve these kinds of problems. Perhaps the most familiar is the `mtl` approach - perhaps unfortunately named as the technique itself doesn't have much to do with the library. In the `mtl` approach, we write our interfaces as type classes abstracting over some `Monad m`, and then provide instances of these type classes - either by stacking transformers ("plucking constraints", in the [words of Matt Parson](https://www.parsonsmatt.org/2020/01/03/plucking_constraints.html)), or by a "mega monad" that implements many of these instances at once (e.g., like Tweag's [`capability`](https://github.com/tweag/capability)) approach.

Despite a few annoyances (e.g., the "n+k" problem, the lack of implementations being first-class, and a few other things), this approach can work well. It also has the *potential* to generate a great code, but *in practice* it's rarely possible to achieve maximal performance. In her excellent talk ["Effects for Less"](https://www.youtube.com/watch?v=0jI-AlWEwYI), [Alexis King](https://lexi-lambda.github.io/) hits the nail on the head - despite being able to provide good code for the implementations of particular *parts* of an effect, the majority of effectful code is really just threading around inside the `Monad` constraint. When we're being polymorphic over any `Monad m`, GHC is at a loss to do any further optimization - and how could it? We know nothing more than "there will be some `>>=` function when you get here, promise!" Let's look at this in a bit more detail.

Say we have the following:

```haskell
foo :: Monad m => m Int
foo = go 0 1_000_000_000
  where
    go acc 0 = return acc
    go acc i = return acc >> go (acc + 1) (i - 1)
```

This is obviously "I needed an example for my blog" levels of contrived, but at least small. How does it execute? What are the runtime consequences of this code? To answer, we'll go all the way down to the STG level with `-ddump-stg`:

```haskell
$wfoo =
    \r [ww_s2FA ww1_s2FB]
        let {
          Rec {
          $sgo_s2FC =
              \r [sc_s2FD sc1_s2FE]
                  case eqInteger# sc_s2FD lvl1_r2Fp of {
                    __DEFAULT ->
                        let {
                          sat_s2FK =
                              \u []
                                  case +# [sc1_s2FE 1#] of sat_s2FJ {
                                    __DEFAULT ->
                                        case minusInteger sc_s2FD lvl_r2Fo of sat_s2FI {
                                          __DEFAULT -> $sgo_s2FC sat_s2FI sat_s2FJ;
                                        };
                                  }; } in
                        let {
                          sat_s2FH =
                              \u []
                                  let { sat_s2FG = CCCS I#! [sc1_s2FE]; } in  ww1_s2FB sat_s2FG;
                        } in  ww_s2FA sat_s2FH sat_s2FK;
                    1# ->
                        let { sat_s2FL = CCCS I#! [sc1_s2FE]; } in  ww1_s2FB sat_s2FL;
                  };
          end Rec }
        } in  $sgo_s2FC lvl2_r2Fq 0#;

foo =
    \r [w_s2FM]
        case w_s2FM of {
          C:Monad _ _ ww3_s2FQ ww4_s2FR -> $wfoo ww3_s2FQ ww4_s2FR;
        };
```

In STG, whenever we have a `let` we have to do a heap allocation - and this code has quite a few! Of particular interest is the what's going on inside the actual loop `$sgo_s2FC`. This loop first compares `i` to see if it's `0`. In the case that's it's not, we allocate two objects and call `ww_s2Fa`. If you squint, you'll notice that `ww_s2FA` is the first argument to `$wfoo`, and it ultimately comes from unpacking a `C:Monad` dictionary. I'll save you the labor of working out what this is - `ww_s2Fa` is the `>>`. We can see that every iteration of our loop incurs two allocations for each argument to `>>`. A heap allocation doesn't come for free - not only do we have to do the allocation, the entry into the heap incurs a pointer indirection (as heap objects have an info table that points to their entry), and also by merely being on the heap we increase our GC time as we have a bigger heap to traverse. While my STG knowledge isn't great, my understanding of this code is that every time we want to call `>>`, we need to supply it with its arguments. This means we have to allocate two closures for this function call - which is basically whenever we pressed "return" on our keyboard when we wrote the code. This seems crazy - can you imagine if you were told in C that merely using `;` would cost time and memory?

If we compile this code in a separate module, mark it as `{-# NOINLINE #-}`, and then call it from `main` - how's the performance? Let's check!

```haskell
module Main (main) where

import Foo

main :: IO ()
main = print =<< foo
```

```
$ ./Main +RTS -s
1000000000
 176,000,051,368 bytes allocated in the heap
       8,159,080 bytes copied during GC
          44,408 bytes maximum residency (1 sample(s))
          33,416 bytes maximum slop
               0 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     169836 colls,     0 par    0.358s   0.338s     0.0000s    0.0001s
  Gen  1         1 colls,     0 par    0.000s   0.000s     0.0001s    0.0001s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time   54.589s  ( 54.627s elapsed)
  GC      time    0.358s  (  0.338s elapsed)
  EXIT    time    0.000s  (  0.000s elapsed)
  Total   time   54.947s  ( 54.965s elapsed)

  %GC     time       0.0%  (0.0% elapsed)

  Alloc rate    3,224,078,302 bytes per MUT second

  Productivity  99.3% of total user, 99.4% of total elapsed
```

**OUCH**. My i7 laptop took almost a minute to iterate a loop 1 billion times.

A little disclaimer: I'm intentionally painting a severe picture here - in practice this cost is irrelevant to all but the most performance sensitive programs. Also, notice where the `let` bindings are in the STG above - they are nested within the loop. This means that we're essentially allocating "as we go" - these allocations are incredibly cheap, and the growth to GC is equal trivial, resulting in more like constant GC pressure, rather than impending doom. For code that is likely to do any IO, this cost is likely negligible compared to the rest of the work. Nonetheless, it is there, and when it's there, it's nice to know if there are alternatives.

So, is the TL;DR that Haskell is completely incapable of writing effectful code? No, of course not. There is another way to compile this program, but we need a bit more information. If we happen to know what `m` is and we have access to the `Monad` dictionary for `m`, then we might be able to inline `>>=`. When we do this, GHC can be a lot smarter. The end result is code that now doesn't allocate for every single `>>=`, and instead just gets on with doing work. One trivial way to witness this is to define everything in a single module (Alexis rightly points out this is a trap for benchmarking that many fall into, but for our uses it's the behavior we actually want).

This time, let's write everything in one module:

```haskell
module Main ( main ) where


```

And the STG:

```haskell
lvl_r4AM = CCS_DONT_CARE S#! [0#];

lvl1_r4AN = CCS_DONT_CARE S#! [1#];

Rec {
main_$sgo =
    \r [void_0E sc1_s4AY sc2_s4AZ]
        case eqInteger# sc1_s4AY lvl_r4AM of {
          __DEFAULT ->
              case +# [sc2_s4AZ 1#] of sat_s4B2 {
                __DEFAULT ->
                    case minusInteger sc1_s4AY lvl1_r4AN of sat_s4B1 {
                      __DEFAULT -> main_$sgo void# sat_s4B1 sat_s4B2;
                    };
              };
          1# -> let { sat_s4B3 = CCCS I#! [sc2_s4AZ]; } in  Unit# [sat_s4B3];
        };
end Rec }

main2 = CCS_DONT_CARE S#! [1000000000#];

main1 =
    \r [void_0E]
        case main_$sgo void# main2 0# of {
          Unit# ipv1_s4B7 ->
              let { sat_s4B8 = \s [] $fShowInt_$cshow ipv1_s4B7;
              } in  hPutStr' stdout sat_s4B8 True void#;
        };

main = \r [void_0E] main1 void#;

main3 = \r [void_0E] runMainIO1 main1 void#;

main = \r [void_0E] main3 void#;
```

The same program compiled down to much tighter loop that is almost entirely free of allocations. In fact, the only allocation that happens is when the loop terminates, and it's just boxing the unboxed integer that's been accumulating in the loop.

As we might hope, the performance of this is much better:

```
$ ./Main +RTS -s
1000000000
  16,000,051,312 bytes allocated in the heap
         128,976 bytes copied during GC
          44,408 bytes maximum residency (1 sample(s))
          33,416 bytes maximum slop
               0 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     15258 colls,     0 par    0.031s   0.029s     0.0000s    0.0000s
  Gen  1         1 colls,     0 par    0.000s   0.000s     0.0001s    0.0001s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    9.402s  (  9.405s elapsed)
  GC      time    0.031s  (  0.029s elapsed)
  EXIT    time    0.000s  (  0.000s elapsed)
  Total   time    9.434s  (  9.434s elapsed)

  %GC     time       0.0%  (0.0% elapsed)

  Alloc rate    1,701,712,595 bytes per MUT second

  Productivity  99.7% of total user, 99.7% of total elapsed
```

Our time in the garbage collector dropped by a factor of 10, from 0.3s to 0.03. Our total allocation dropped from 176GB (yes, you read that right) to 16GB (I'm still not entirely sure what this means, maybe someone can enlighten me). Most importantly our total runtime dropped from 54s to just under 10s. All this from just knowing what `m` is at compile time.

So GHC is capable of producing excellent code for monads - what are the circumstances under which this happens? We need, at least:

1. The source code of the thing we're compiling must be available. This means it's either defined in the same module, or is available with an `INLINABLE` pragma (or GHC has chosen to add this itself).

2. The definitions of `>>=` and friends must also be available in the same way.

These constraints start to feel a lot like needing whole program compilation, and in practice are unreasonable constraints to reach. To understand why, consider that most real world programs have a small `Main` module that opens some connections or opens some file handles, and then calls some library code defined in another module. If this code in the other module was already compiled, it will (probably) have been compiled as a function that takes a `Monad` dictionary, and just calls the `>>=` function repeatedly in the same manner as our original STG code. To get the allocation-free version, this library code needs to be available to the `Main` module itself - as that's the module that choosing what type to instantiate 'm' with - which means the library code has to have marked that code as being inlinable. While we could add `INLINE` everywhere, this leads to an explosion in the amount of code produced, and can sky rocket compilation times.

Alexis' [`eff`](https://github.com/hasura/eff) library works around this by *not* being polymorphic in `m`. Instead, it chooses a concrete monad with all sorts of fancy continuation features. Likewise, if we commit to a particular monad (a transformer stack, or maybe using `RIO`), we again avoid this cost. Essentially, if the monad is known a priori at time of module compilation, GHC can go to town. However, the latter also commits to semantics - by choosing a transformer stack, we're choosing a semantics for our monadic effects.

With the scene set, I now want to present you with another approach to solving this problem using Backpack.

# A Backpack Primer

Vanilla GHC has a very simple module system - modules are essentially a method for name-spacing and separate compilation, they don't do much more. The Backpack project extends this module system with a new concept - signatures. A signature is like the "type" of a module - a signature might mention the presence of some types, functions and type class instances, but it says nothing about what the definitions of these entities are. We're going to (ab)use this system to build up transformer stacks at configuration time, and allow our library to be abstracted over different monads. By instantiating our library code with different monads, we get different interpretations of the same program.

I won't sugar coat - what follows is going to pretty miserable. Extremely fun, but miserable to write in practice. I'll let you decide if you want to inflict this misery on your coworkers in practice - I'm just here to show you it can be done!

## A Signature for Monads

The first thing we'll need is a signature for data types that are monads. This is essentially the "hole" we'll rely on with our library code - it will give us the ability to say "there exists a monad", without committing to any particular choice.

In our Cabal file, we have:

```cabal
library monad-sig
  hs-source-dirs:   src-monad-sig
  signatures:       Control.Monad.Signature
  default-language: Haskell2010
  build-depends:    base
```

The important line here is `signatures: Control.Monad.Signature` which shows that this library is incomplete and exports a signature. The definition of `Control/Monad/Signature.hsig` is:

```haskell
signature Control.Monad.Signature where

data M a
instance Functor M
instance Applicative M
instance Monad M
```

This simply states that any module with this signature has some type `M` with instances of `Functor`, `Applicative` and `Monad`.

Next, we'll put that signature to use in our library code.

## Libary Code

For our library code, we'll start with a new library in our Cabal file:

```cabal
library business-logic
  hs-source-dirs:   lib
  signatures:       BusinessLogic.Monad
  exposed-modules:  BusinessLogic
  build-depends:
    , base
    , fused-effects
    , monad-sig

  default-language: Haskell2010
  mixins:
    monad-sig requires (Control.Monad.Signature as BusinessLogic.Monad)
```

Our business-logic library itself exports a signature, which is really just a re-export of the `Control.Monad.Signature`, but we rename it something more meaningful. It's this module that will provide the monad that has all of the effects we need. Along with this signature, we also export the `BusinessLogic` module:

```haskell
{-# language FlexibleContexts #-}
module BusinessLogic where

import BusinessLogic.Monad ( M )
import Control.Algebra ( Has )
import Control.Effect.Empty ( Empty, guard )

businessCode :: Has Empty sig M => Bool -> M Int
businessCode b = do
  guard b
  return 42
```

In this module I'm using `fused-effects` as a framework to say which effects my monad should have (though this is not particularly important, I just like it!). Usually `Has` would be applied to a type variable `m`, but here we're applying it to the type `M`. This type comes from `BusinessLogic.Monad`, which is a signature (you can confirm this by checking against the Cabal file). Other than that, this is all pretty standard!

## Backpack-ing Monad Transformers

Now we get into the really fun stuff - providing implementations of effects. I mentioned earlier that one possible way to do this is with a stack of monad transformers. Generally speaking, one would write a single `newtype T m a` for each effect type class, and have that transformer dispatch any effects in that class, and to `lift` any effects from other classes - deferring their implementation to `m`.

We're going to take the same approach here, but we'll absorb the idea of a transformer directly into the module itself. Let's look at an implementation of the `Empty` effect. The `Empty` effect gives us a special `empty :: m a` function, which serves the purpose of stopping execution immediately. As a monad transformer, one implementation is `MaybeT`:

```haskell
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
```

But we can also write this using Backpack. First, our Cabal library:

```haskell
library fused-effects-empty-maybe
  hs-source-dirs:   src-fused-effects-backpack
  default-language: Haskell2010
  build-depends:
    , base
    , fused-effects
    , monad-sig

  exposed-modules: Control.Carrier.Backpack.Empty.Maybe
  mixins:
    monad-sig requires (Control.Monad.Signature as Control.Carrier.Backpack.Empty.Maybe.Base)
```

Our library exports the module `Control.Carrier.Backpack.Empty.Maybe`, but also has a hole - the type of base monad this transformer stacks on top of. As a monad transformer, this would be the `m` parameter, but when we use Backpack, we move that out into a separate module.

The implementation of `Control.Carrier.Backpack.Empty.Maybe` is short, and almost identical to the body of `Control.Monad.Trans.Maybe` - we just change any occurrences of `m` to instead refer to `M` from our `.Base` module:

```haskell
{-# language BlockArguments, FlexibleContexts, FlexibleInstances, LambdaCase,
      MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}

module Control.Carrier.Backpack.Empty.Maybe where

import Control.Algebra
import Control.Effect.Empty
import qualified Control.Carrier.Backpack.Empty.Maybe.Base as Base

type M = EmptyT

-- We could also write: newtype EmptyT a = EmptyT { runEmpty :: MaybeT Base.M a }
newtype EmptyT a = EmptyT { runEmpty :: Base.M (Maybe a) }

instance Functor EmptyT where
  fmap f (EmptyT m) = EmptyT $ fmap (fmap f) m

instance Applicative EmptyT where
  pure = EmptyT . pure . Just
  EmptyT f <*> EmptyT x = EmptyT do
    f >>= \case
      Nothing -> return Nothing
      Just f' -> x >>= \case
        Nothing -> return Nothing
        Just x' -> return (Just (f' x'))

instance Monad EmptyT where
  return = pure
  EmptyT x >>= f = EmptyT do
    x >>= \case
      Just x' -> runEmpty (f x')
      Nothing -> return Nothing
```

Finally, we make sure that `Empty` can handle the `Empty` effect:

```haskell
instance Algebra sig Base.M => Algebra (Empty :+: sig) EmptyT where
  alg handle sig context = case sig of
    L Empty -> EmptyT $ return Nothing
    R other -> EmptyT $ thread (maybe (pure Nothing) runEmpty ~<~ handle) other (Just context)
```

## Base Monads

Now that we have a way to run the `Empty` effect, we need a base case to our transformer stack. As our transformer is now built out of modules that conform to the `Control.Monad.Signature` signature, we need some modules for each monad that we could use as a base. For this POC, I've just added the IO monad:

```
library fused-effects-lift-io
  hs-source-dirs:   src-fused-effects-backpack
  default-language: Haskell2010
  build-depends:    base
  exposed-modules:  Control.Carrier.Backpack.Lift.IO
```

```haskell
module Control.Carrier.Backpack.Lift.IO where
type M = IO
```

That's it!

## Putting It All Together

Finally we can put all of this together into an actual executable. We'll take our library code, instantiate the monad to be a combination of `EmptyT` and `IO`, and write a little `main` function that unwraps this all into an `IO` type. First, here's the `Main` module:

```haskell
module Main where

import BusinessLogic
import qualified BusinessLogic.Monad

main :: IO ()
main = print =<< BusinessLogic.Monad.runEmptyT (businessCode True)
```

The `BusinessLogic` module we've seen before, but previously `BusinessLogic.Monad` was a signature (remember, we renamed `Control.Monad.Signature` to `BusinessLogic.Monad`). In executables, you can't have signatures - executables can't be depended on, so it doesn't make sense for them to have holes, they must be complete. The magic happens in our Cabal file:

```cabal
executable test
  main-is:          Main.hs
  hs-source-dirs:   exe
  build-depends:
    , base
    , business-logic
    , fused-effects-empty-maybe
    , fused-effects-lift-io
    , transformers

  default-language: Haskell2010
  mixins:
    fused-effects-empty-maybe (Control.Carrier.Backpack.Empty.Maybe as BusinessLogic.Monad) requires (Control.Carrier.Backpack.Empty.Maybe.Base as BusinessLogic.Monad.Base),
    fused-effects-lift-io (Control.Carrier.Backpack.Lift.IO as BusinessLogic.Monad.Base)
```

Wow, that's a mouthful! The work is really happening in `mixins`. Let's take this step by step:

1. First, we can see that we need to mixin the `fused-effects-empty-maybe` library. The first `(X as Y)` section specifies a list of modules from `fused-effects-empty-maybe` and renames them for the `test` executable that's currently being compiled. Here, we're renaming `Control.Carrier.Backpack.Empty.Maybe` as `BusinessLogic.Monad`. By doing this, we satisfy the hole in the `business-logic` library, which was otherwise incomplete.

2. But `fused-effects-empty-maybe` itself has a hole - the base monad for the transformer. The `requires` part lets us rename this hole, but we'll still need to plug it. For now, we rename `Control.Carrier.Backpack.Empty.Maybe.Base`).

3. Next, we mixin the `fused-effects-lift-io` library, and rename `Control.Carrier.Backpack.Lift.IO` to be `BusinessLogic.Monad.Base`. We've now satisfied the hole for `fused-effects-empty-maybe`, and our executable has no more holes and can be compiled.

## We're Done!

That's "all" there is to it. We can finally run our program:

```haskell
$ cabal run
Just 42
```

If you compare against `businessCode` you'll see that we got passed the `guard` and returned `42`. Because we instantiated `BusinessLogic.Monad` with a `MaybeT`-like transformer, this `42` got wrapped up in `Just`.

# Is This Fast?

The best check here is to just look at the underlying code itself. If we add

```haskell
{-# options -ddump-simpl -ddump-stg -dsuppress-all #-}
```

to `BusinessLogic` and recompile, we'll see the final code output to STDERR. The core is:

```haskell
businessCode1
  = \ @ sig_a2cM _ b_a13P eta_B1 ->
      case b_a13P of {
        False -> (# eta_B1, Nothing #);
        True -> (# eta_B1, lvl1_r2NP #)
      }
```

and the STG:

```haskell
businessCode1 =
    \r [$d(%,%)_s2PE b_s2PF eta_s2PG]
        case b_s2PF of {
          False -> (#,#) [eta_s2PG Nothing];
          True -> (#,#) [eta_s2PG lvl1_r2NP];
        };
```

Voila!

# Conclusion

In this post, I've hopefully shown how we *can* use Backpack to write effectful code without paying the cost of abstraction. What I didn't answer is the question of whether or you not you *should*. There's a lot more to effectful code than I've presented, and it's unclear to me whether this approach can scale to the needs. For example, if we needed something like `mmorph`'s `MFunctor`, what do we do? Are we stuck? I don't know! Beyond these technical challenges, it's clear that Backpack here is also not remotely ergonomic, as is. We've had to write *five* components just to get this done, and I pray for any one who comes to read this code and has to orientate themselves.

Nonetheless, I think this an interesting point of the effect design space that hasn't been explored, and maybe I've motivated some people to do some further exploration.

The code for this blog post can be found at https://github.com/ocharles/fused-effects-backpack.

Happy holidays, all!
