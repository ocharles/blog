---
title: "24 Days of Hackage: sbv"
author: Tikhon Jelvis <tikhon@jelv.is>
---

I think you've all heard me ramble on about the libraries I find interesting, so
today it's with great pleasure that I turn the spotlight over to
[Tikhon Jelvis](http://jelv.is/). Enjoy!

----

Today we're going to talk about [`sbv`](http://hackage.haskell.org/package/sbv),
a library for interacting with SMT solvers. SBV stands for "SMT-based
verification"; SMT itself stands for "SAT modulo theories" and SAT is "Boolean
satisfiability" --- three layers of acronyms! Luckily, they're reasonably easy
to follow. Let's start from the bottom.

[Boolean satisfiability](http://en.wikipedia.org/wiki/Boolean_satisfiability_problem)
(SAT) is one of the canonical NP-complete problems. The problem is based on
Boolean formulas: boolean variables combined with AND ($\land$), OR ($\lor$) and
NOT ($\lnot$). Here's an example formula with variables $a$, $b$, $c$, $d$ and
$e$:

$$ (a \lor b \lor c) \land (a \lor \lnot d \lor \lnot e) \land (\lnot y \lor c
\lor e) $$

The goal is to figure out if a Boolean formula is "satisfiable" --- that is, if
there is some way to assign all the variables to `true` or `false` that results
in the entire formula evaluating to `true`.

SAT is interesting because it can be easily used to express other (usually also
NP-complete) problems. While SAT, being NP-complete, is hard in general, most
real-world instances tend to be reasonably tractable. A fast algorithm to solve
SAT could be used in quite a surprising variety of real-world contexts ---
things like verifying and optimizing circuit design or automated theorem
proving.

[SMT](http://en.wikipedia.org/wiki/Satisfiability_modulo_theories) is a popular
extension to SAT which adds "theories" to the basic logic SAT supports. These
theories let us write formulas using more than just Boolean constraints:
examples include bitvectors, arbitrary-sized integers and arrays, records or
even algebraic numbers. These extensions make encoding various domain-specific
problems much easier with SMT than with just SAT; moreover, SMT solvers can use
high-level knowledge about the constraints to implement particular theories more
efficiently.

SMT solvers are useful for most problems that can be recast as a set of
constraints, like dependency resolution for package managers. They are also very
useful for working with programming languages: most common language features are
easy to express using basic SMT theories. SMT solvers can be used to verify code
against some sort of logical specification (like
[Liquid Haskell](http://goto.ucsd.edu/~rjhala/liquid/haskell/blog/about/)) or
even synthesize new algorithms[^2].

[^2]: [This article](http://cis.upenn.edu/~alur/CIS673/smt11.pdf) from the
*Communications of the ACM* contains more examples, use cases and background
information on SMT solvers.

`sbv` is a library for conveniently interacting with SMT solvers. It lets you
write Haskell programs that produce a formula that can be sent off to an SMT
solver. These formulas are created by using "symbolic" versions of normal data
types like `Boolean`s and various `Word`s, prefixed by an "`S`", such as
`SBool`, `SWord` and so on. "Symbolic" refers to the fact that the types do not
represent actual booleans or numbers in Haskell; instead, they are variables in
a formula to be passed to the solver. Functions[^1] written with these symbolic
types can be compiled to logic formulas and passed to the solver.

[^1]: The solver works on a bunch of different types so most of the operators
are polymorphic. This means that you have to specify the type you're working
with somewhere. In these examples, I add a type annotation to a lambda:

        \ (x::SInteger) y -> ...

    To do this, you have to turn on `ScopedTypeVariables`. In GHCi, you can do this with:

        > :set -XScopedTypeVariables

    Chances are that you will have to do this more often in GHCi than in normal code because GHCi leaves less scope for types to be inferred.

To work with `sbv`, you need to install an external SMT solver. The library
currently supports four different backends, with Microsoft's
[Z3](http://z3.codeplex.com/) having the most features, making it the default
choice. Currently, Z3 is the fastest overall solver based on the SMT
competition. It is available under an MIT license, suitable for any purpose
including commercial use.

Once you've installed Z3 and have it in your path, you can start playing around
in GHCi by importing `Data.SBV`.

The simplest example we can do is prove something like the fact that `x + x`
always equals `x * 2`. This use is very similar to QuickCheck except that the
solver is *exhaustive* --- it proves the property for *all* possible values:

    > prove . forAll ["x"] $ \ (x::SWord8) -> x * 2 .== x + x

    Q.E.D.
    
The string `"x"` in the `forAll` is what the corresponding variable will be
named in the solver. This is the name used in the output of the solver and is
really useful for debugging. It helps to call the corresponding Haskell variable
(the argument in the lambda) the same name, but it isn't obligatory.

If the property does not hold, the solver gives us a counter-example:

    > prove . forAll ["x"] $ \ (x::SWord8) -> x * 3 .== x + x

    Falsifiable. Counter-example:
      x = 255 :: SWord8
      
We can also solve for values that satisfy our constraints. For example, we could
find a solution to a system of equations:

    > sat . forSome ["x", "y"] $ \ (x::SInteger) y ->
        x^2 + y^2 .== 25 &&& 3 * x + 4 * y .== 0

    Satisfiable. Model:
      x = 4 :: SInteger
      y = -3 :: SInteger
      
These constraints don't even have to have a solution. If there is no solution,
the solver can often tell you, even if you're using *unbounded* integers!

    > sat . forSome ["x", "y"] $ \ (x::SInteger) y ->
        x^2 + y^2 .== 42 &&& 3 * x + 4 * y .== 0

    Unsatisfiable

If there is more than one solution, the solver will just choose one.

    >  sat . forSome ["x", "y", "z"] $ \ (x::SInteger) y z ->
         x + 2 * y .== 3 * y + z + x^2 &&& x ./= y

    Satisfiable. Model:
      x = 1 :: SInteger
      y = 0 :: SInteger
      z = 0 :: SInteger
      
You could instead get *all* the satisfying assignments by using `allSat` in
place of `sat`.

While certainly interesting as an interactive tool, the solver really comes into
its own when you can use its results in the rest of your program. We can do this
with `extractModel` which returns a "model" for our formula, which is just a set
of assignments to variables that satisfies all the constraints --- a single
solution to our formula.

    > res <- sat . forSome ["x", "y"] $ \(x::SWord8) y ->
        x^2 + y .== 10 &&& y^2 + x^3 .== 100
    > extractModel res :: Maybe (Word8, Word8)
    Just (251,241)

`extractModel` is polymorphic over the sort of types it can output, and `sbv`
knows how to construct different containers. For example, you could get a list
instead of a tuple:

    > extractModel res :: Maybe [Word8]
    Just [251,241]
    
In the case of `prove`, `extractModel` is reversed: if the formula is
satisfiable, it returns `Nothing` and otherwise it returns a counterexample. For
a function like `allSat` with multiple results, you can use the `extractModels`
function to get all of the models.

Playing around with `sbv` at the REPl is a good way to get a handle on how the
package works and how SMT solvers can be used. For actual programs, chances are
you will want to work with significantly more complicated SMT formulas; `sbv`
provides a monad for stringing together different assertions in a formula and
generating variables programmatically which can be very useful.

The
[`sbv` documentation](http://hackage.haskell.org/package/sbv-2.10/docs/Data-SBV.html)
is a good reference for the capabilities and constructs `sbv` offers. The
package also comes with a few deeper examples which are well worth a look.

----

This <span xmlns:dct="http://purl.org/dc/terms/" href="http://purl.org/dc/dcmitype/Text" rel="dct:type">post</span> is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc-nd/3.0/">Creative Commons Attribution-NonCommercial-NoDerivs 3.0 Unported License</a>.
