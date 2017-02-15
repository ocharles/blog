---
title: 24 Days of GHC Extensions: Type Operators
---

As GHC gradually adopts more and more extensions that allow us to do
dependently typed programming in Haskell, it's natural that we'd like be more
expressive at the type level. Today, we'll look at the
[type operators](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/data-type-extensions.html#type-operators),
which allows us to use more natural syntax when we write our types.

The extension itself is straightforward. As the documentation mentions,
ordinarily an operator doesn't mean anything special when used in types - GHC
would interpret operators as type variables, not particularly useful! However,
now that we've got some support for type level literals, it's desirable to
concisely type out computations using infix operators (for example, addition and
multiplication of natural numbers).

Recently, I was working with a little templating engine using a variant of
[*data types a la carte*](http://www.staff.science.uu.nl/~swier004/Publications/DataTypesALaCarte.pdf). I
wanted to be able to write templated emails, where I can compose emails from
text fragments and variables that need to be filled in later. However, not all
emails have the same variables - a registration email doesn't have the same
variables as a booking confirmation email. To work with this, I decided to use
data types a la carte to represent open data types.

If you're new to this method of programming, we're essentially breaking apart a
data type into a combination of functors. For my email templating, I use the
following:

```haskell
data I a = I { unI :: a }
data Var a x = Var { unK :: a }
```

`I` is the identity functor - we use this to insert text literals. `Var` is a
constant functor - it ignores the data under the functor, instead choosing a
variable as a placeholder. The last remaining piece of the puzzle is the ability
to combine literal text with placeholders. We can do this with one more
functor - the sum functor:

```haskell
data Sum f g a = InL (f a) | InR (g a)
```

Armed with this, we are free to write our emails:

```haskell
data UserVar = UserName | UserEmail

email :: [Sum (Var UserVar) I Text]
email = [ InR (I "Dear "), InL (Var UserName), ", thank you for your recent email to Santa & Santa Inc."]
```

However, the type of email templates becomes messier when we have multiple variables:

```haskell
data ChristmasVar = ChristmasPresent

email :: [Sum (Sum (Var UserVar) (Var ChristmasVar)) I Text]
email = ...
```

What is... Lisp?! Instead, we can define an infix operator, and the type becomes
a little more readable:

```haskell
email :: [(Var UserVar + Var ChristmasVar + I) Text]
email = [ "Dear "
        , var UserName
        , ", thank you for your recent email to Santa & Santa Inc."
        , "You have asked for a: "
        , var ChristmasPresent
        ]
```

There's more work that's needed to complete the email templating, and a [full
code listing can be found on Github](https://github.com/ocharles/blog/blob/master/code/2014-12-08-type-operators.hs). However,
hopefully you can see how a fairly obvious extension (opening up the
restrictions on how we write our type signatures) can find its way into some
interesting applications.

----

*This post is part of
[24 Days of GHC Extensions](/pages/2014-12-01-24-days-of-ghc-extensions.html) -
for more posts like this, check out the
[calendar](/pages/2014-12-01-24-days-of-ghc-extensions.html)*.
