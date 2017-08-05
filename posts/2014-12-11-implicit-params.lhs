---
title: "24 Days of GHC Extensions: Implicit Parameters"
---

> {-# LANGUAGE ImplicitParams #-}
> import Data.Char
 
[Yesterday](/posts/2014-12-10-nullary-type-classes.html) we looked at nullary
type classes - type classes that don't vary over any types - and saw how they
can be used to leave part of a program undefined. Our particular example looked
at building a library that needs to call a logging function, but we leave the
implementation up to the library author.

However, using a nullary type class for this has some drawbacks - the biggest is
that we are now tied to a single choice of logging function. This can be
problematic if we ever need to vary what it means to log throughout the life
time of a program. This may crop up if we decide that we need to transform or
discard log entries at smaller parts of our program.

To solve this, lets rewrite our library to take logging as a parameter to a
function:

> type LogFunction = String -> IO ()
>
> type Present = String
>
> queueNewChristmasPresents :: LogFunction -> [Present] -> IO ()
> queueNewChristmasPresents logMessage presents = do
>   mapM (logMessage . ("Queueing present for delivery: " ++)) presents
>   return ()

This isn't much different from what we saw yesterday, but it quickly becomes
painful to use in practice. Whenever we want to abstract over
`queueNewChristmasPresents`, we have to either choose to commit to a specific
log function, or we have to manually propagate the `LogFunction` to the parent
function.

One solution might be to move the `LogFunction` to a reader monad, but this
still carries a cost and the program will need to be transformed. A less
invasive technique is to use an *implicit* parameter.

Implicit parameters act like parameters to a function, except the caller never
has to apply the function to the argument. Instead, the argument is
automatically passed to the function by merely being in scope. Let's see how
this works out for our logger:

> queueNewChristmasPresents2 :: (?logMessage :: LogFunction) => [Present] -> IO ()
> queueNewChristmasPresents2 presents = do
>   mapM (?logMessage . ("Queueing present for delivery: " ++)) presents
>   return ()

As you can see, something interesting has happened. The `LogFunction` is no
longer a parameter to the function, but is rather part of the *context* of the
function - constraints that must be satisfied when we use the program. The body
of the program is mostly the same, other than the leading `?` that prefixes
implicit parameters.
 
To supply an implicit parameter, all we need to do is bring an appropriately
named variable into scope:

> ex1 :: IO ()
> ex1 =
>   let ?logMessage = \t -> putStrLn ("[XMAS LOG]: " ++ t)
>   in queueNewChristmasPresents2 ["Cuddly Lambda", "Gamma Christmas Pudding"]

```
.> ex1
[XMAS LOG]: Queueing present for delivery: Cuddly Lambda
[XMAS LOG]: Queueing present for delivery: Gamma Christmas Pudding
```

Perfect, we've got the same type of programs as we saw yesterday!

However, we now have the ability to use two different types of loggers in the
same program:

> ex2 :: IO ()
> ex2 = do
>   -- Specifies its own logger
>   ex1
>
>   -- We can locally define a new logging function
>   let ?logMessage = \t -> putStrLn (zipWith (\i c -> if even i
>                                                      then c
>                                                      else toUpper c)
>                                            [0..]
>                                            t)
>   queueNewChristmasPresents2 ["Category Theory Books"]

```
[XMAS LOG]: Queueing present for delivery: Cuddly Lambda
[XMAS LOG]: Queueing present for delivery: Gamma Christmas Pudding
QUeUeInG PrEsEnT FoR DeLiVeRy: CaTeGoRy THeOrY BoOkS
```
 
Neat!
 
Implicit parameters are something of a love-hate extension, to the best of my
knowledge - though it can be mighty useful sometimes. Also, as with many aspects
of Haskell - it is useful for things that were never even anticipated. Tom Ellis
has a blog post that demonstrates how one can ["fake" a module
system](http://h2.jaguarpaw.co.uk/posts/modules-for-lennart/) by using implicit
parameters.
 
----

*This post is part of
[24 Days of GHC Extensions](/pages/2014-12-01-24-days-of-ghc-extensions.html) -
for more posts like this, check out the
[calendar](/pages/2014-12-01-24-days-of-ghc-extensions.html)*.
