---
title: "24 Days of Hackage: QuickCheck"
---
As I mentioned in the article on [errors](/posts/2012-12-04-errors.html), the
Haskell programmer takes error handling and edge cases very seriously. While we
try and constrain our types as much as possible, there is always a trade off
between exact types and pragmatism, not to mention that there are some
invariants that are very difficult to encode in the Haskell type system. As
such, without rigorous testing, there is still a risk of exceptions or
unexpected behaviors at runtime.

The solution is, of course - testing! And if we really want to be confident in
our application, we need to be certain we have tested it under all possible
inputs. Again, there is a trade off to be made - you could logically reason
about your program, proving things by induction and so on, but this is a
demanding task, and one that goes beyond a lot of programmers abilities. The
solution most people are used to, is to manually generate some test data that
exercises different aspects of a system, and hope it's good enough. While you
can do that in Haskell, there's also a third option.

[QuickCheck](http://hackage.haskell.org/package/QuickCheck) is a library for
doing *random* testing. This means that rather than have the programmer write
test data, `QuickCheck` will generate random data for you. It sounds naive,
doesn't it? It's a simple solution, but an incredibly powerful one - in fact
many people swear by it.

Let's dig in with an example!

```haskell
absAverage :: [Double] -> Double
absAverage ds = sum ds / fromIntegral (length ds)
```

My intention with this function was to take the average of the absolute value of
all values in a list. So, lets write a property to make sure this is correct:


```haskell
quickCheck1 :: IO ()
quickCheck1 = quickCheck $ \x -> absAverage x >= 0

> quickCheck1
*** Failed! Falsifiable (after 1 test):
[]
```

Here I've asserted the property that "for all lists of integers, `x`,
`absAverage x` is positive." But `QuickCheck` made light work of that property
and quickly proved us wrong! `QuickCheck` generated some test data for us - in
this case the empty list - and our property didn't hold - because our function
doesn't make sense for empty lists. We can weaken the property a bit to only
consider non-empty lists:

```haskell
quickCheck2 :: IO ()
quickCheck2 = quickCheck $ \x -> length x > 1 ==> absAverage x >= 0

> quickCheck2
*** Failed! Falsifiable (after 2 tests and 3 shrinks):
[-2.0,1.0]
```

Huh, a failure again... Oh! I forgot to actually take the `abs` value of each
element of `ds` in my original function, lets get that fixed...

```haskell
absAverage :: [Double] -> Double
absAverage ds = sum (map abs ds) / fromIntegral (length ds)

> quickCheck2
+++ OK, passed 100 tests
```

Alright!

QuickCheck can do a *lot* more than this, but in the spirit of these articles
I'm only trying to scratch the surface - it's up to you to do the extra
reading. Thankfuly, there's a lot of great material already published. Check out
the
[introduction to QuickCheck](http://www.haskell.org/haskellwiki/Introduction_to_QuickCheck)
on the [Haskell wiki](http://www.haskell.org/haskellwiki), or the excellent
chapter on Haskell testing in
[Real World Haskell](http://book.realworldhaskell.org/read/testing-and-quality-assurance.html).

It's worth noting that `QuickCheck` is not the only library of this ilk; the
[`smallcheck`](http://hackage.haskell.org/package/smallcheck) operates under a
similar principle, however `smallcheck` tries to build random data of various
'depth', based on the assumption that "If a program fails to meet its
specification in some cases, it almost always fails in some simple case."

Now that you have `QuickCheck` you don't even have to think about generating
data for your tests, and you can concentrate on the essential properties. In
other words - no excuses!
