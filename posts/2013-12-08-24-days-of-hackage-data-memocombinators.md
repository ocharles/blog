---
title: 24 Days of Hackage: data-memocombinators
---

Today we look at a tiny little library that packs an awfully strong punch for
it's size. That library is
[data-memocombinators](http://hackage.haskell.org/package/data-memocombinators),
maintained by [Luke Palmer](http://lukepalmer.wordpress.com/) and
[Dmitry Malikov](https://github.com/dmalikov).

`data-memocombinators` is one of a handful of
[memoization](http://en.wikipedia.org/wiki/Memoization) libraries available for
Haskell, and to me, stands out for its simplicity. Before we go into the
library, lets recap on what it means for a function to be memoized, and why we
would need a library for this in Haskell.

Memoization is a technique for trading run-time space for execution time, and is
often used to improve the performance of heavily recursive definitions in order
to save recomputing the same information over and over again. One of the
canonical examples of memoization is the Fibonacci series. Naively, one might
write this as:

```haskell
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```

However, to compute `fib 4` we have to compute `fib 3` and `fib 2`. To compute
`fib 3` we also need to compute `fib 2` and `fib 1`. Thus we actually need to
compute `fib 2` *twice* - once for `fib 4` (n - 2) and once for `fib 3` (n -
1). There is already plenty of material on this, so I'll leave further research
for you to do, if you've not already encountered this.

Of interest to the working Haskell programmer is the question: why do I care?
Surely the calculation `fib 2` is the same every time - after all, `fib :: Int ->
Int`, and due to referential transparency, the result of this computation
applied to the same argument must be constant. Why can't our compiler help us out
and prevent duplicate work?

The answer is due to how terms are evaluated. When we assign a value to a name,
we usually store an unevaluated *thunk*, that when *forced* will finally
produce the value under that computation. Once forced, the binding now points to
the final value, and not the computation. Thus, subsequent accesses will incur
almost no cost at all. This is why in `let y = 4 * 4 in y + y`, we would only
calculate `4 * 4` once. Once these bindings go out of scope, the garbage
collector is free to come along and remove all of this work.

Now we are in a position to understand the poor performance of our `fib`
definition. In `fib`, we never bound recursive calls to names, and so there is no
ability to share information between calls. Therefore, there is no ability to
share the work, so we end up creating new thunks every time we recurse.

Confused? Don't worry I was too, and it's a tricky topic. Thankfully, you're not
the first person to be confused, and there is some great information on
[Stack Overflow](http://stackoverflow.com) discussing this problem:

* [How is this fibonacci-function memoized?](http://stackoverflow.com/questions/11466284/how-is-this-fibonacci-function-memoized?lq=1)
* [Why does memoization not work?](http://stackoverflow.com/questions/12019984/why-does-memoization-not-work)

While it is often possible to reformulate code to achieve sharing, sometimes
it's just more convenient to stick on some explicit memoization over the top,
and that's what `data-memocombinators` is all about.

`data-memocombinators` provides a few combinators that transform a function into
an equivalent function that uses it first argument as the memoization key. There
are no data structures that you as a user have to worry about, this is all taken
care behind the scenes. Thus the code is exactly as expressive as before - you
are not restricted to working under newtypes or passing around explicit
caches. Remarkably, `data-memocombinators` is entirely pure - which at first
glance seems impossible. After all, isn't memoization about mutating some sort of
shared store of data? With lazy evaluation available, we'll see that it's
possible to get memoization *and* purity.

For example, we can add memoization to our `fib` example from earlier, by the
use of the `integral` combinator. This is the same example from the
`data-memocombinators` documentation, but I'll reproduce it here to discuss how
it works:

```haskell
fibFast :: Int -> Int
fibFast = Memo.integral fib'
 where fib' 0 = 1
       fib' 1 = 1
       fib' n = fibFast (n - 1) + fibFast (n - 2)
```

The type of our `fib`-like function has not changed, but we've moved the work
out into a locally defined `fib'` function. Notice the relation between `fibFast`
and `fib'` - when we call `fibFast` we first check (indirectly) if this value
has already been computed, and if not, we use `fib'` to perform the work. `fib'`
then calls `fibFast` with smaller value, and the cycle repeats. Now we've got
the sharing that was mentioned earlier, and we only have to pay for the cost of
computing `fib n` once (for each `n`).

But how on earth does this all work? `data-memocombinators` is built by building
an infinitely large trie. Each value in the trie can be thought of as a thunk to
compute that value. Building up the thunks is almost free, and it's only when we
lookup the value for a specific key (function argument) that we pay for the
work. After that, we still have a binding to that value, and that's why
subsequent accesses don't require the work to be calculated again.

Now while `data-memocombinators` has a simple interface that is powerful to use,
it does sadly only go so far. For example, there is no general memoization
routine for types where all we have is an `Eq` instance. This seems like it
*should* be possible, but unfortunately we are given no such
combinator. However, `data-memocombinators` does have some points for
extension. If you are able to define a `Bits` instance on the result type, then
you can use the `bits` combinator to build a memoization routine, to consider
one example.

When you do need to start working with more arbitrary types you have a few
options. If you want to stay in `data-memocombinators` you may be able to form
an isomorphism with your type and something that is easily memoized - for
example, an isomorphism to a unique `Integer`. The other option is to bite the
bullet and use something more powerful, like working inside a `Map` - here
[`monad-memo`](http://hackage.haskell.org/package/monad-memo) may be more
help.

I'll finish by saying that not only is `data-memocombinators` a very powerful
library, it's a fantastic starting point for exploring some programming
techniques that truly shine in Haskell. The underlying "trick" in
`data-memocombinators` is really due to the idea of sharing in lazy
evaluation. `data-memocombinators` is a tiny amount of code, and I highly
encourage you to dig into the guts of this library and teach yourself how it all
works. I guarantee, even if you don't have a need to use `data-memocombinators`,
you'll come away feeling a little more enlightened.
