---
title: 24 Days of Hackage: HLint
---

In my [first post](/posts/2012-12-01-24-days-of-hackage.html) I mentioned
that Cabal is capable of building executables, but so far we've only looked at
libraries. [Hackage](http://hackage.haskell.org) also contains a plethora of
tools for the working Haskell programmer, and today we will look at HLint.

[HLint](http://community.haskell.org/~ndm/hlint/), created by
[Neil Mitchell](http://community.haskell.org/~ndm/), is a tool that takes
Haskell `.hs` files as input, and analyses them to produce hints on better ways
to write code - be that more performent, more succinct or more
idiomatic. Because of this, HLint is frequently suggested to beginners - but it's
still a tool I frequently use! Though maybe I'm really still just a beginner...

It would be a rather long article if I were to enumerate all of the hints HLint
can give you; instead I will concentrate on a few that have helped further my
Haskell knowledge.

To the beginner, Haskell seems full of operators and combinators, and has a
somewhat alien way of calling functions. This threw me for a while - "how on
earth am I meant to remember the precedance for function application?!" and "why
can't I drop the brackets here?" were amongst the questions I found myself
asking. HLint helped clear a lot of this confusion for me: instead, I started
liberally using parentheses and let HLint guide me to write more idiomatic
Haskell. For example, we could start with the following code.

```haskell
[(10 + 20), 9] == (map (\x -> x * 3) [10, 3])
```

HLint, after a few passes, reduces this code to:

```haskell
[10 + 20, 9] == map (* 3) [10, 3]
```

Which is certainly much cleaner! By repeating this exercise, it all started to
click a litle bit more.

HLint is also aware of common programming patterns too, which has been another
way to strengthen my knowledge, especially in developing intuition for how all
the pieces fit together. All the various monad combinators seemed a little
mythical to me at first, and I would often write code such as:

```haskell
getLine >>= return . map toUpper
```

But this can be simplified by the `liftM` combinator:

```haskell
liftM (map toUpper) getLine
```

Why not indeed - thanks HLint!

## Writing your own hints

HLint also allows you to extend it with your own hints - a feature that I think
can be over looked, but is nonetheless very useful, and likely essential for
large team who wish to write consistent code. I personally tend to prefer the
use of infix `fmap` via `<$>` - so ideally I'd like `fmap x foo` to produce a
hint to change it to `x <$> foo`.

```
warn = fmap f x ==> f <$> x
```

And now if we try:

```haskell
fmap (map toUpper) getLine
```

HLint rightfully suggests:

```
      ocharles.org.uk/foo.hs:3:8: Warning: Use <$>
Found:
  fmap head getLine
Why not:
  head <$> getLine

```

Simple! HLint features literally
[hundreds](http://community.haskell.org/~ndm/darcs/hlint/data/Default.hs) of
hints out the box, which are already enough to get going with. If your
participating in [24 Pull Requests](http://24pullrequests.com) this year, HLint
can be a great way to make pull requests on code that is otherwise too difficult
for you - after all, which maintainer doesn't want pretty, consistent code?
