---
title: 24 Days of Hackage: 2013 in Review
---

Welcome, one and all, to the 2013 installment of 24 Days of Hackage! After a
phenomenal reception last year, I'm back again with another collection of
libraries to blog about. Not only that, but I've also got a handful of wonderful
guest posts to compliment these. From rendering graphics to formatting text, to
distributed programming, to constraint solving... I certainly have a lot of
goodies to share with you.

But first, let's start slow, and spend some time looking back on the last year
and see how things have progressed since 2012.

A lot has changed in the Haskell landscape, but of particular interest
to this series is that Hackage itself has been upgraded to
Hackage 2. Hackage is the infrastructure that we use in Haskell to
share libraries, and the new version is a complete rewrite. Hackage 2
launched in October, and this a rewrite is build using much more
maintainable code, and provides a considerably more modular
infrastructure. This seems to be paying off nicely, as the
[contributor count](https://github.com/haskell/hackage-server/graphs/contributors)
is slowly rising.

Most of us work with Cabal when we work with Hackage, and Cabal has
also been updated to make programming in Haskell even more
enjoyable. Cabal 1.18 came out, bringing with it sandboxes (for
Python-like virtual environments to try and avoid the so-called "Cabal
hell"), an easier REPL to launch GHCI, convenience commands to launch
executables and tests, and
[much more](http://coldwa.st/e/blog/2013-08-21-Cabal-1-18.html).

Inside Hackage, the libraries themselves have been changing rapidly. Since the
start of the year, we've gone from approximately 4.8k to 5.7k packages - almost
1000 new packages this year, and that's not even accounting for updates!
Specifically, Hackage has gone from hosting 28k versions of libraries to 34k -
so I think that gives you one more data point to show that we're far more than
a "academic" language that some people may think.

Of libraries that saw a lot of activity, of the libraries we covered
last year, the following stand out:

* [`postgresql-simple`](http://hackage.haskell.org/package/postgresql-simple)
  continued to mature in 2013. The API remains fairly consistent, but
  functionality has grown to deal with more of PostgreSQL's feature
  set. Of major interest to users of this database will be the support
  for array types, `COPY` support,
  [hstore](http://www.postgresql.org/docs/9.1/static/hstore.html)
  support, and plenty of bug fixes.

* [`lens`](http://lens.github.io/) continues its plans for
  world-domination, and the `lens` team have now discovered new
  abstractions that have lead to an even stronger formalization of its
  internals. The `lens` team discovered more uses for `profunctors`
  inside `lens`, and this has lead to more compatibility between the
  various `Traversals`, `Folds`, `Prisms`, and so on. Internals aside,
  `lens` has expanded to give you even more batteries, maybe one could
  say it now comes power-station-included - and continues to gain
  traction in the Haskell ecosystem.

* [`pipes`](http://hackage.haskell.org/package/pipes) moved into its 4th
  iteration, and also benefited from discovering new fundamental
  abstractions. `pipes-4.0` features a much more concise API that is equally
  expressive to it's predecessors, and Gabriel has reached a point where he is
  now moving out from the experimental design stage to expanding the breadth of
  `pipes`. Specifically, `pipes` now has answers to stream parsing, concurrency
  and resource finalizing, while also beginning to provide compatibility with
  other Haskell libraries, such as in `pipes-bytestring`.

* The [Snap](http://snapframework.com) web framework saw more activity,
  specifically with a release of the `io-streams` library for streaming data,
  which the next version of Snap will be built around - though this is yet to be
  released. The Snap team also reworked their "Heist" templating system to
  support interpreted splices as before, but also compiled splices, which have
  the potential to render even faster.

But don't be tricked into thinking this is the *only* activity - far
from it. Many of the other libraries I wrote about last year have
continued to move with the rest of Hackage - updating dependencies,
adjusting to new APIs, and integrating feature requests (and pull
requests) from their ever-expanding user bases.

Tomorrow we'll get the ball rolling for good. What will we cover? You'll just
have to wait and see...
