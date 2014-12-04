---
title: 24 Days of GHC Extensions: Welcome!
---

Hello everyone, I'm pleased to announce it's that time of the year again!
Following in the tradition of
[24 Days of Hackage 2012](/pages/2012-12-01-24-days-of-hackage.html) and
[2013](/pages/2013-12-01-24-days-of-hackage.html), it's time for another advent
calendar. However, this year I felt it could be fun to try something a little
different.

The previous years have concentrated on using libraries available on Hackage,
and this is naturally a very important part of working in Haskell. However, it's
one thing to use libraries, but it's another to just write code - and for this
it's important to note just what the language itself has to offer.

Haskell itself is a well defined language - officially specified by the
[Haskell '98 report](https://www.haskell.org/onlinereport/), and the subsequent
[Haskell 2010 report](https://www.haskell.org/onlinereport/). Language
implementers needn't stop there though, as Haskell is meant to be a rich
playground for experimenting with new functional programming constructs. For
many years, GHC has embraced the notion of *extensions* - opt in functionality
that gives the user even more tools when writing their programs.

GHC extensions vary from basic syntax sugar, to meta-programming, all the way to
full type system extensions. Remarkably, the extension system is extremely
mature - where many of the extensions transparently extend syntax, error
messages, and is far from an ad hoc approach to extending the language. In fact,
it's hard to avoid using GHC extensions these days - the power and productivity
gains they bring is simply too much to ignore.

Over the next few weeks, we'll explore just a handful of the extensions that GHC
supports - a list which as of GHC 7.8 is almost exceeding 100 extensions! I'm
also happy to announce that this year will feature some guest posts once again,
so the lead up to Christmas should packed with excitement.

If you're interested in writing a guest post, please do get in touch - there are
still spaces! If there's any extension that excites you, or crops up a lot in
your programming, I highly encourage you to volunteer a post about it. As with
previous years, I would love it if 24 Days can become a platform for those new
to blogging to get their feet wet, along with a fantastic learning resource.

On with the show!

----

*This post is part of
[24 Days of GHC Extensions](/pages/2014-12-01-24-days-of-ghc-extensions.html) -
for more posts like this, check out the
[calendar](/pages/2014-12-01-24-days-of-ghc-extensions.html)*.
