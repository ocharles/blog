---
title: 24 Days of Hackage: text
---

Text processing is one of the many tasks in computer programming that seems like
it should be simple, but in reality can be an absolute nightmare to get
right. Programmers make assumptions about the characters they need to deal with,
often forgetting about other scripts, and the volume of data you need to process
rapidly increases so you also need libraries which are fast.

[Bryan O'Sullivan](http://www.serpentine.com/blog/)'s
[`text`](http://hackage.haskell.org/package/text) library is the current de
facto library for handling human readable text. The `text` library consists of
the `Text` type, and a comprehensive API for manipulating pieces of text
efficiently. I'm not qualified to talk about the performance, but that's OK -
some great minds have
[already done this](http://www.serpentine.com/blog/2009/10/09/announcing-a-major-revision-of-the-haskell-text-library/).

While performance is clearly excellent, its not the reason I chose to write
about `text`. The real win of `text` for me is in the type. `Text` very clearly
indicates that you are working with text, rather than arbitrary binary
data. There are only a few ways to introduce `Text` values too - you either need
a `String`, or you have to specifically decode to `Text`, for example with
`decodeUtf8`. This sounds so simple, and it is, but in my previous experience
with Perl, forgetting to perform this vital step meant it was far too easy to
treat over-the-wire data as text before it was decode. More often than not, this
would lead to small explosions in other places much later.

This class of bugs is simply not possible anymore. If you assume you have human
readable text then you have a `Text` value. If you don't, GHC will be very happy
to point that mistake out to you, and refuse to let you build your application
until it is corrected.

Text provides a little more, but the basic API and the ability to encode and
decide is likely where you'll spend the majority of time. However, it's almost
Christmas, and one present just won't do!

There are also a handful of packages on Hackage that provide even more text
functionality. If you've ever done heavy text processing before then you've
probably heard of the ICU library. Haskell bindings to ICU exist in the
`text-icu` package, giving you the ability to perform unaccenting, collation and
more.

Perhaps you didn't find
[`parsec`](/posts/2012-12-10-24-days-of-hackage-parsec.html) so appealing, and
would rather use regular expressions. I think you're mad, but that doesn't mean
you're out of luck - `text-icu` is one of a handful of regular expression
packages that is capable of working with `text`.

The text library really is one of the most valuable libraries in on Hackage in
my opinion, and thankfuly, not at the cost of speed of simplicity. It's also
part of the Haskell platform, so once you master this interface, your knowledge
is extremely [transferable](http://packdeps.haskellers.com/reverse/text).
