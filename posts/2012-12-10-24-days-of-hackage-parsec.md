---
title: 24 Days of Hackage: parsec
---

There comes a time in every programmer's life when a plain old string just won't
cut it anymore - you need more structure. However, the more you think about
this, the more you are terrified of the prospect of having to write some sort of
parser, or maybe it's time to put on some protective clothing on and fight
through a forest of regular expression operators and arcane escaping rules. Why
why why, you ask yourself, does it have to be this painful?

Ok, maybe that was a little melodramatic, but I can say that parsing with
Haskell is an activity that I no longer dread, in fact - I find it fun and
exciting! [`parsec`](http://hackage.haskell.org/package/parsec) is a parser
combinator library that provides a set of primitives for parsing. As I've been
stressing throughout this series (and will continue to!), combinators let us
build complex logic out of very small, easy to reason about, pieces.

For the example today, we'll look at parsing
[International Standard Recording Codes](https://en.wikipedia.org/wiki/International_Standard_Recording_Code)
(ISRCs), a problem that I had to solve for some work on
[MusicBrainz](http://musicbrainz.org). The linked Wikipedia does a good job at
explaining the format of ISRCs, so lets dive right in and try and build up
our own ISRC parser. First of all, we need to parse the country code. That's
just two uppercase characters, so lets write that parser:

```haskell
countryCode = count 2 upper
```

Simple! This parser requires two uppercase characters, so we've combined the
`upper` parser with the `count` combinator to run `upper` twice. If this parser
succeeds, it will return the two uppercase characters that it parsed. Moving on,
the next section of data we need to parse is "a three character alphanumeric registrant code":

```haskell
regCode = count 3 upperNum
  where upperNum = upper <|> digit
```

Much like our `countryCode` parser, we've combined the `upperNum` parser with
the `count` combinator to run it 3 times. `upperNum` can be defined by parsing
either an `upper` character (A-Z) or a `digit` (0-9). The `<|>` combinator, part
of the `Alternative` type class, allows us to try one parser and if that fails,
try another.

Next, we take the last two digits of the year of registration:

```haskell
regYear = count 2 digit
```

And finally, we take the five digit number identifying the recording:

```haskell
recordingId = count 5 digit
```

All that we need to do now is thread all these parsers together, and we're done!
One option could be to do the following:

```haskell
data ISRC = ISRC { isrcCountryCode :: String
                 , isrcRegCode :: String
                 , isrcRegYear :: Int
                 , isrcRecording :: Int
                 } deriving (Show)

isrcParser = ISRC <$> countryCode
                  <*> regCode
                  <*> fmap read regYear
                  <*> fmap read recordingId
                  <*  eof

λ> parse isrcParser "" "USPR37300012"
Right (ISRC { isrcCountryCode = "US"
            , isrcRegCode = "PR3"
            , isrcRegYear = 73
            , isrcRecording = 12})
```

Here I've used the `Applicative` interface to build a parser that gradually
builds up an `ISRC` value, and also the `Functor` instance to convert
`regCode`'s into an `Int`, as `ISRC` requires. I've finished off by using `eof`
to indicate that the parser should fail if there is any left over input.

However, this isn't the only way to use `parsec`, though it's certainly a
convenient way. In practice, I actually only wanted to perform validation and
normalization, so I can get by just sequencing these parsers and then stitching
things back together:

```haskell
isrcParser₂ = mconcat <$>
  sequence [ countryCode, regCode, regYear, recordingId ]
    <* eof

λ> parse isrcParser₂ "" "USPR37300012"
Right "USPR37300012"
```

This parser uses the `sequence` combinator for `Monad`s to run a bunch of
parsers, and collect all their results - then I just `mconcat` the results into
a single `String`.

Oh, did I just mention parsers are `Monad`s? This gives us even more power - for
example, we could branch out and run different parsers depending on what we've
parsed so far. Not only that, but they are monad *transformers*, which gives us
even more power. If we used a base monad such as `IO`, it's entirely possible to
write a parser that looks up things in a database while it does the
parsing. Maybe a little bit on the crazy side, but it's all possible!
