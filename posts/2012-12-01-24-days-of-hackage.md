---
title: 24 Days of Hackage: Cabal
---

Over the next 24 days I'm going to be posting a series of reviews on some of my
favourite libraries on [Hackage](http://hackage.haskell.org). This will be a
whirlwind tour of some modules that I use on an almost daily basis, including
modules that have inspired me, modules that have changed the way I think about
code, and some modules that are so amazing I'm not even smart enough to use
them!

I'm motivated to do this after spending a few years in the Perl community, which
often has advent calendars - some notable ones include the
[Perl 6 advent calendar](http://perl6advent.wordpress.com/), the
[Catalyst advent calendar](http://www.catalystframework.org/calendar/2011), and
many more. I've got a lot out of those calendars in the past, and I haven't seen
any for Haskell yet. I hope I can inspire some people with these posts to try
their own advent calendars, they are a fantastic resource for knowledge sharing.

As a quick disclaimer, I don't think I'm really an expert on much of the stuff
I'm going to be writing about, but if I can introduce at least one person to one
new idea, then I'll happily call this project has been a success. So, with that
out of the way, lets get on with the show - and what better project to begin
with than Cabal!

Cabal, the Common Architecture for Building Applications and Libraries, is a set
of libraries and tools for managing the packaging and deployment of Haskell
projects. Cabal is undeniably one of the most used tools by the working Haskell
programmer, and while it's not perfect it's certainly one of the slickest
packaging tools I've used in all the languages I've worked with.

At a basic level, Cabal will let you specify some basic metadata about your
project - the name of it, some version information
([and even that is formally specified, in true Haskell spirit](http://www.haskell.org/haskellwiki/Package_versioning_policy)),
copyright information and dependencies. One thing I really like about the cabal
format is that in a single .cabal file I can package multiple executables along
with a library. This has really helped me create small, single purpose
applications, while also structuring my code cleanly. The cabal format is
expressive too - you can get up and running with a really basic file, but it
certainly scales to custom build types and all sorts of trickery, if that's what
you need.

Cabal has great integration with other tools in the Haskell eco-system, with
built in support for Haddock for documentation, HPC for code coverage, and a
flexible way of specifying both test suites and benchmarks. I really like the
ability to maintain a local set of documentation for all installed modules by
setting `documentation: True` in my `~/.cabal/config` configuration file - this
has been a life saver several times on long commutes!

Of course, the biggest selling point of cabal is that you get access to all of
Hackage - which is exactly what this series will be about! So, if you want to
experiment with things I'm about to blog about, I recommend you grab yourself
the latest [Haskell platform](http://haskell.org/platform), run `cabal update`
and get ready to start living in GHCi!
