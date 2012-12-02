---
title: Announcing Raskell
---

In July, I was inspired to follow in the footsteps of Chris Done's
[Hackage Enhancement Suite](http://chrisdone.com/hes/) and try my hand at a
similar project, which I'm cheekily dubbing
[Raskell](http://raskell.ocharles.org.uk).

I come from a Perl background, and the new [MetaCPAN](http://metacpan.org)
website has the ability for user's to show their support for libraries by
submitting '+1's. Now, with Raskell, we can do this too!

![Raskell in Action](/img/raskell.png)

## How Raskell Works

Raskell is a [Snap](http://snapframework.com) powered web server, with a user
script that users install in their browser. When you browse Hackage, you'll make
an (asynchronous) request to the Raskell server to query for the latest
ratings. The Raskell user script will also add '++' buttons for users to support
libraries.

## Going Forward

Raskell is really simple at the moment, and I mostly whipped it up to prove that
you could generate useful Haskell applications in a short period of
time. However, I do have plans for it going forwards. Firstly, it'd love to do
more analytics. It shouldn't be hard to add tables of top libraries both overall
and in the last week (month?). It might also be interesting to plot the
aggregate rating of projects overtime, maybe providing sparklines for to embed
on Hackage or as widgets for projects to put on their own websites/GitHub
profiles.

## Why Not Hackage?

An obvious question is why not just add this to Hackage directly? I would love
to do this, but I mostly wanted to do this as a prototype to guage if the
project is even useful. I'm definitely up for working with Hackage maintainers
on integrating this proper, and will certainly give this data over if that does
happen.

Also, I'm personally a little unclear about the future of Hackage. I'm aware of
Hackage 2, but I don't know how active that is, let alone how to build it. This
is plain ignorance I'll admit, but hopefully people can appreciate the prototype
nature of this project!

## Be a Raskell!

To use Raskell, head over to the homepage at
[http://raskell.ocharles.org.uk](http://raskell.ocharles.org.uk). The home page
is currently a bit (!) sparse, but I'll grow that when I get a bit more free
time.

The code is all over at [ocharles/Raskell](http://github.com/ocharles/Raskell)
on GitHub, and the bug tracker is
[also there](http://github.com/ocharles/Raskell/issues).

I hope people find this useful!
