---
title: "Datatype Generic Programming"
---

Today I had the pleasure of attending a talk by [Andres
](http://www.andres-loeh.de/) entitled "[Datatype-Generic
Programming](http://skillsmatter.com/podcast/home/a-haskell-lecture-with-leading-expert-andres-loh/)",
where Andres discussed how we can almost leverage the power of the
`deriving` construct for our own type classes.

Interestingly, this was a Haskell talk where I initially left thinking
that, while impressive there wasn't particularly a "wow" moment. But
then it hit me on the train back - the wow is really in the fact that
it's all so *simple*! Generic programming, at least with `deriving
Generic` is straightforward, and the applications are immediately
obvious. It really helped that the talk was well paced, and I left
feeling I had a good understanding of the theory - switch over to an
isomorphic type representation with a limited set of constructors,
implement your type class there, and you can then provide wrappers to
use generic solutions.

The
[`generic-deriving`](http://hackage.haskell.org/package/generic-deriving)
also got a mention towards the end of the talk, and it turns out this
is a library I've really been wanting. I was aware that it was
possible to write generic implementations of, for example, `Monoid`
for a while - but usually the overhead of writing a generic version
first was greater than just writing a single `Monoid` instance. I was
happy to find out that people have already done this hard work, so now
I just need to use the default implementations.

For example:

```haskell
data Factory = Factory { trinkets :: Set Trinket
                       , widgets :: Set Widget
                       }
  deriving (Generic)

instance Monoid Factory where
  mappend = mappenddefault
  mempty = memptydefault
```

Does exactly what I want. Splendid.