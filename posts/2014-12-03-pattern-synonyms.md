---
title: "24 Days of GHC Extensions: Pattern Synonyms"
---

Continuing yesterday's theme of *binding* extensions, as I like to call them,
today we're going to look at pattern
synonyms. [Previously](/posts/2014-12-02-view-patterns.html), we saw how view
patterns allow us to view data through the result of function application. This
allowed us to keep some of the definition of the data type abstract, while
presenting an easy-to-use interface to API users. However, we noted that there
was a syntactic cost - view patterns require the user to learn new syntax.

Today, we'll look at a brand new extension to GHC -
[pattern synonyms](https://downloads.haskell.org/~ghc/7.8.3/docs/html/users_guide/syntax-extns.html#pattern-synonyms). Pattern
synonyms were introduced in GHC 7.8, and they allow us to give names to pattern
matches. This allows us to keep code maintainable, introduce new abstractions,
and even pattern match values as if they were ordinary data definitions. This
sounds somewhat magical, so lets dive right in and look at examples.

## Pattern Synonyms As Constants

Perhaps the most basic use of pattern synonyms is as a tool to replace magic
constants in code. When working with foreign code, for example from C libraries,
enumerations are often loosely typed as an integer. As an example, let's look at
the [SDL library](http://libsdl.org). In this library, there are C routines such
as

```c
int SDL_SetRenderDrawBlendMode(SDL_Renderer* renderer,
                               SDL_BlendMode blendMode)
```


Looking at the documentation, we see that `SDL_BlendMode` is just an enum - so
what we're actually passing around at runtime is a number - a `CInt`. This is a
little clunky, and it's idiomatic Haskell to move constants into a ADT:

```haskell
data BlendMode = NoBlending | AlphaBlending | AdditiveBlending | ColourModulatedBlending

toBlendMode :: BlendMode -> CInt
toBlendMode NoBlending = #{const SDL_BLENDMODE_NONE}
toBlendMode AlphaBlending = #{const SDL_BLENDMODE_BLEND}
toBlendMode ...

fromBlendMode :: CInt -> Maybe BlendMode
fromBlendMode 0 = Just NoBlending
fromBlendMode ...
```

(Note that the `#{const ...}` syntax comes from
[hsc2hs](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/hsc2hs.html)).

However, this abstraction comes with a cost - we have to do an actual runtime
conversion between the two representations. It's unfortunate that we have to pay
this cost just to write idiomatic code.

Fortunately, this cost goes away entirely when we use pattern synonyms:

```haskell
pattern NoBlending = #{const SDL_BLENDMODE_NONE} :: CInt
pattern AlphaBlending = #{const SDL_BLENDMODE_BLEND} :: CInt
pattern ...
```

Here we see the definition of some new pattern synonyms. This indicates to GHC
that any time you see a pattern match for `NoBlending`, we're actually expecting
a number, and that number should be equal to the `SDL_BLENDMODE_NONE`
constant. This would allow us to write a function such as:

```haskell
setUpBlendMode :: CInt -> IO ()
setUpBlendMode AlphaBlending = do
  putStrLn "Enabling Alpha Blending"
  activateAlphaBlendingForAllTextures
  activateRenderAlphaBlending
```

Here, we pattern match on a `CInt`, but we use a pattern synonym to give the
constant a much more readable name. Astute readers might be a little concerned
at this point - we still have to pattern match against values that don't
semantically make sense, because we are still pattern matching against a
`CInt`. However, pattern synonyms play well with the rest of Haskell, so we can
use a `newtype` to introduce more safety:

```haskell
newtype BlendMode = MkBlendMode { unBlendMode :: CInt }

pattern NoBlending = MkBlendMode #{const SDL_BLENDMODE_NONE}
pattern AlphaBlending = MkBlendMode #{const SDL_BLENDMODE_BLEND}
```

Now we can hide the `MkBlendMode` constructor in our module, and export only the
pattern synonyms. Thus we get all the benefits of an ADT, but without the
runtime overhead! Very cool.

## Bidirectional Patterns

So far, we've looked at how pattern synonyms can be used to help pattern
matching. However, we can do more than this - bidirectional pattern synonyms
also allow us to *create* data. For example, using the previous example, we
already have the ability to send the correct integers to the SDL c library:

```haskell
setRenderAlphaBlending :: Renderer -> IO ()
setRenderAlphaBlending r =
  sdlSetRenderDrawBlendMode r (unBlendMode AlphaBlending)
```

Here we use the `AlphaBlending` pattern synonym - a bidirectional pattern - to
*construct* a value of type `BlendMode` - the `newtype` we defined earlier. We
use `unBlendMode` to coerce the newtype back to the underlying `CInt` and hand
this off to SDL. Very cool!

This type of idea can be taken
[a lot further](https://mpickering.github.io/posts/2014-11-27-pain-free.html) -
in the linked blog post, Matthew Pickering shows us how we can use some seriously
cutting edge features of pattern synonyms (not even released yet!) to work with
so called "unfixed" versions of data types.

icelandj embraces pattern synonyms to almost an absurd level in this
[FPComplete article](https://www.fpcomplete.com/user/icelandj/Pattern%20synonyms)
to build an IRC bot - using patterns in a way that is very different to what you
may be used to!

Personally, I'm only just getting started with pattern synonyms myself - so I'm
in the middle of a learning process to determine when they are useful. It seems
when we are working with very generic data (as in Matt's blog post) pattern
synonyms really shine, but I find the patterns-as-constants usage a really nice
trick too (first
demonstrated to me by Edward Kmett in
[`gl`](http://hackage.haskell.org/package/gl) and later in
[`sdl2`](http://hackage.haskell.org/package/sdl2)).

I see pattern synonyms as a tool to compliment view patterns. If you're only
using view patterns as a way to tidy up bindings, then there's a good chance
that pattern synonyms will be a better fit. However, view patterns have the
advantage of being able to perform actual computations - something that (to the
best of my knowlege) pattern synonyms cannot do.

Have a play with them - see what you find! Code accompanying this blog post can
be found
[on Github](https://github.com/ocharles/blog/blob/master/code/2014-12-03-pattern-synonyms.hs).

----

*This post is part of
[24 Days of GHC Extensions](/pages/2014-12-01-24-days-of-ghc-extensions.html) -
for more posts like this, check out the
[calendar](/pages/2014-12-01-24-days-of-ghc-extensions.html)*.
