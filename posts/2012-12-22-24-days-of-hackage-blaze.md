---
title: 24 Days of Hackage: blaze-html
---

We [recently looked at](/posts/2012-12-19-24-days-of-hackage-snap.html) the Snap
web framework for building web applications in Haskell, but I never got round to
covering templating and generating HTML. In today's post, we'll take a look at a
somewhat different approach to generating HTML that *doesn't* use templates, but
in fact uses Haskell itself.

The [`blaze-html`](http://hackage.haskell.org/package/blaze-html) library,
predominantly written by [Jasper Van der Jeugt](http://jaspervdj.be/) for Google
Summer of Code 2010, is a "blazingly fast HTML combinator library" for
Haskell. This means that it provides a collection of primitives to build up HTML
documents inside Haskell code. Going straight into an example, here's how
documents generally look:

```haskell
      greet :: UserName -> Html
greet userName = H.docTypeHtml $ do
  H.head $
    H.title "Hello!"
  H.body $
    H.h1 "Tervetuloa!"
    H.p ("Hello " >> toHtml userName >> "!")
```

As you can see, the main abstraction is do-notation - `Html` is actually a type
synonym for `MarkupM`, which is a instance of `Monad`. This lets us build up a
HTML document in a familiar hierarchical manner. On top of that, I'm using the
`OverloadedStrings` extension in GHC to automatically convert string literals
into `Html` - typing `"Hello!"` is the same as typing `toHtml "Hello"`, but far
more convenient! The `H.` stuff is there because I usually import
`Text.Blaze.Html5` qualified.

Not only does `blaze-html` look natural and familiar, because it's Haskell code
and doesn't introduce a huge amount of new data types, a lot of the things we
already know immediately carry forward to `blaze-html` too! For example, suppose
we want to insert `<hr />` between paragraphs:

```haskell
      addHr [] = mempty
addHr [p] = p
addHr (p:ps) = p >> H.hr >> addHr ps
```

Now we can easily use our combinator to build up more complicated documents:

```haskell
      doc = H.docTypeHtml $
  H.body $
    addHr [ H.p "Hello, world!"
          , H.p "How are you?"
          ]
```

It's exactly this type of refactoring that we already do in our code day-to-day,
so why not apply it to rendering HTML too?

There are sadly a few drawbacks to `blaze-html` - notably it is not a "true"
monad (it violates the monad laws), nor is it a monad transformer. It would be
fantastic if it *was* a transformer, as we'd then be free to use a `Reader`
monad as our base monad, which might provide a nice abstraction to passing
around common variables in templates (e.g., the currently logged in
user). That's not to say these things are impossible - you can always layer
`Reader` *on top of* `Html`, but it just becomes a tad harder to work with.

Anyway, `blaze-html` remains my go-to choice for templating small web sites,
because I have to learn practical nothing, now that I've got a good grip on
Haskell! If you're focusing on learning Haskell over the holidays, and would
like to see how far you can go without learning other languages, I highly
recommend `blaze-html`. Even if you're not using Haskell, maybe the ability to
refactor your templates just like ordinary code is convincing enough!
