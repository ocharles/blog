---
title: The list of monoids pattern
---

Hello! Yes, this blog is still alive. In this post, I want to share a small little pattern that I've found to have a surprisingly high quality-of-life improvement, and I call it the _list of monoids_ pattern.

The idea is that whenever we have a monoidal value - a type that is an instance of the `Monoid` type class - we can sometimes produce a more ergonomic API if we change our functions to instead to a list of these monoidal values.

I recently [proposed an instance of this pattern to `lucid`](https://github.com/chrisdone/lucid/issues/127), and it was well received and ultimately merged as part of the new [`lucid2`](https://hackage.haskell.org/package/lucid2-0.0.20220526) package. To motivate this post, I'm going to start by reiterating my proposal.

`lucid` is a domain specific language for producing HTML documents. In `lucid` we have the [`Attribute`](https://hackage.haskell.org/package/lucid-2.11.1/docs/Lucid-Base.html#t:Attribute) type which represents a single key-value pairing. When we construct HTML elements, we supply a `[Attribute]` list. For example,

```haskell
div_ :: [Attribute] -> Html a -> Html
```

(Note that `lucid` has an overly mechanism, this is one possible type of `div_`).

The problem with this API is that it makes it difficult to abstract groups of attributes and reuse them.

## Example 1

My motivation came from using the fantastic [HTMX](https://htmx.org/) library, and wanting to group a common set of attributes that are needed whenever you connect an element with an end-point that serves [server-sent events](https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events). More specifically, I wanted to "tail" an event stream, automatically scrolling the latest element of the stream into view (using [Alpine.js](https://alpinejs.dev/)). An example of the attributes are:

```html
<div
   hx-sse="connect:/stream"
   hx-swap="beforeend"
   x-on:sse-message.camel="$el.scrollIntoView(false);">
```

In `lucid`, we can express this as:

```html
div_
  [ makeAttribute "hx-sse" "connect:/stream"
  , makeAttribute "hx-swap" "beforeend"
  , makeAttribute "x-on:sse-message.camel" "$el.scrollIntoView(false);"
  ]
```

This is great, but my problem is wanting to re-use these attributes. If I have another page that I also want to have a stream in, I could copy these attributes, but the programmer in me is unhappy with that. Instead, I want to try and share this definition.

One option is:

```haskell
tailSSE url =
  [ makeAttribute "hx-sse" $ "connect:" <> url
  , makeAttribute "hx-swap" "beforeend"
  , makeAttribute "x-on:sse-message.camel" "$el.scrollIntoView(false);"
  ]
```

But look what happens when I use this:

```haskell
div_
  (concat [ tailSSE "/stream", [ class_ "stream-container" ] ])
```

Urgh! Just using this requires that I call `concat`, and to use more attributes I have to nest them in another list, and then I have to surround the whole thing in parenthesis. Worse, look what happens if we consider this code in the context of more "ordinary" HTML:

```haskell
div_ [class_ "page"] do
  h1_ "Heading"
  div_
    [class_ "scroll"]
    do
      div_
        ( concat
            [ tailSSE "/stream",
              [ class_ "stream-container",
                id_ "stream-1"
              ]
            ]
        )
```

Our SSE attributes stand out like a sore thumb, ruining the nice DSL that `lucid` gives us.

At this point, we need to start thinking about ways to fix this.

Before we get to that, let's look at one more example

## Example 2

Continuing with `lucid`, I'm also a user of [Tailwind](https://tailwindcss.com/) for styling pages. In Tailwind, we combine primitive classes to style our elements. Sometimes, this styling needs to be conditional. When we we layout a list, we might want to emphasize a particular element:

```haskell
ul_ do
  li_ [ class_ "p-4" ] "Item 1"
  li_ [ class_ "p-4 font-bold" ] "Item 2"
  li_ [ class_ "p-4" ] "Item 3"
```

Generally this list will come from another container which we want to enumerate over:

```haskell
ul_ do
  for_ items \item ->
    li_ [ class_ $ if active item then "p-4 font-bold" else "p-4" ]
```

It's unfortunate that we've had to repeat `p-4` here. We could of course factor that out, but what I more generally want to do is define a common attribute for list items, and another attribute that indicates active. Then, for active items I can just conditionally add the "active" element:

```haskell
ul_ do
  for_ items \item ->
    li_
      [ class_ "p-4"
      , if active item then class_ "font-bold" else ???
      ]
      (toHTML (caption item))
```

But what we are we going to put for `???`? There isn't really an "identity" attribute. A common hack is to add `class_ ""`, but that is definitely a hack.

## Solutions

If you see both of these problems, a natural reaction might be to make `Attribute` an instance of `Monoid`. We might change the type of

```haskell
div_ :: Attributes -> Html a -> Html a
```

However, when we do this we momentarily make things a little worse. Starting with the second example 2:

```haskell
ul_ do
  for_ items \item ->
    li_
      ( mconcat
          [ class_ "p-4",
          , if active item then class_ "font-bold" else mempty
          ]
      )
      (toHTML (caption item))
```

Our `???` becomes `mempty` which literally means "no attributes at all". This solves our problem, but the cost is that overall the API has got more verbose.

How about our first example?

```haskell
div_ (class_ "page") do
  h1_ "Heading"
  div_
    (class_ "scroll")
    do
      div_
        ( mconcat
            [ tailSSE "/stream",
            , class_ "stream-container",
            , id_ "stream-1"
            ]
        )
```

The result here is somewhat mixed. Applying a single attribute isn't too bad, but my main objection to this was that it's inconsistent, and here it's even more inconsistent - a single attribute uses parethesis, but multiple attributes need a call to `mconcat`. It is nice though that our `tailSSE` call no longer sticks out, and just looks like any other attribute.

## The list of monoids pattern

With that setup, I can now present my solution - the list of monoids pattern. As the name suggests, the trick is to simply change our `Attributes` argument to now be a `[Attributes]`. This is essentially a list-of-lists of key-value pairs, which is probably not our first instinct when creating this API. However, I think it pays of when we try and lay out HTML using `lucid`:

```haskell
div_ [ class_ "page" ] do
  h1_ "Heading"
  div_
    [ class_ "scroll" ]
    do
      div_
        [ tailSSE "/stream",
        , class_ "stream-container",
        , id_ "stream-1"
        ]
```

We're back to where we started! However, we've also retained a solution to the second example:

```haskell
ul_ do
  for_ items \item ->
    li_
      [ class_ "p-4",
      , if active item then class_ "font-bold" else mempty
      ]
      (toHTML (caption item))
```

## `lucid` could go further

Interestingly, once I had this observation I realised that `lucid` could actually go further. `Html a` is a `Monoid`, but notice that when we construct a `div_` we supply a single `Html a`. This post suggests that an alternative API is instead:

```haskell
div_ :: [Attributes] -> [Html a] -> Html a
```

Users of Elm might be getting a sense of déjà vu, as this is very similar to the type that Elm uses! I like this form because I think it makes HTML documents much more regular:

```haskell
div_
  [ class_ "p-4 font-bold" ]
  [ p_ "Paragraph 1"
  , img_ [ src_ "haskell.gif" ]
  , p_ "More"
  ]
```

Elm falls short of the pattern advocated in this blog post, as both attributes and html elements lack an identity element, so while Elm uses lists, they aren't lists of monoidal values.

## `optparse-applicative`

I want to briefly compare this to the API in `optparse-applicative`. [Gabriella postulated](https://twitter.com/GabriellaG439/status/1539325694991249408) that `optparse-applicative` might be more approachable if it used records instead of its current monoidal based API. While I don't disagree, I want to suggest that the list-of-monoids pattern here might also help.

When we use `optparse-applicative`, we often end up with code like:

```haskell
flag
  True
  False
   ( long "no-extensions"
  <> short 'E'
  <> help "Don't show the possible extensions for physical files" )
```

If `optparse-applicative` instead used a list of monoids, the API would be a little more succinct for users, while not losing any functionality:

```haskell
flag
  True
  False
  [ long "no-extensions"
  , short 'E'
  , help "Don't show the possible extensions for physical files"
  ]
```

Modifiers can be grouped and abstracted as before, and if we want to compute modifiers with an option to produce no modifiers at all, we can still return `mempty`. However users are no longer burdened with needing to combine modifiers using `<>`, and can instead lean on Haskell's special syntax for lists.

## Concluding thoughts

If at this point you're somewhat underwhelmed by this blog post, don't worry! This pattern is extremely simple - there are no complex tricks required, it's just literally wrapping things in a list, moving a call to `mconcat`, and you're done. However, I think the implications are fairly significant, and I highly recommend you give this a try.
