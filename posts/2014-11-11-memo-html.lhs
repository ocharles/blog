-----
title: "Self-Memoizing HTML Rendering via Mutually Recursive Data Types"
-----
 
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE TypeOperators #-}
> import Debug.Trace
> import Data.Monoid
> import Data.HashMap.Strict
> import Data.MemoTrie

This post demonstrates a cute little construct I've been playing with recently,
which allows building HTML trees that use memoization to cache the result of
their rendering function. By storing memoization at each node, we can then
mutate the tree, but subsequent renders only do as little work as necessary.

We start out with the deep embedding of a HTML tree. This is exactly as you
might expect; a constructor for text and a constructor for elements, with their tag name and a list of children.
 
> data HTML' = Text String | Element String [HTML]

However, notice that the list of children is *not* of type `HTML'`, as one might expect. Instead, we bounce over to the next data type:

> data HTML = HTML (HTML' :->: String) HTML'
 
Here we wrap up the `HTML'` values with a trie that tabulates the rendering
function. We're using Conal's excellent
[`MemoTrie`](http://hackage.haskell.org/package/MemoTrie) library. Notice how `HTML'` has values of type `HTML`, but `HTML` itself refers back to `HTML'` - this is an example of mutually recursive data types.
 
What we have so far is a `HTML` type that "knows how to render itself", but also carries the deep-embedding that was used to construct it. Furthermore, each subtree knows how to render itself, so we can modify the tree but only pay to render what we changed. We'll see an example of this shortly.

In order to work with the `MemoTrie` library, we need to define instance of `HasTrie`. As scary as these definitions look, they mostly come from using the basic instances and then following the types.

> instance HasTrie HTML' where
>   data HTML' :->: x = HTML'Trie (String :->: x) ((String, [HTML]) :->: x)
>   trie f = HTML'Trie (trie (f . Text)) (trie (f . uncurry Element))
>   untrie (HTML'Trie f _) (Text t) = untrie f t
>   untrie (HTML'Trie _ g) (Element a c) = untrie g (a, c)
>
> instance HasTrie HTML where
>   newtype HTML :->: x = HTMLTrie (HTML' :->: x)
>   trie f = HTMLTrie (trie (f . HTML (trie render')))
>   untrie (HTMLTrie f) (HTML _ x) = untrie f x

The data types we defined above are private, so we export smart constructors. The application of a smart constructor ensures we pair up the rendering function with the correct data.

> text :: String -> HTML
> text t = embed (Text t)
 
> element :: String -> [HTML] -> HTML
> element el children = embed (Element el children)

> embed :: HTML' -> HTML
> embed = HTML (trie render')

All that is left is to define how to render our deep embedding of a HTML tree. This can be done naively with some trusty pattern matching and recursion. We could, of course, also call out to another library like `xmlhtml`.

> render' :: HTML' -> String
> render' (Text t) = trace "** RENDERING TEXT **" t
> render' (Element el children) =
>   trace "** RENDERING ELEMENT **" $
>   "<" ++ el ++ ">" ++ concatMap render children ++ "</" ++ el ++ ">"

There are a few `trace` calls here so we can see what goes on as we try and evaluate these values. Also, note that `render'` itself is somewhat shallow - it doesn't call itself in the recursive position, but instead it calls `render`:

> render :: HTML -> String
> render (HTML f x) = untrie f x

`render` builds the rendering function from the trie at hand, which gets us back to `render'`. However, this function is also memoized, so only initial calls will enter the function. We export `render` but keep `render'` private.


Examples
--------

Let's start by building a simple document:

> document :: HTML
> document = element "div"
>              [ element "h1" [ text "Hello!" ]
>              , element "p" [ text "Bonjour tout le monde" ]
>              ]

When we try and render this, we'll be forced to do some work:

```
... render document

"** RENDERING ELEMENT **
<div>** RENDERING ELEMENT **
<h1>** RENDERING TEXT **
Hello!</h1>** RENDERING ELEMENT **
<p>** RENDERING TEXT **
Bonjour tout le monde</p></div>"

```

However, if we try and render the same document again (in the same environment), something interesting happens...

```
... render document
"<div><h1>Hello!</h1><p>Bonjour tout le monde</p></div>"

```

No tracing! The lack of any tracing calls indicates that we never actually did any rendering - instead we returned the memoized value we computed prior. This starts to become really useful when we have functions to modify a document. For example, this combinator appends a child element to a document:

> (<+/>) :: HTML -> HTML -> HTML
> html@(HTML f (Text _)) <+/> _ = html
> (HTML f (Element x children)) <+/> y = HTML f (Element x (children ++ [y]))

Taking our original document, we can expand it with another paragraph:

> document' :: HTML
> document' =
>   document <+/> (
>    element "p" [text "That's an approximation of 'Hello, world!' in French"])

Again, we can `render` this document:

```
... render document'
"** RENDERING ELEMENT **
<div><h1>Hello!</h1><p>Bonjour tout le monde</p>** RENDERING ELEMENT **
<p>** RENDERING TEXT **
That's an approximation of 'Hello, world!' in French</p></div>"
```

This time we do see some tracing, but only on the elements that have actually changed - in this case the new paragraph, and the container `div` itself. A final call to `render document'` does no work and gives us back the rendering:

```
... render document'
"<div>
<h1>Hello!</h1><p>Bonjour tout le monde</p>
<p>That's an approximation of 'Hello, world!' in French</p></div>"
```

(Line breaks added for clarity)

Concluding Thoughts
-------------------

I'm pretty blown away with how elegantly Haskell is able to capture such an idea. This work arose as I'm currently exploring some ideas using GHCJS and `reactive-banana`, so I really do have documents that change over time and need to be fast. While `reactive-banana` has a network that means I already do minimal recomputation, it's easy to still pay too much. `reactive-banana` is based around behaviors, and events can be constructed using `Applicative`, such that if one changes so does the other. However, beyond this basic dependency information, `reactive-banana` has no way of knowing what's "inside" the function.

One thing that I don't yet understand though, is how this plays out with memory usage. For example, if I'm caching from my root element, this presumably means every single view that makes it to the browser stays in memory... forever. That seems pretty bad! One easy fix is to annotate my rendering with explicit "don't cache this" points, but everytime I find a model that requires the programmer to annotate it for performance, my heart sinks.

I'm sure this work has been discovered before me, so if anyone has any thoughts or pointers to related work, I'm all ears.
