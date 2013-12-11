---
title: 24 Days of Hackage: heist
---

[Last year](/posts/2012-12-19-24-days-of-hackage-snap.html) we looked at the
[Snap web framework](http://snapframework.org), focusing specifically on Snap
itself - how to do routing, how to assemble snaplets, and so on - while
overlooking the arguably more important details about how to generate the HTML
in responses. The Snap team are also responsible for a templating engine that
can do this HTML generation, and this library is called
[`heist`](http://hackage.haskell.org/package/heist).

`heist` is a template engine that presents a usage pattern that is likely
familiar to most web developers. Templates are written in separate files outside
the Haskell source code, and then we render these templates using a specific
context, which defines how to "fill in the blanks", so to speak. `heist` breaks
away from the crowd in the finer details of how it achieves this.

`heist` templates themselves are simply HTML documents; they don't have a single
root element (they can have many), but other than that they are valid
HTML5. On top of HTML templates `heist` has three main abstractions: `<bind>`,
`<apply>` and splices.

`<bind>` is very straight forward to understand - it just acts like a `let`
binding in Haskell. However, instead of introducing a new identifier as we would
expect in Haskell, `<bind>` introduces a new HTML *element*. For example, we can
bind some text to an element:

```xml
<bind tag="kiddo">Billy</bind>

Merry Christmas, <kiddo/>!
<!-- Merry Christmas, Billy! -->
```

Or we could bind a more complex HTML tree:

```xml
<bind tag="kiddo"><em>Billy</em></bind>

Merry Christmas, <kiddo />!
<!-- Merry Christmas, <em>Billy</em>! -->
```

A binding can also be used in element attributes, like so:

```xml
<bind tag="kiddo">Dorothy</bind>

That's a <a href="/list/${kiddo}">big wish list</a>, <kiddo />!
<!-- That's a <a href="/list/Dorothy">big wish list</a>, Dorothy! -->
```

Already, we're seeing the ability to create abstractions in our templates, and
we have the added bonus that we didn't have to learn a new syntax - we got to
reuse our knowledge of HTML, and only had to learn a few new semantics.

The next part of `heist` is the `<apply>` tag, which can be used to insert the
contents of one template into another template. Furthermore, this wrapper
template can refer to bindings that the caller defines. For example, Santa might
have a standard letter template, which can be sent to any `<kiddo />`:

```xml
<!-- letter.tpl -->
Dear <kiddo />,

<apply-content />

Yours,
Santa

---
The following letter was sent on behalf of Santa. The contents of this letter
represent potential thoughts of Santa and must be interpreted as fiction. Santa
is a entity of Christmas & Christmas Ltd (c) 2013.
```

Now we can write letters that use this letter as a template:

```xml
<!-- billy.tpl -->
<bind tag="wanted">Playstation 4</bind>
<bind tag="got">Monopoly board game</bind>

<apply template="letter">
  <bind tag="kiddo">Billy</bind>
  I regret to inform you the "<wanted />" you have requested is currently
  unavailable. I have substituted this with "<got />". I hope this does not
  disappoint you.
</apply>
```

The special `<apply-content>` tag in the letter template is bound with the
contents of the call to `<apply>`. As you can see, we can also introduce
bindings inside `<apply>` - the `kiddo` binding - which propagate down into the
template we are including.

While we've introduced some abstraction, at Christmas & Christmas Ltd. we'd
really like to go further, and automate the whole process of sending letters
according to our database. We already have some Haskell code to interface with
the database, so what we'd like to do is fill in those bindings from
code. Before we get to there, lets have a look at how we call a template from
Haskell:

```haskell
billy :: IO ()
billy = eitherT (putStrLn . unlines) return $ do
  heist <- initHeist mempty
    { hcTemplateLocations = [ loadTemplates "templates" ]
    , hcInterpretedSplices = defaultInterpretedSplices
    } 

  Just (output, _) <- renderTemplate heist "billy" 

  liftIO . BS.putStrLn . toByteString $ output
```

First of all we define Heist's configuration, by starting with the default
configuration (available by `mempty`) and augmenting this configuration to load
some templates from the disk. Next, we initialize `heist` with this
configuration using `initHeist`. Finally, we use `renderTemplate` to perform the
actual rendering. `renderTemplate` looks up a template by name in the list of
templates that were loaded when we called `initHeist`, and also takes our `heist`
configuration. 

If we run this with `templates/billy.tpl` existing as defined above you'll see:

```
Dear Billy,

  I regret to inform you the "Playstation 4" you have requested is currently
  unavailable. I have substituted this with "Monopoly board game". I hope this
  does not disappoint you.

Yours,
Santa

---
The following letter was sent on behalf of Santa. The contents of this letter
represent potential thoughts of Santa and must be interpreted as fiction. Santa
is a entity of Christmas & Christmas Ltd (c) 2013.

```

The final concept that we need to understand is that of splices. Splices are a
powerful part of Heist, and in general allow us to bind arbitrary Haskell code
to elements - a lot like the `<bind>` logic we saw earlier. To get started,
we'll look at binding Haskell values as text. Assume we have a `getNames :: IO
[Text]` action. Now we can produce a letter to every name:

```haskell
names :: IO ()
names = eitherT (putStrLn . unlines) return $ do
  heist <- initHeist mempty
    { hcTemplateLocations = [ loadTemplates "templates" ]
    , hcInterpretedSplices = defaultInterpretedSplices
    }

  names <- liftIO getNames

  forM_ names $ \name -> do
    Just (output, _) <- renderTemplate
      (bindSplice "kiddo" (textSplice name) heist)
      "merry-christmas"

    liftIO . BS.putStrLn . toByteString $ output
```

Here we called the `getNames` action, and then enumerated all of the names it
returned and printed a letter to each name. We bound text to splices by using
`textSplice` to create a splice that contains the lucky child's name, and then
gave this splice a name.

However, you're not limited to binding pure values to splices, you can also bind
monadic actions, or even implement more complex control flow.

As another example, Santa might want a daily list of all children who we've sent
greetings to. That is, Santa would like to see a list of all the names that
`getNames` returns. We could perform this IO action and combine all the names
into a single string, but this isn't very flexible and it's certainly not very
reusable. An alternative is to introduce a `<names>` element that runs its
content *for each* name in the list. This is exactly the type of stuff we can do
with custom splices. Here's how it looks:

```haskell
namesSplice =
  liftIO getNames >>=
    mapSplices (\name -> runChildrenWithText ("name" ##  name))
```

We use `liftIO getNames` again to get the list of names, but then we use
`mapSplices` to run sub-bindings *for each* element in the list. In this case,
for each element `getNames` returns, we bind the `<name>` element
appropriately. We have to wire this into the rendering of our template, which is
just another call to `bindSplice` as we've seen before:

```haskell
  Just (output, _) <- renderTemplate
    (bindSplice "names" namesSplice heist)
    "summary"

  liftIO . BS.putStrLn . toByteString $ output
```

If we run this against the following template:

```xml
<apply template="letter">
  <bind tag="kiddo">Santa</bind>

  The following children were greeted:  

  <names>
    * <name />
  </names>
</apply>
```

Then we get the following summary output, just as we'd expect:

```
Dear Santa,

  The following children were greeted:
  
    * Tom
  
    * Dick
  
    * Harry

Yours,
Santa

---
The following letter was sent on behalf of Santa. The contents of this letter
represent potential thoughts of Santa and must be interpreted as fiction. Santa
is a entity of Christmas & Christmas Ltd (c) 2013.
```

## Conclusion

`heist` is a really powerful templating system that is a great fit for teams
with have separate developers, who can't be assumed to know Haskell. `heist`
also doesn't compromise on functionality - we've barely scratched the surface of
what's possible with splices, including recursion and more involved flow
control. What I really like about `heist` though, is the templating language
doesn't try and pretend to be a programming language. Instead, it's up to you to
define what is essential to go into a template, and have your templates reflect
the structure of that data. I think this goes a long way to avoiding the mess
that other templating languages can result in.

[Doug Beardsley](http://softwaresimply.blogspot.co.uk/) has let me know about
some blog posts that will help point people in the right direction, if you want
to learn more about Heist:

* [`digestive-functors-heist`](http://hackage.haskell.org/package/digestive-functors-heist-0.8.3.0)
  uses heist to build highly dynamic forms.

* [Doug's posts on Heist](http://softwaresimply.blogspot.com/search/label/heist)

* Soostone's
  [Charade](http://devblog.soostone.com/posts/2013-04-18-charade.html) project
  shows a good example of idiomatic Heist, and also demonstrates
  [dynamic reloading](http://devblog.soostone.com/posts/2013-06-17-snap-template-reloading.html)
  in Snap.

The code for today's post is available
[on Github](http://github.com/ocharles/blog) as always, so have a play!
