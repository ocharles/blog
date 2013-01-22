---
title: 24 Days of Hackage: digestive-functors
---

Form processing is a task I used to dread doing in other programming languages,
but not any more! Just about all non-trivial websites have some form of user
interaction, and it can be notoriously hard to sanitize all of the data that
users submit into something that you can be sure is correct. Not only are there
horrible edge cases to deal with, you also need to render the form and present
any validation errors back to users. And to complicate things even more, you
usually want to validate as much of the form as possible, returning *all* errors
at once - it wouldn't provide a very good user experience if you only got one
error at a time!

It sounds horrible right? Well luckily, it's mostly a solved problem. In 2008,
Cooper, Lindley, Wadler and Yallop wrote
[the Essence of Form Abstraction](http://groups.inf.ed.ac.uk/links/formlets/)
which introduced 'formlets', with a paper I recommend giving at least a light
read at some point to understand how the design came about. Originally there was
a fairly literal translation to haskell in the
[`formlets`](http://hackage.haskell.org/package/formlets) library, and this has
now been superceded by [Jasper Van der Jeugt](http://jaspervdj.be/)'s library:
[`digestive-functors`](http://hackage.haskell.org/package/digestive-functors).

Essentially, `digestive-functors` gives you an `Applicative` instance to
construct a data type by evaluating a form. If you're not familiar with
applicative functors, a layman explanation might be that they allow you to apply
arguments to a function, where the arguments themselves come from some sort of
side effect - in this case the form evaluation. Let's have a look at an example:

```haskell
      data Category = Category { catName :: Text }

data BlogPost = BlogPost { postTitle :: Text
                         , postBody :: Text
                         , postCategory :: Category
                         }

postForm :: Monad m => Form Html m BlogPost
postForm = BlogPost
             <$> "title" .: text Nothing
             <*> "body" .: text Nothing
             <*> (Category <$> "category" .: text Nothing)
```

I've defined 2 data types - one for categories of posts, and one for blog posts
themselves. We use digestive-functors in `postForm` to produce a form that runs
in any monad, uses `Html` for errors, and will eventually return a
`BlogPost`. This is a great start, but it's not really doing much - there's no
validation! We don't want to accept `BlogPosts` which have no title or body, for
example, so lets start by fixing that:

```haskell
      postForm :: Monad m => Form Html m BlogPost
postForm = BlogPost
             <$> "title" .: nonEmptyText
             <*> "body" .: nonEmptyText
             <*> (Category <$> "category" .: nonEmptyText)
  where
    nonEmptyText =
      check "Cannot be empty" (not . Data.Text.null) $ text Nothing
```

The `check` combinator allows us to apply any predicate (that is, `a -> Bool`)
to any form that returns an `a`. In this case, we use the predicate `not
. Data.Text.null` which will return `True` if the `text` form returns a `Text`
value that is not empty. The beauty is that `nonEmptyText` is *itself* still a
`Form` - so we could apply further validation on top of this, if we so wanted!

Validation is often not pure though - we might need to check the database to
make certain checks (for example, checking for duplicates first). Lets assume in
this example we have a `BlogWebsite` monad, which amongst other things has the
operation `lookupCategory :: Text -> BlogWebsite (Maybe Category)`. We can then
use this in our `postForm` to lookup categories, and only allow `BlogPosts` to
be created in a valid category:

```haskell
      postForm :: Form Text BlogWebsite BlogPost
postForm = BlogPost
             <$> "title" .: nonEmptyText
             <*> "body" .: nonEmptyText
             <*> "category" .: category
  where
    nonEmptyText =
      check "Cannot be empty" (not . Data.Text.null) $
        text Nothing
    category =
      let failure = Error "Category does not exist"
      in validateM (maybe failure Success) <$> lookupCategory)
           nonEmptyText
```

We've now built a *monadic* `Form` - `category` - which uses the `nonEmptyText`
`Form` we already wrote, and if the text is not empty, attempts to use
`lookupCategory` to find the category. We then unwrap the `Maybe` by converting
to the `Result` type - turning `Nothing` into an `Error` and `Just` into
`Success`.

Alright, this is looking great! But we haven't actually been able to use
it... Afterall, we need a `BlogPost` not a `Form`! There are 2 parts to being
able to run a `Form` - the `View`, an something that can provide an environment
to lookup form fields in. Lets use
[`digestive-functors-blaze`](http://hackage.haskell.org/package/digestive-functors-blaze)
for the rendering, first:

```haskell
      renderForm :: View Html -> Html
renderForm v = do
  form v "POST" $ do
    H.p $ do
      label "title" v "Post title: "
      inputText "title" v
    H.p $ do
      label "body" v "Post: "
      inputTextArea "body" v
    H.p $ do
      label "category" v "Category: "
      inputText "category" v
    inputSubmit "Submit Post"
```

Fairly straight forward here - I'm using
[`blaze-html`](http://hackage.haskell.org/package/blaze-html) for most of the
rendering, and using a few functions from `digestive-functors-blaze` to provide
the form fields and labels. There are other rendering libraries if you don't
like `blaze` - including `digestive-functors-heist` for using Heist templates.

Finally, lets put it all together with a Snap action, so we can actually serve
stuff:

```haskell
      blogPostHandler :: Handler BlogWebsite BlogWebsite ()
blogPostHandler = do
  (view, result) <- runForm "blog-post" blogPost
  case result of
    Nothing -> blaze $ renderForm view
    Just blogPost -> do
      with blogSite $ addPost blogPost
      redirect (postUlr blogPost)
```

Voila! So I guess form processing doesn't have to be so hard after all.

## Further Reading

If this post has piqued your interest, I can highly recommend a read through the
[official tutorial](https://github.com/jaspervdj/digestive-functors/blob/master/examples/tutorial.lhs) and the various other [examples](https://github.com/jaspervdj/digestive-functors/tree/master/examples).

It was also mentioned in comments on Reddit that I haven't quite clarified that
`digestive-functors` is framework agnostic - `digestive-functors` also works
with Happstack and JSON. In fact, here's a list of all
[`digestive-functor`-derived modules on Hackage](http://packdeps.haskellers.com/reverse/digestive-functors)
at the time of writing:

- [digestive-functors-aeson](http://hackage.haskell.org/package/digestive-functors-aeson)
  lets you evaluate forms with a JSON document - great for web services.
- [digestive-functors-blaze](http://hackage.haskell.org/package/digestive-functors-blaze)
  lets you render forms using `blaze-html`.
- [digestive-functors-happstack](http://hackage.haskell.org/package/digestive-functors-happstack)
  allows you to easily use `digestive-functors` with the
  [Happstack](http://happstack.com) framework.
- [digestive-functors-heist](http://hackage.haskell.org/package/digestive-functors-heist)
  lets you use the [Hesit](http://snapframework.com/docs/tutorials/heist)
  templating language (from the Snap framework team).
- [digestive-functors-hsp](http://hackage.haskell.org/package/digestive-functors-hsp)
  lets you use [HSP](http://www.happstack.com/docs/happstack-hsp-7.1.0/doc/html/happstack-hsp/index.html)
  as your mark up language, from the Happstack team.
- [digestive-functors-snap](http://hackage.haskell.org/package/digestive-functors-happstack)
  lets you use `digestive-functors` with [Snap](http://snapframework.com) applications.
