---
title: A Batch Querying Applicative Functor Without IORefs
---

Regular readers of my blog will remember that I recently blogged about a
[`Querying` applicative functor](/posts/2014-03-24-queries-in-loops-without-a-care-in-the-world.html),
which is able to "watch" for queries to a data store and batch multiple queries
into one. It's able to do all of this while allowing us to write code in a
declarative manner, using traversals and other techniques we're already familiar
with. To refresh your memory, here's an example what we can write with
`Querying`:

```haskell
withQuery getUsersById $ \usersById ->
withQuery getTagsByPostId $ \tagsByPostId ->
  for blogPosts $ \post ->
    BlogPost <$> pure post
             <*> usersById @? postAuthor post
             <*> tagsByPostId @? postId post
```

In this example, we have a list of `blogPosts` that we want to load more
information for. We loop over each post, and load the author and tags each
post. Despite using a loop, by using `Querying` we only perform two queries, as
`Querying` batches the queries together.

Even though we reached a viable solution, it left me unsatisfied. The solution
relied on mutability (twice!) - once to record which keys were looked up (here
we used an `IORef`), and again to create a suspended computation containing the
result (an initially empty `MVar`). There's no reason we should have to require
mutability, so I've been searching for a pure solution - and I'm happy to say
I've found it.

## One at a Time, Please

To start understanding the problem, let's look at a pure `Querying` applicative
that works with a single query. A lot of people on Reddit immediately
suggested this as an alternative solution to my `IORef` solution, but didn't
realise it only works for a single query. Nonetheless, it's a good starting
point:

```haskell
data Querying k v a = Querying [k] ([(k, v)] -> a)
```

This applicative functor can be thought of as the product of two smaller
applicative functors - the `Const [k]` functor collects a list of keys, and
the `([(k, v)] ->)` functor allows us to create a suspended computation
awaiting results. If we write out the applicative instance by hand, we can see
what it means to combine two `Querying`s together:

```haskell
instance Applicative (Querying k v) where
  pure x = Querying mempty (pure x)
  (Querying ks f) <*> (Querying ks' x) =
    Querying (ks <> ks') (\result -> f result <*> x result)
```

The pure computation asks no questions and thus doesn't care what the results of
the query are. We can combine two `Querying`s together by collecting the keys of
both values, and creating a new computation suspend on the result that combine
the underlying values.

The problem is that this applicative is now parameterized on a single key/value
pair. If we have a `Querying Int String` and a `Querying Int Bool`, the two are
different types and thus we can't use `<*>` to combine them.

What we need to do is be able to store *lists* of key value pairs of potentially
different types. If we could do this, then we can have a list of keys for every
key value pair. Likewise, we want a similar construction for our suspended
computation, one that would depend on the results of all queries. If we ask 3
queries, then we would like a function of three arguments.

## KeyLists

First of all, let's see how we can have a list of keys of different types. Since
GHC learned how to promote data types to the type level, things have become a
lot easier here. First, we'll introduce a useful type of key value pairs - this
will help us stay organised and principled at the type level:

```haskell
data KV k v = KV k v
```

We won't actually be constructing any terms of type `KV`, but we need a
constructor to promote to be a type. Next, we introduce a GADT `KeyList` which
is a list of lists of keys:

```haskell
data KeyList :: [KV * *] -> * where
  Nil :: KeyList '[]
  Cons :: [k] -> KeyList kvs -> KeyList ('KV k v ': kvs)
```

The empty key list contains no information, and by constructing a term with
`Nil`, we learn that there are no keys at all - as we can see by the empty list
in the *type* of `Nil`. However, we also have a `Cons` constructor which takes a
list of keys (of type `k`), and appends this list onto another list of
potentially different types. So here we can see that we combine terms with cons,
and we mirror this structure at the type level - we are consining a new `KV`
*type* in the final type of `Cons`.

To emphasise the list-like nature of this data type, we can make it an instance
of `Monoid`, which will be useful later. However, there are actually infinitely
many monoid instances - one for every possible list of `KV`s. Fortunately,
Haskell's type classes are able to deal with this, by generating `Monoid`
instances out of smaller ones:

```haskell
instance Monoid (KeyList '[]) where
  mempty = Nil
  mappend _ _ = Nil

instance (Monoid (KeyList kvs)) => Monoid (KeyList ('KV k v ': kvs)) where
  mempty = Cons mempty mempty
  mappend (Cons l ls) (Cons r rs) = Cons (l ++ r) (mappend ls rs)
```

These monoid instances are somewhat reminiscent of an inductive proof - our base
case is the empty list, and a monoid instance for a non-empty list relies on the
monoid instance for a one-element-smaller list, which is a lot like making use
of an inductive hypothesis.

## An Answer For Every Question

Now that we know that we can work with lists of keys of different types, we
should next spend some time to work out how we're going to create the suspended
computation for multiple questions. Let's look at a few examples to get a feel
for things. For a single query, we want:

```haskell
[(k, v)] -> a
```

For two queries, we want:

```haskell
[(k1, v1)] -> [(k, v)] -> a
```

Hmm, what about *zero* queries? Well, logically this would just be a term of
type `a` - no function at all. Well hey, this again looks a lot like a list! We
can use a very similar technique to building `KeyList` - but instead of storing
lists, we would store functions. The constant value moves to be part of our base
case:

```haskell
data ResultF :: [KV * *] -> * -> * where
  ResultConst :: a -> ResultF '[] a
  ResultFunc :: ([(k, v)] -> ResultF kvs a) -> ResultF ('KV k v ': kvs) a
```

Because the base case carries a term, we need a way to mention the type of this
term, so `ResultF kvs` actually constitutes a `Functor`:

```haskell
instance Functor (ResultF kvs) where
  fmap f (ResultConst x) = ResultConst (f x)
  fmap f (ResultFunc g) = ResultFunc (fmap f . g)
```

This is a fairly natural definition - we just pattern match and either apply our
function to the constant value, or we `fmap` inside the function, which builds
up a new function.

In fact, `ResultF` actually creates a full applicative functor - a hint that
we're on the right track to building a larger applicative functor out of
it. Applicative functors have the `pure` method as part of their type class,
which is a producer of data. As such, we'll have to make sure we have instances
for any possible type of `ResultF`, as we did with our `KeyList` `Monoid`:

```haskell
instance Applicative (ResultF '[]) where
  pure = ResultConst
  (ResultConst f) <*> (ResultConst x) = ResultConst (f x)

instance Applicative (ResultF kvs) => Applicative (ResultF ('KV k v ': kvs)) where
  pure x = ResultFunc (const (pure x))
  (ResultFunc f) <*> (ResultFunc x) = ResultFunc (\args -> f args <*> x args)
```

These aren't particularly pretty, but fortunately they aren't particularly
surprising either.

Believe it or not, we're now ready to build our `Querying` functor!

```haskell
data Querying :: (* -> *) -> [KV * *] -> * -> * where
  Querying :: KeyList kvs -> Compose m (ResultF kvs) a -> Querying m kvs a

instance Functor m => Functor (Querying m kvs) where
  fmap f (Querying kvs x) = Querying kvs (fmap f x)

instance (Applicative m, Applicative (ResultF kvs), Monoid (KeyList kvs)) =>
           Applicative (Querying m kvs) where
  pure a = Querying mempty (pure a)
  (Querying kvs f) <*> (Querying kvs' x) = Querying (kvs <> kvs') (f <*> x)
```

`Querying` itself is just a combination of a key list and a monadic action
producing a `ResultF` that eventually produces an `a` (I use `Compose` to help
me write the `Applicative` instances). The `Functor` instance leaves the key
list un-changed, and just uses the `Functor` instance for `ResultF` to do the
heavy lifting.

The applicative instance may look a little bit more intricate, but look at the
definitions... They are exactly the same as what we had before! This shouldn't
be surprising, but somehow... I can't help but be surprised :)

## Query Pointers

Now that we have the ability to build up `KeyList` and `ResultF` terms we're
making good progress. I have a loose plan - I want `withQuery` to introduce
another level of scope in our `Querying` applicative. This corresponds to `Cons`
for `KeyList` and adding a new `ResultFunc` layer for `ResultF`. Introducing a
new level of scope will provide the user with a pointer to this scope, which
gives them a way to demand results from the query.

But how do we represent these pointers? For usual lists, we can use natural
numbers, but if we used numbers here we'd throw an awful lot of useful
information away. A better description would be to describe the operations that
someone has to do to the current scope to get to the data they seek. There are
two possibilities: either the data you seek is at the outermost level of scope,
or you have a pointer that is valid under that scope. You can think of this as
"stripping away" one level of scoping and then carrying on a lookup. It may be
easier to understand this by seeing the data type and some examples:

```haskell
data Where :: [KV * *] -> (KV * *) -> * where
  Here :: Where (kv ': tail) kv
  There :: Where kvs kv -> Where ('KV k v ': kvs) kv
```

`Where` carries a list of scopes that it's valid in as its first parameter, and
its second parameter indicates exactly which key-value pair we're pointing
to. Either we're pointing to the outermost scope with `Here`, or we want to
strip off one layer of scope and then carry on.

For example, we could construct the following queries:

```haskell
Here :: '['KV Int Int] ('KV Int Int)
There Here :: '['KV Int String, 'KV Int Int] ('KV Int Int)
There Here :: '['KV String Char, 'KV Int String, 'KV Int Int] ('KV Int String)
```

## Introducing Queries

The `Where` data type will provide us with a way to introduce a pointer to a
query which is useful for `withQuery`. If I tease you with the type of
`withQuery`, it will be easier to see what the plan is:

```haskell
withQuery
  :: ([k] -> m [(k, v)])
  -> (Where ('KV k v ': kvs) ('KV k v) -> Querying m ('KV k v ': kvs) a)
  -> Querying m kvs a
```

`withQuery` will take a query in some monad `m`, and a function to operate in a
larger scope. We can see that we extend an existing `Querying` of `kvs` with a
new level of scope for `KV k v`. The pointer that we pass in only has to
reference the outermost scope, and we know that we can do that with `Here`. When
we call the continuation with Here, we get back a `Querying` that contains some
keys and a `ResultF`. `withQuery` then performs the query, and removes this
outer scope to get us back to where we were:

```haskell
withQuery query k =
  case k Here of
    Querying (keys `Cons` kvs) (Compose m) ->
      Querying kvs $ Compose $ do
        ResultFunc f <- m
        results <- query keys
        return (f results)
```

Once we have a query, how do we actually ask questions? A first approach might
look like this:

```haskell
ask :: (Eq k, Monad m) => Where kvs ('KV k v) -> k -> Querying m kvs (Maybe v)
ask q k = Querying (mkKeyList q k) (Compose $ return (mkResultF q k))

mkKeyList :: Where kvs ('KV k v) -> k -> KeyList kvs
mkKeyList Here k = Cons [k] (mempty)
mkKeyList (There path) k = Cons [] (mkKeyList path k)

mkResultF :: (Eq k) => Where kvs ('KV k v) -> k -> ResultF kvs (Maybe v)
mkResultF Here k = ResultFunc (identityResultF . lookup k)
mkResultF (There path) k = ResultFunc (const (mkResultF path k))
```

This is a reasonable amount of code, so lets walk through it. When we ask a
query for the value under a specific key, we need to do two things. First we
have to record that we are looking up a specific key, and secondly we have to
create a suspended computation to lookup the result. Both of these are built by
looking at the `Where` value for the query, which tells us how the index into
the `KeyList` for keys for this query, and the corresponding position in the
`ResultF` functor to extract the results.

`mkKeyList` and `mkResultF` simply iterate on a `Where` value until they find
`Here`. Once they do, they simply return a `KeyList` or `ResultF` from that
point all the way to the bottom. For `KeyList` that's easy - we use the `Monoid`
instance to create empty cells for all subsequent nodes. For `ResultF` it's a
little more complex - but the idea is we simply ignore any subsequent parameters
and just return our result. Check out the full code listing for a definition of
`identityResultF`.

If we write a way to actually run a `Querying`, then what we have is already
useful:

```haskell
runQuerying :: Monad m => Querying m '[] a -> m a
runQuerying (Querying Nil (Compose m)) = liftM (\(ResultConst a) -> a) m

example :: IO [(Maybe String)]
example =
  runQuerying $
    withQuery getUserNameById $ \userNameById ->
      for [1..10] $ \userId ->
        ask userNameById userId

  where
  getUserNameById ids =
    print ids
    return [(1, "Alice"), (2, "Bob")]
```

```
> example >>= print
```

Cool!

However, things fall apart pretty quickly if we try two queries at once:

```haskell
example :: IO [(Maybe String, Maybe Int)]
example =
  runQuerying $
    withQuery getUserNameById $ \userNameById ->
    withQuery getUserAgeById $ \userAgeById ->
      for [1..10] $ \userId ->
        (,) <$> ask userNameById userId
            <*> ask userAgeById userId
```

```

```

So, what's gone wrong? The problem is that when we call `withQuery` we are given
a path into a *specific* list of key-values. However, `withQuery` modifies this
very list. Thus if we open two queries, then the first query is no longer
valid - because the second query is now operating in a larger environment. What
we need to do is extend our old references to work with larger scopes. How would
we do that?

When we extend the scope of a query, we grow the list of keys and the list of
arguments in `ResultF` by one. Thus to make an existing query valid in this
environment, we need to prepend `There` onto the query. Remember that `There`
simply skips one layer - in this case stripping off a layer of scope and getting
back to where we were.

It's completely mechanical how many times we need to add `There`, and we want to
exploit this and have GHC figure it out for us. This means we need to *infer*
the amount of `There`s to add, and this is a perfect job for type classes. I'm
going to add a new type class, `Somewhere`, which will take a `Where` term in
one environment, and give you an equivalent path in a larger environment.

```haskell
class Somewhere (kvs :: [KV * *]) (kvs' :: [KV * *]) (kv :: (KV * *)) where
  somewhere :: Where kvs kv -> Where kvs' kv
```

All that's left is to write the instances. The trivial case is that you are you
trying to use a query in an environment where the query is already at the
outer-most scope. In this case, the path is simply `Here`:

```haskell
instance Somewhere ('KV k v ': kvs') ('KV k v ': kvs') ('KV k v) where
  somewhere _ = Here
```

Notice how we are referring to the same `k` and `v` at every place in the
context for this type class instance.

The other scenario is that we have an environment that's too big. In this case
we use `There`, and try again with the smaller environment:

```haskell
instance Somewhere kvs kvs' kv => Somewhere kvs ('KV k v ': kvs') kv where
  somewhere = There . somewhere
```

These instances overlap, so we'll need `-XOverlappingInstances`, but it seems to
be safe here.

Armed with this type class, we just need to modify `ask` to call `somewhere`,
which now makes it more polymorphic in the environment type:

```haskell
ask :: (Eq k, Monad m, Somewhere kvs kvs' ('KV k v)) => Where kvs ('KV k v) -> k -> Querying m kvs' (Maybe v)
ask q k = Querying (mkKeyList (somewhere q) k) (Compose $ return (mkResultF (somewhere q) k))
```

Again, notice that the query `kvs` is not necessarily the same as `Querying`'s
`kvs'`. We require that the two environments are compatible though by
`Somewhere` to the context of this function.

With this last change, we're done - our example now compiles:

## Concluding Thoughts

When I first attempted to work on the batching `Querying` applicative functor, I
felt like it could obviously be done with a pure solution. However, after a day
of dead ends, I moved over to the `IORef` solution. However, as this post
shows - the solution is very much possible and doesn't require a particularly
different formulation. What the solution does require is the ability to really
understand how various different techniques (such as GADTs and type class) can
be used, and how to retain information within the type system. The moment you
lose information, problems of this nature become *very* complicated.

While I've provided a (hopefully!) coherent solution above, by no means was it
as simple as I may have made out! Infact, this problem is a perfect case study
of my favourite type of problems - the class of problems that are just *outside*
my current capabilities. I highly encourage everyone to hold on to these
problems - they don't come up often, but the rewards if you follow it all the
way to the solution are substantial. Don't give up, but don't be afraid to ask
questions either.
