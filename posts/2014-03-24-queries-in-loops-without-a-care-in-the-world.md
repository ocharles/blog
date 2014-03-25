---
title: Queries in Loops Without a Care in the World
---

If you've interacted with databases when programming, chances are you've heard
about the "N+1 Selects Problem",
[discussed in detail](http://use-the-index-luke.com/sql/join/nested-loops-join-n1-problem)
at [Use the Index, Luke!](http://use-the-index-luke.com/). Roughly speaking,
this problem is characterised by performing `SELECT` statements repeatedly
inside a loop. For example,

```haskell
do userIds <- getAllUsers
   users <- for userIds $ \userId -> do
     getUserById userId
   doSomething users
```

With the above approach, we do one query to find all user ids, and then we do
another query *for each user id* to get user information. The problem is one of
efficiency, as it reduces performance in two areas. Firstly, this increases
network traffic and incurs more blocking, and it also reduces your ability to
actually take advantage of your database to its full potential. The correct
solution is to perform a constant amount of queries. Ideally a single query,
though even two queries will perform and scale better.

However, I would argue that it's easier to write code in the above form. I don't
want to have to think about collapsing data into single queries, nor do I want
to worry about how to re-associate all that data. To take a more realistic
example, lets imagine we are building a system where we have entities that have
an owner and a type. If we want to get a list of all entities with their owner
and type properties fully expanded, then we have two possibilities. The first is
to throw good practice out of the window, and write the obvious solution:

```haskell
do entities <- getAllEntities
   expandedEntities <- for entities $ \entity -> do
     entityType <- getEntityTypeById (entityTypeId entity)
     entityOwner <- getEntityOwnerById (entityOwnerId entity)
     return $ ExpandedEntity entity entityType entityOwner

   doSomething expandedEntities
```

This suffers from the "N+1 Selects Problem", so we should instead collect all
our data to perform a single query:

```haskell
do entities <- getAllEntities
   let entityTypeIds = map entityTypeId entities
       entityOwnerIds = map entityOwnerId entities
   entityTypes <- getEntityTypesById entityTypeIds
   entityOwners <- getEntityOwnersById entityOwnerIds
   doSomething $ flip map entities $ \entity ->
    ExpandedEntity entity
                   (entityTypeId entity `lookup` entityTypes)
                   (entityOwnerId entity `lookup` entityOwners)
```

Oof. That got a lot more complicated!

Is it possible to write our code in the first way, but have it optimise away to
only issue a constant amount of queries? With a little trickery it turns out -
yes, we can.

## Tell Me What You Want

Before we dive into the code, lets consider how we might reach the solution I'm
going to demonstrate. There are two "phases" in our code, and we want to keep
the two as separate as possible. In the first phase, we determine a set of keys
that should be looked up in the database. I'll call this the *key collection*
phase. After we've got an idea of all these keys, we move onto the next phase,
which is the *querying* phase. In this phase, we perform a query with all the
keys we collected earlier, and then we route the results back to the places
where the query was originally performed.

The type of keys and the data underlying these keys is determined by the query
we are performing. A query for entity types by their id is a query from
`EntityTypeId` to `EntityType`, whereas a query for entity owners would be from
`EntityOwnerId` to `EntityOwner`. This suggests a data type parameterized on
these two types:

```haskell
data Query k v
```

Now, the code performing a query needs to somehow "communicate" with this query,
registering both the key that is being queried for, and allowing information to
flow back in order to use the query results. We can model this by using some
imperative programming constructs (you don't hear that often!). We'll use an
`IORef` to keep track of a mapping keys to query with, to `MVar`s which store
the result of the query (noting that the query might fail). Thus `Query` now
becomes:

```haskell
data Query k v = Query (IORef (Map k [MVar (Maybe v)]))
```

Now we can perform queries by adding a key to the `Map` inside the `Query` and
providing an `MVar` for the result to be placed in:

```haskell
(Query keys) @? k = do
  result <- newEmptyMVar
  modifyIORef' keys (Map.insertWith (++) k [result])
```

This doesn't let us use the query result though, so we'll also return an
`IO` computation to read from the `MVar`:

```
(Query keys) @? k = do
  result <- newEmptyMVar
  modifyIORef' keys (Map.insertWith (++) k [result])
  return (takeMVar result)
```

This is preferred over returning the `MVar` itself, as we'll see later that we
can guarantee that the `MVar` is never read for before it is written from (which
would hang the whole program indefinitely).

Now that we have some basic building blocks, we need a way to structure this in
a safe way. Currently, if you issue a query and immediately ask for a result the
whole computation will block. We need to guarantee that query results can only
be asked for *after* performing the query that will populate the `MVar`s. Lets
see if we can find a way to both compose these queries in a non-blocking
fashion.

## Composing

When thinking of a way to compose things, we should first see if there is a well
established form of composition that we could use. Monads are used extensively
for modelling effects, but they don't give us the ability to statically analyse
the computation before executing it, as the computation can choose what to do
next based on what has already happened. For example, consider the following:

```haskell
do entity <- getEntityById 1
   if entityOwnerId entity `mod` 2 == 0
     then do owner <- getEntityOwnerById (entityOwnerId entity)
             return (entity, Just owner)
     else return (entity, Nothing)
```

If we only have this to consider, then there's no way of determining which
entity owners to select without running some of the computation. The choice of
selecting entity owners *depends on the result* of the initial call to
`getEntityById`.

However, if we choose a less powerful, though still effectful, structure -
namely the applicative functor - then we *do* get the ability to statically
analyse the computation, as we have given up the ability to make branches as
above.

So maybe we'll have luck with the applicative functor, but how do we form it? I
like to construct applicative functors somewhat mechanically, so we'll start by
remembering that there were two phases to this computation - the key collection
phase and the query phase. Both of these phases need `IO` - the former to modify
an `IORef` and the latter to take from the result `MVar`. Applicative functors
have the nice property of being closed under composition, and as all monads are
applicative functors, we can compose two `IO` actions into one applicative
functor:

```haskell
newtype Querying a = Querying { runQuerying :: Compose IO IO a }
```

If you've never seen `Compose` before, allow me to repeat it's definition here:

```haskell
newtype Compose f g a = Compose { getCompose :: f (g a) }

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure x = Compose (pure (pure x))
    Compose f <*> Compose x = Compose ((<*>) <$> f <*> x)
```

The `Applicative` instance for `Compose` shows that `pure` is lifted through
both layers, and `<*>` applies effects at the outer- and the inner-functors (f
and g, respectively).

It turns out the `(@?)` operator we introduced above already fits the form
above, if we simply wrap it all up with `Querying . Compose`:

```haskell
(@?) :: (Ord k, Eq k) => Query k v -> k -> Querying (Maybe v)
(Query keys) @? k = Querying $ Compose $ do
  result <- newEmptyMVar
  modifyIORef' keys (Map.insertWith (++) k [result])
  return (takeMVar result)
```

## Performing Queries

There are two omissions from this article so far - how do we introduce `Query`
values, and perhaps more importantly - how do we actually run this thing? First
of all, lets look at how to introduce a `Query`. When we introduce a `Query`, we
need to ensure that people use them in a safe manner in order to avoid them
blocking. We can do this by using a `bracket`-like construction. The only way to
introduce a `Query` will be by using `withQuery`, which will provide a `Query`
to a `Querying` computation. It will then inspect its own `IORef`, perform the
query, and populate any related `MVar`s:

```haskell
withQuery :: (Ord k, Eq k) => ([k] -> IO (Map.Map k v)) -> (Query k v -> Querying a) -> Querying a
withQuery runner k = Querying $ Compose $ do
  -- Create a IORef to keep track of requested keys and result MVars
  keysRef <- newIORef Map.empty

  -- Run the first phase of the Querying action
  getResponse <- getCompose $ unQuerying (k (Query keysRef))

  -- Check which keys were requested and perform a query
  keys <- readIORef keysRef
  qResults <- runner (Map.keys keys)

  -- Populate all MVars with results
  flip Map.traverseWithKey keys $ \k mvars ->
    for_ mvars $ \mvar ->
      putMVar mvar (Map.lookup k qResults)

  -- Return the IO action that reads from the MVar
  return getResponse
```

Finally, we just need a way to run escape from `Querying`, which is trivial. The
first phase has already been performed, so we only need to perform the second
phase. This is eloquently expressed by `join`ing the two `IO` actions:

```haskell
runQuerying :: Querying a -> IO a
runQuerying (Querying (Compose io)) = join io
```

## Examples

To wrap up, here's a little example of everything in action:

```
getUserAgesById :: [Int] -> IO (Map.Map Int Int)
getUserAgesById keys = do
  putStrLn $ "Looking up " ++ show keys
  return $ Map.fromList $ [(1, 1), (2, 2)]

example :: IO (Maybe Int)
example = runQuerying $
  withQuery getUserAgesById $ \usersAgeById ->
    liftA2 (+) <$> (usersAgeById @?! 1) <*> (usersAgeById @?! 2)
```

And if we run `example`, we see:

```
> example
Looking up [1,2]
Just 3

```

Voila.

## Addendum (or, IORefs!? MVars?! ARE YOU MAD?)

My first attempts to solve this problem didn't use `MVar`s or `IORef`s, but I've
come to see them as a very useful tool to create a sort of request-response
pattern internally. They allow me to separate the request type and the response
type into something I can easily pass around, and by combining them with a
suitable applicative functor, I think a lot of the details can be kept
sufficiently abstract.

However, I would love to know how this could be done without mutable cells. I've
had a little bit of a play with continuations (getting as far as delimited
continuations and then rocking in a corner), but the problem here seems to be
that the `k` and `v` types would have to be part of the `Querying` applicative
itself. Worse, every time you introduce a new query, you'd have to some how get
more types in.

I think that there may be a solution here that uses existential types, as we
immediately consume the value that we ask for - but again, I couldn't pull that
off.

As a final bit of food for thought, Henrik Nilsson pointed out that this all
feels very circular. The normal tool for circular programming is `fix` and
laziness, and that extends to effectful computations with `mfix`. Again, maybe
there is a way to do it using `mfix`, but I had difficult working out how to
keep everything separated.

If anyone wants to take this idea and see what an even more Haskell-y solution
would look like, I would *love* to see it!

One remaining piece of future work is to see if it's possible to do this without
`withQuery`. Currently there is a bit of duplication - you have to introduce a
binding for every query you're going to perform, which can be tedious -
especially when you're trying to extract common queries. On the flip side, it
seems to make sense, in as far as it matches a functional API: we perform a
query, and then we bind the results to some variable and we pull individual rows
out of this result set.
