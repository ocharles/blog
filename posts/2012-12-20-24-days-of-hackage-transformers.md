---
title: 24 Days of Hackage: transformers
---

Monads monads monads monads monads monads. There, now this blog is *officially*
a Haskell blog. It's true, you can't do much programming in Haskell without
dealing with monads, but as we'll see - this isn't any reason to be scared of
them. Instead, we should embrace them! `transformers` is built for this.

The [`transformers`](http://hackage.haskell.org/package/transformers) library
provides monad *transformers* which let you combine the behavior of multiple
monads together. The first transformer I used was the `ReaderT` transformer,
which lets you add a fixed environment to a computation. For example, lets say
we have the following functions:

```haskell
      listAllUsers :: Connection -> IO [User]
listAllUsers c = query c "SELECT * FROM users" ()

findUser :: Connection -> UserName -> IO (Maybe User)
findUser c name = listToMaybe <$>
  query c "SELECT * FROM users WHERE name = ?" (Only name)
```

We've got two computations here which both require access to the database, so we
need to pass a `Connection` to every call. If we're having to call these
functions regularly, this quickly becomes a pain. What we'd really like is to
add some sort of "context" to our computation. In imperative languages,
especially those with global variables, this would be easy! Well, with
`ReaderT`, it's just as easy in Haskell. Here's a variant using `ReaderT`:

```haskell
      listAllUsers :: ReaderT Connection IO [User]
listAllUsers = query' "SELECT * FROM users" ()

findUser :: UserName -> ReaderT Connection IO (Maybe User)
findUser name = listToMaybe <$>
  query' "SELECT * FROM users WHERE name = ?" (Only name)

-- With...
query' :: Sql -> Parameters -> ReaderT Connection IO [a]
query' = ...
```

I've introduced my own little operation in the `ReaderT Connection IO` - the
`query'` function simply reads the `Connection` out of the environment and runs
`query` as before. Now we can form computations inside this, and easily leverage
the database connection:

```haskell
      do
  c <- openConnection
  runReaderT c $ do
    users <- listAllUsers
    ...
    user <- findUser "Bob"
```

We have isolated computations that touch the database from those that don't,
while also made computing with the database even simpler, as we don't need to
worry about threading the connection handle throughout all the code.

Adding a fixing environment is not the only thing we can do with
`transformers`. Another handy transformer is the `WriterT` transformer, which
lets us emit some values in a monoid as we run a computation. Logging is the
somewhat obvious example of this:

```haskell
      listAllUsersLogged :: WriterT [String] (ReaderT Connection IO) [User]
listAllUsersLogged = do
  tell ["Listing all users..."]
  users <- lift listAllUsers
  tell ["Found " ++ show (length users) ++ " users"]
  return users
```

In this example, I've reused the `listAllUsers` function from the previous
example and added some logging to it - logging the entry and exit of the
function. As you can see, the base monad can be as complex as you want - we're
not limited to just IO, but we can also use our `ReaderT Connection` monad.

## Combining Functors

One thing I love about `transformers`, which I don't think is often talked
about, is the ability to transform functors - combining them into a larger
functor. It was Gibbons and Oliveira's
[the Essence of the Iterator Pattern](http://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf)
that first introduced me to how powerful this can be. While we can always take
the product of two monads, we can also take the product of two
functors. However, *unlike* monads, the composition of two functors is also a
valid functor! This freedom makes me really warm and fuzzy.

I recently used this to build an applicative functor for doing three-way
merges. A three-way merge combines data from a left side, a right side, and the
original document. I modelled this with a `MergeScope`, and a `Merge`
applicative functor:

```haskell
      data MergeScope a = MergeScope { left :: a
                               , original :: a
                               , right :: a
                               }

newtype Merge e a = Merge (Compose ((->) (MergeScope e)) Maybe a)
  deriving (Functor, Applicative)
```

It looks a little bit scary, but what I've done here is taken two applicative
functors and combined them together. The `Maybe` applicative functor is adds the
ability for something to fail - if part of a merge is impossible, then we can't
merge the entire document, so we should fail. On top of this, I've added used
the reader applicative functor to automatically thread the 3 sides of the
document through computations. What I really like about this is that I didn't
really have to worry about *how* these functors are structured - I simply
reasoned about the sementics they offered, and the semantics I needed, and the
end result naturally fell out.

If you're interested in this sort of stuff, definitely check out the
aforementioned paper and work through it, it's a great read!

`transformers` is another library in the Haskell platform, so again - you
probably already have this. If you're just exploring monads, transformers are
something well worth having a play with, they're a lot of fun!
