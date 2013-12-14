---
title: 24 Days of Hackage: acid-state
---

[Earlier this month](/posts/2013-12-06-24-days-of-hackage-persistent-esqueleto.html)
we looked at one way to add persistent state via the `persistent` library. This
approach made use of existing technology and bridged the gap between Haskell
types and persistent storage. Today, we're going to look at an even more
Haskell-orientated approach to persistent storage and explore David
Himmelstrup's [`acid-state`](http://acid-state.seize.it/) library.

`acid-state` is a library that takes existing Haskell values and adds the
ability to persist them so that state can be re-used across application
invocations. However, `acid-state` is more than just a serialization method --
as the name suggests, using `acid-state` you get the full set of
[ACID](http://en.wikipedia.org/wiki/ACID) properties, which helps rule out a big
class of errors.

For today's example, I want to revisit an application where I used
`acid-state`. At work we were using RabbitMQ as a standard worker queue to
distribute work. If a job failed, a message would be added to a *failures*
queue, with the intention that this would later be inspected by a human. Having
people pull stuff out of RabbitMQ is messy, so I built a little web application
to pull this queue out into a more queryable format. To make sure we didn't
actually lose any messages I needed persistent state, and I didn't want to fuss
around with external technology, so I decided to use `acid-state` as my storage
layer.

The first thing you need to do when using `acid-state` is to define your entire
database state. For this example, we need a way to uniquely identify failure
messages, and we need a way to describe failures. An `IntMap` seems an
appropriate structure for the former requirement, and we'll write a new data
type to describe each individual message. This gives us something like the
following:

```haskell
data Failure = Failure { failureReason :: String
                       , failureTime :: UTCTime
                       } deriving (Show, Typeable)

data FailureDb = FailureDb { allFailures :: IntMap Failure }
  deriving (Typeable)
```

The next step is to define some Haskell functions that operate on this
database. At the very least, we'll want the ability to query for all failures in
the database in time order, and also to add new failures into the
database. These functions are just Haskell functions that operate in the `Query`
and `Update` monads, respectively. The `Query` monad is read-only, and is a lot
like a `Reader` monad, while `Update` is a state monad. Starting with the query,
we have:

```haskell
failuresOverTime :: Query FailureDb [Failure]
failuresOverTime =
  sortBy (comparing failureTime) . IntMap.elems . allFailures <$> ask
```

We use the `ask` operation from the `MonadReader` type class (part of the `mtl`
library) to query for the entire database, and then we `fmap` a pure
transformation over this to convert our `IntMap` to a sorted list.

To add new failures to the database, we work in the `Update` monad by modifying
the underlying state:

```haskell
addFailure :: Failure -> Update FailureDb ()
addFailure failure = modify go
 where
  go (FailureDb db) = FailureDb $
    case IntMap.maxViewWithKey db of
      Just ((max, _), _) -> IntMap.insert (max + 1) failure db
      Nothing            -> IntMap.singleton 1 failure
```

A little more involved, but still nothing out of the ordinary. We use `modify`
from `MonadState` (again, from the `mtl` library) and modify our `FailureDb`
`IntMap` accordingly. If it's empty we use a singleton `IntMap`, otherwise we
add a new `Failure` with a increased key.

We still haven't seen any `acid-state` specific code yet, other than operating
in the `acid-state` monads. The only `acid-state` specific work we need to do is
*promote* the combination of our `FailureDb` type and these functions into
 `acid-state` queries. We can easily do this with a few lines of template
 Haskell:

```haskell
deriveSafeCopy 0 'base ''Failure
deriveSafeCopy 0 'base ''FailureDb
makeAcidic ''FailureDb ['failuresOverTime, 'addFailure]
```

(Don't worry about the `deriveSafeCopy` stuff, though we will touch on that at
the end of this article).

Now we're ready to start persisting some data! We need to do a little initial
configuration to get things going, and we have a few options on where the data
gets persisted. We can use an in-memory representation which is useful for
testing, a local file, or even a
[remote](http://hackage.haskell.org/package/acid-state-0.12.1/docs/Data-Acid-Remote.html)
`acid-state` server. We'll do as little as possible to solve the problem, and
use local storage. Below is an example of how we could query this database:

```haskell
main :: IO ()
main = do
  state <- openLocalState (FailureDb IntMap.empty)

  -- Record a new failure
  now <- getCurrentTime
  update state (AddFailure $ Failure "ENOMISSLES" now)

  -- Query for all failures
  allFailures <- query state FailuresOverTime
  
  mapM_ print allFailures
```

Now we can run this multiple times, and observe the changing output:

```
$ for i in `seq 1 10`
do echo "Run $i:"; ./2013-12-14-acid-state ; echo ""; sleep 5;
done

Run 1:
Failure {failureReason = "ENOMISSLES", failureTime = 2013-12-14 17:42:07.82243 UTC}

Run 2:
Failure {failureReason = "ENOMISSLES", failureTime = 2013-12-14 17:42:07.82243 UTC}
Failure {failureReason = "ENOMISSLES", failureTime = 2013-12-14 17:42:12.83426 UTC}

Run 3:
Failure {failureReason = "ENOMISSLES", failureTime = 2013-12-14 17:42:07.82243 UTC}
Failure {failureReason = "ENOMISSLES", failureTime = 2013-12-14 17:42:12.83426 UTC}
Failure {failureReason = "ENOMISSLES", failureTime = 2013-12-14 17:42:17.846783 UTC}
```

We can cleary see that each successive run shows the previous modifications, and
appends its own modification.

## Conclusion

If you
[have a look](https://github.com/ocharles/blog/blob/master/code/2013-12-14-acid-state.hs)
at today's code, you can see the majority of the code we had to write was
*interesting* and *essential* code to our application. We had to define our
state and some appropriate functions operating on that state, and then we
handed it all over to `acid-state` to do the heavy lifting. This is perfect when
it comes to prototyping -- we don't have to constrain ourselves to restrictions
imposed by a certain query language or database model, we can just dive straight
in to solving problems.

`acid-state` need not be restricted to only modeling prototypes. With
`safecopy`, `acid-state` is able to keep up with changing data types
transparently. `safecopy` lets you specify migrations between versions of data
types, and it will automatically deal with migrating data as it needs to.

If you want to learn more about `acid-state`, check out the
[`acid-state` wiki](http://acid-state.seize.it/), and also some other
[examples](http://mirror.seize.it/acid-state/examples/).
