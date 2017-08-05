---
title: "24 Days of Hackage: persistent & esqueleto"
---

Many of the applications we write these days tend to be long running processes
that gradually manipulate state. Your average website is really not much more
than a function that turns requests into responses, where the response varies
depending on various state. The state could be whether you're logged in, blog
entries made within a certain time range, items you've saved to your wishlist,
etc. This state is very important, so just keeping it in memory isn't a great
idea, as that greatly increases the chance of you losing it for good!

For this reason, we have a lot of great technology for persisting state -
PostgreSQL, SQLite, Redis, and so on. Today, we'll have a look at the
[`persistent`](http://hackage.haskell.org/package/persistent) library, which
gives us a unified API into these persistent data stores.

## Schemas and Data with `persistent`

Programming with `persistent` is a slightly different experience than you may
have had with other Haskell libraries, as it provides its own language for
defining schemas. This language isn't built using Haskell combinators, but has
its own syntax and is compiled using a *quasiquoter*. For example, if we wanted
to try and beat Facebook at its own game, here's how we might start:

```haskell
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
  name Text
  deriving Show

StatusUpdate
  producer PersonId
  update Text
  createdAt UTCTime
  mood Text Maybe
  deriving Show
|]
```

The special `[|` and `|]` brackets specify that we're going to use the
`persistLowerCase` quasiquoter, and within this block we see the definition for
our schema. In this case, a `Person` has a name, and a sequence of
`StatusUpdate`s. This schema definition language is simple, and is similar to
defining data types in Haskell (for good reason, as we'll see shortly). We have
the name of the entity (`Person` and `Update`), and each entity has a list of
fields. Each field has a name and a data type. Notice that the `createdAt` field
uses the exactly the same type as provided in `Data.Time`. `persistent` uses
real Haskell types to define schemas, which makes it fantastic for code
reuse. We mark the `mood` attribute of updates as optional by tagging on the
`Maybe` modifier. Finally, each `Entity` has a series of deriving clauses, just
like a Haskell data type.

When we parse this definition and realise it using `mkPersist`, `persistent`
introduces various data types and type class instances for us. `persistent` is
going to create (amongst other things) a series of data types for each entity,
along with Haskell field accessors (e.g., `personName` or
`updateCreatedAt`).

Once we have declared our schema and associated Haskell data types, we're ready
to start using our `persistent` data store. We will need to choose a backend to
run queries against, so for brevity we'll use an in-memory SQLite database,
though you're encouraged to [clone this code](http://github.com/ocharles/blog)
and try it against other backends! As an example, we can create the schema,
insert a person, and attach a single status update:

```haskell
main :: IO ()
main = runSqlite ":memory:" $ do
  runMigration migrateAll
  ollie <- insert (Person "Oliver Charles")
  now <- liftIO getCurrentTime
  insertMany
    [ StatusUpdate ollie "Writing another blog post!" now Nothing,
    , StatusUpdate ollie "I <3 24 Days of Hackage" now (Just "^.^")
    ]
  return ()
```

Very simple - and we didn't even have to write a line of SQL! Not only did we
avoid writing SQL, we can actually use this exact same schema against MongoDB,
or Redis, or any other data store with a `persistent` backend.

Walking through this code, once we've connected to our in memory SQLite
database, we need to make sure it has the correct relational schema - which we
can do with `runMigration migrateAll`. When we introduced the schema earlier, we
asked for a `migrateAll` action to be created as well. When we call this action,
`persistent` makes sure that our data store has the schema we are expecting -
adding tables and columns as necessary.

Next, we create Haskell values (with `Person` and `StatusUpdate`) and insert
them into our database with `insert` or `insertMany`. At this point,
`persistent` is going to mint these entities with new identifiers, so we can
refer to them in the future.

## Querying with `esqueleto`

We've seen how to put data into a data store, but what about getting it out?
While `persistent` does provide a query API, it can be quite
limited. [`esqueleto`](http://hackage.haskell.org/package/esqueleto) builds on
top of `persistent` by providing a richer SQL-like query syntax. For example, we
can query for 5 people in our database sorted by name:

```haskell
sortedNames =
  select $
  from $ \person -> do
  orderBy [asc (person ^. PersonName)]
  limit 5
  return $ person ^. PersonName
```

Tacking `sortedNames >>= mapM_ print` onto the end of `main` above produces the
following output:

```
Value "Oliver Charles"
```

As we would expect.

`esqueleto` is a Haskell DSL that works with all of the things that `persistent`
defined for us. In this case, we can see that we are using the `PersonName`
field on the `person` variable to define our ordering. This lets Haskell know
that `person` must be a `Person` record, so `esqueleto` knows how to pull the
data out we requested we used `from`. In this sense, we really benefit from type
inference - by operating on the types, we also inform, `esqueleto` as to which
tables to pull data from.

We don't have to limited to just one entity though, we could just as easily find
the latest 5 status updates from all people on the system:

```haskell
latestUpdates =
  select $
  from $ \(person `InnerJoin` update) -> do
  on (person ^. PersonId ==. update ^. StatusUpdateProducer)
  orderBy [asc (update ^. StatusUpdateCreatedAt)]
  limit 5
  return (person ^. PersonName, update ^. StatusUpdateUpdate)
```

In this query we specify that we're going to be performing an *inner join* on
`Person` and `StatusUpdate`, and we tell `esqueleto` which predicate to use in
order to perform this join. We then have both the `Person` and `StatusUpdate` in
scope, so we can sort by the timestamp of the update. We then return a pair of
both the name of the person who created the status update, and the contents of
the status update itself.

As we saw yesterday with
[`scotty`](/posts/2013-12-05-24-days-of-hackage-scotty.html), Haskell can really
shine when it comes to prototyping small applications, and I think `persistent`
follows in this tradition. It's a little opionated, with the `mkPersist` syntax
and automatic creation of surrogate keys, which can make fitting `persistent`
onto existing schemas a challeng. However, if you don't have that requirement,
`persistent` might be one of the easiest routes to getting data into a
`persistent` store with the minimal amount of code duplication. You don't have
to worry about defining isomorphisms between rows and Haskell values - you can
just let `persistent` do that for you, while you carry on doing the interesting
work.
