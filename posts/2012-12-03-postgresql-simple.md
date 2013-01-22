---
title: 24 Days of Hackage: postgresql-simple
---

[Yesterday](/posts/2012-12-02-digestive-functors.html) we saw
how we can use `digestive-functors` to perform form validation. However, most of
the time we want to persist that form data, often to a database. In todays entry
of 24 Days of Hackage, we'll look at how we can use
[Leon Smith's](http://blog.melding-monads.com/)
[`postgresql-simple`](http://hackage.haskell.org/package/postgresql-simple) to
store data in a [PostgreSQL](http://postgresql.org) database.

`postgresql-simple` describes itself as a "mid-level PostgreSQL client library,
forked from mysql-simple" - and I think this is a great description. It's not an
ORM-style library that will write SQL for you, for that there are tools such as
[persistent](http://hackage.haskell.org/package/persistent). Rather,
`postgresql-simple` gives you a rich API to interact specifically with a
PostgreSQL database - and as we'll see in this post, that lets us do some rather
neat things.

Let's start with the basics first though.

## Helping Santa

Santa's trying to get organized for Christmas 2012, and has decided that he's
fed up with all the problems in his existing Christmas management software, and
is writing some new software to hopefully get a better handle on things. Like
any other great mythical figure, he's chosen to write it in Haskell. Here are
the data types he has to work with:

```haskell
      data Present = Present { presentName :: Text }

data Location = Location { locLat :: Double
                         , locLong :: Double
                         }

data Child = Child { childName :: Text
                   , childLocation :: Location
                   }
```

No surprises there. Santa needs to be able to get lists of children and presents
out of the database though, and in order to map an SQL row to a Haskell
data-type, we can use the `FromRow` class:

```haskell
      instance FromRow Present where
  fromRow = Present <$> field

instance FromRow Child where
  fromRow = Child <$> field <*> liftM2 Location field field
```

The `FromRow` class has only one associated function - `fromRow :: RowParser
a`. `RowParser` is a `Monad`, `Applicative` and `Functor`, which makes it a
breeze to construct data values. In this case we use the `Applicative` and
`Monad` instances to consume fields of a row (from left-to-right), converting
them into the appropriate values. The `field` combinator takes a single field
from a row, and tries to convert it to a data-type. This means that an SQL
`varchar` can be mapped to `text`, but is also expressive enough to guarantee a
field is not null (such as `presentName`).

Armed with our new `FromRow` instances, we can pluck things out of the database:

```haskell
      allChildren :: Connection -> IO [Child]
allChildren c = query_ c "SELECT name, loc_lat, loc_long FROM child"

allPresents :: Connection -> IO [Present]
allPresents c = query_ c "SELECT name FROM present"
```

Simple!

Alongside `FromRow`, there is also a `ToRow` type class. As you'd expect, this
lets us turn Haskell values into SQL rows, for insertion. I'll leave these
instances as an exercise for the reader!

## Santa LIVE

As I said at the start of the article, `postgresql-simple` has a few PostgreSQL
specific features. I don't have time to go over all of them, but one overlooked
feature is the `LISTEN`/`NOTIFY` protocol.

`LISTEN` and `NOTIFY` are two PostgreSQL specific queries which let you
communicate between server and clients by *pushing* to clients, rather than
having clients poll. In this hypothetical example, we'll assume we have a
`notifySanta :: Text -> IO ()` function, which lets Santa know that a new
Present has been added to a Child's wishlist via SMS. We could write a daemon
that polls the database, but using notifications, we can be much elegant, and
performant!

```haskell
      santaNotifier :: Connection -> IO ()
santaNotifier c = listen >> loop
  where
    listen = query c "LISTEN presents"
    loop = forever $
      getNotification c >>= notifySanta . notificationData
```

## There's More!

I've only scrated the surface of `postgresql-simple`  - there's a lot more that
it's capable of. To whet your appetite, there's also support for PostgreSQL
large-objects, sane parameter substitution, joins, rich error exception
handling, flexible transaction support (with isolation level and automatic
retrying for serializable transactions), and a left fold for incrementally
streaming results.

If only delivering all those presents was as easy as writing this code...
