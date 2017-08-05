---
title: "24 Days of Hackage: haskelldb"
---

I conjecture that the majority of bugs that programmers encounter occur at the
boundaries between 2 different systems. In Haskell, we do everything we can to
encode our assumptions of behaviour via the type system, and perform strict
validation when bringing outside information into our cosy type-safe
bubble. However, this doesn't help if you've requested the wrong data in the
first place - all you can do is throw a runtime exception.

For me, this is a frequent problem with my choice of database access -
`postgresql-simple`. Too often I have queried for `foo, bar` in `some_table`,
but elsewhere assumed I was querying for `bar, foo` - the order doesn't
agree. In today's post, we will look at
[`haskelldb`](http://hackage.haskell.org/package/haskelldb), which is a high
level abstract interface to databases.

Fundamentally, `haskelldb` takes an encoding of your database, and then uses
this information to construct SQL for you, and deals with mapping data too and
from the format the database. Kicking off today's example, again using the
Helping Santa project from earlier posts, we begin by defining our database
interface. First of all, we need to define the columns:

```haskell
data PresentName = PresentName

instance FieldTag PresentName where
  fieldName = const "name"

presentName :: Attr PresentName String
presentName = mkAttr PresentName


data ChildName = ChildName

instance FieldTag ChildName where
  fieldName = const "name"

childName :: Attr ChildName String
childName = mkAttr ChildName


data ChildLocationLat = ChildLocationLat

instance FieldTag ChildLocationLat where
  fieldName = const "loc_lat"

childLocLat :: Attr ChildLocationLat Double
childLocLat = mkAttr ChildLocationLat


data ChildLocationLong = ChildLocationLong

instance FieldTag ChildLocationLong where
  fieldName = const "loc_long"

childLocLong :: Attr ChildLocationLong Double
childLocLong = mkAttr ChildLocationLong
```

For each column we introduce a unique type, we introduce an `Attr` for it (with
its underlying `Expr` type), and also add `FieldTag` instances to map to and
from the database column name. Now that we are armed with these column
definitions, we can define our tables:

```haskell
child :: Table (RecCons ChildName (Expr String)
               (RecCons ChildLocationLat (Expr Double)
               (RecCons ChildLocationLong (Expr Double)
                RecNil)))
child = baseTable "child"
          $ hdbMakeEntry ChildName
          # hdbMakeEntry ChildLocationLat
          # hdbMakeEntry ChildLocationLong

present :: Table (RecCons PresentName (Expr String)
                 (RecCons ChildName (Expr String)
                  RecNil))
present = baseTable "present" $ hdbMakeEntry PresentName
                              # hdbMakeEntry ChildName
```

We use `baseTable` to define the "base" tables in the database - the tables that
actually have data in, and use `hdbMakeEntry` to add columns to each table.

It's quite a bit of up front typing I'll admit, but you could automate this
typing with a custom step in your build process (easy with Shake!), or use
Template Haskell. I've opted to write it all by hand as I find this helps
further develop my understanding, as I only have to understand one thing at a
time.

Now that we've represented the schema in code, let's have a look at leveraging
this to write some queries.

```haskell
allPresents :: Query (Rel (RecCons PresentName (Expr String)
                           RecNil))
allPresents = do
  allPresents <- table present
  project $ presentName << allPresents ! presentName

presentsFor :: String
            -> Query (Rel (RecCons PresentName (Expr String)
                           RecNil))
presentsFor name = do
  children <- table child
  presents <- table present
  restrict $ children ! childName .==. presents ! childName
  restrict $ children ! childName .==. constant name
  project $ presentName << presents ! presentName
```

If you've only written SQL before this might look a little alien - `haskelldb`
provides an API that is very close to relational algebra. In the first example
we've taken the output of the entire `present` table and returned all rows. In
the second example we've selected from two tables - a Cartesian join - and
filtered this join on a predicate to find just the rows about a specific child
(the child's name is given by input).

The beauty of this interface is that `haskelldb` is free to rewrite your query
in a more optimal form. You can have a look at the generated SQL by using
`ppSql`:

```haskell
> ppSql (presentsFor "Little Bobby Tables")
SELECT name2 as name
FROM (SELECT name as name2,
             name as name2
      FROM present as T1) as T1,
     (SELECT name as name1
      FROM child as T1) as T2
WHERE name1 = 'Little Bobby Tables' AND name1 = name2
```

Coupled with PostgreSQL's stunning query planer, the end result is query with
the same speed as one I'd write by hand!

`haskelldb` gives us a extremely expressive API for querying databases, and
doesn't sacrifice performance to do so. This allows you to consume and compose
the API in a way that best suits the needs of your application. If you
frequently select from the result of a join you can simply store part of this
query separately, and compose it later.

One downside of `haskelldb` is that documentation can be a little bit terse, and
the API is quite large, so it can be hard to get going sometimes. I highly
recommend Chris Done's
[series of blog posts on haskelldb](http://chrisdone.com/tags/haskelldb) if
you're interested in learning more.
