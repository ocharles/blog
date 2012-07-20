---
title: A New Database Abstraction Layer
---

The New Edit System, NES, requires a fairly hefty new schema due to increase of
tables to actually do the versioning. While the bulk of this schema is done,
it's fairly useless if the website can't talk to it!

The next step for me is to get the website to talk to the new database. This
isn't quite as straight forward as changing the implementation of existing API
methods - the whole API needs to change to match the main ideas of NES (trees,
revisions, etc). Even if it was a case of just changing the API, this means I
have to write more Perl code, fix hundreds of tests, etc.

It doesn't fill me with great joy to do this rewrite, partly because it's just
not fun work, but also because I don't believe I'll do a good a job as I can,
simply because it's Perl.

In this post, I've outlined the 2 major ideas going round - PostgreSQL as an
API, and database as a service.

---

## PostgreSQL As An API

One option is to move a lot of the heavy lifting to PostgreSQL through stored
procedures. Before we look at the pro's and con's of this, lets have a look at
what this would look like:

```sql
CREATE TYPE loaded_artist (
  artist_id uuid,
  revision_id integer,
  artist_tree_id integer,
  name text,
  sort_name text,
  artist_type_id integer,
  -- etc
);

-- This gets the latest 'master' version of an artist and joins in basic data
-- from the artist tree
CREATE FUNCTION get_latest_artist_by_mbid(in_mbid UUID)
RETURNS SETOF loaded_artist AS $$
    SELECT
      artist_id, revision_id, artist_tree_id, name.name,
      sort_name.name AS sort_name, artist_type_id
    FROM artist
    JOIN artist_revision USING (artist_id)
    JOIN artist_tree USING (artist_tree_id)
    JOIN artist_data USING (artist_data_id)
    WHERE artist.master_revision_id = revision_id
      AND artist_id = $1
$$ LANGUAGE SQL;

```

This is a real-world example of something that we need to do in NES - loading
artists by their MBID. The first part of the above code snippet declares a new
data type, `loaded_artist` which we can use to enforce a little more type safety
throughout the API. By defining functions in terms of `loaded_artist`, we are
bound to a contract that they /must/ return data that matches that structure.

For some clients, direct queries into the database might be enough, but most
people are going to going to want to talk to the database by some language
specific bindings. In Perl, it might look something like:

```perl
sub get_latest_by_mbid {
    my ($self, $mbid) = @_;
    return $self->new_from_row(
        $self->sql->select_single_row_hash(
            'SELECT * FROM get_latest_artist_by_mbid(?)',
            $mbid));
}
```

Where `new_from_row` knows how to turn a `loaded_artist` into some Perl data
type. The Perl code is now minimized to being a very light wrapper around the
database.

### Advantages

#### Type Safety

PostgreSQL has a pretty nice type system. Method's can only be called if you
specify the correct number of arguments, of the write types. Return types allow
function composition to be type safe.

#### API Cohesion

The type system also creates some great cohesion. In the above example if I
changed the specification of what a `loaded_artist` is but didn't update
`get_latest_artist_by_mbid`, I wouldn't be able to make those changes.

#### Universally Useful

No matter what programming language you're working in, if you can talk to the
database you immediately have a very rich API to work with.

#### Documentation

Documentation in PostgreSQL is sometimes overlooked, but the [COMMENT][]
declaration lets you assign comments to various objects in the database. This
requires the object actual exis

#### Simplicity

This does not change our application stack at all. We already require
PostgreSQL, we're just using it for more higher level operations.

### Disadvantegs

#### API Cohesion

We only get the aforementioned type of cohesion at stuff that's first class, and
more complicated functions (such as PL/Python, PL/Perl and even PL/PGSQL) will
not be type checked at all. This means they may crash at runtime.

Also, this type saftey is not great over `ALTER`ing things - if I later run
`ALTER TYPE` on `loaded_artist` and drop an attribute, the existing functions
will only break at runtime. This problem can be avoided by always doing 'full'
installations rather than migrations (think loading snapshots rather than deltas).

#### Deployment

Releasing new versions of the server now involves having to do database updates
as well. There are tools to help with this, such as [Versioning][], [Sqitch][]
and more.

#### Tool Support

Tool support is weaker for PostgreSQL. There is no real way to debug things
other than print tracing. `EXPLAIN` and log analysis is about as far as you get
with a code profiler (which is not awful by any means).

#### Limited Language

SQL is not the most flexible of languages, and this shows when you start to what
to do things like abstract functions out. Take the example of relationships -
fetching relationships between `x` and `y` is always the same, but the `l_a_b`
table changes, as do the tables you link to. This either leads to massive
amounts of copying and pasting, or [dynamic SQL][]. I usually go with the latter
these days, and create `install_` functions that are then immediately selected
with various parameters, which gives a poor mans compile time phase.

#### Not Cache Aware

As you're talking directly to the database, you can't really make use of
memcached in between. Client libraries would have to do this,

### Personal Opinion

I like the increased safety, but it just feels a bit *weird*, though I can't
quite my finger on where. Not very helpful, I know! I've been somewhat
prototyping this API in my [nes-playground][nes-playground] project, if you want
to have a look.

---

## Database As a Service

Rather than having applications talk to PostgreSQL, they actually talk to a
service somewhere. This could be a REST server, a SOAP server, a Thrift server,
that isn't really important to this discussion. But the idea is that you make
the database an implementation detail, so when handling website requests you
consult this service, rather talk to the database directly.

This is essentially what we have right now, it's just that the service happens
to run in the same process as the web server itself. However, I think if we were
going with this idea we'd be looking at moving this service out of the web
server process and into its own process, giving us the ability to implement this
in a different language.

### Advantages

#### Language Choices

Rewriting the service layer doesn't have to be done in Perl, we could write it
in Java, Python, C, whatever. This will let us do a separate round of language
evaluation, and hopefully we can choose something that doesn't have quite the
same amount of [accidental complexity][] as Perl.

#### Communication With Other Services

Now that we have a service we can aggregate data from other services, or
otherwise communicate with them. For example, memcached, message queues, and so
on. However, as good as this sounds it does mean that the 'service' itself
starts to grow and have more dependencies, or the code becomes more complicated
to do with missing services.

### Disadvantages

#### More to Run

If a client wants to use our API they now have to run a PostgreSQL server for
their queries to run against, but they also have to run this service somewhere
as well. And as alluded to earlier, if the service also requires *other*
services, then they might need those as well.

#### Potential Overhead

Depending on the transport layer for the service, there is more overhead. For
example, most people will probably want REST+JSON, which means you have the
overhead of a HTTP server, and the overhead of serializing and then
deserializing. With a lot of roundtrips, this can be expensive. PostgreSQL
doesn't really suffer from this as much as it's a binary protocol with
persistent connections.

---

## Mini Conclusion

My personal feeling for this is that moving things to PostgreSQL is going to be
enough, though I do like the added type checking that we get. However, I'm not
sure that justifies the increased costs in deployment, the context switching,
and a development environment that's likely new to a lot of people.

With that said, Perl is becoming a maintainence nightmare. We're at least
tightening some parts with my drive for more tests and faster tests, but it's
becoming hard to trust the code that we all write. Though I'm sure I won't be
popular with my choice of language :)

Being practical we have 2 full time developers, and a whole stack of bugs and
more as we go forward. We need something that is going to allow us to get
results progressively, rather than some gold perfect system 4 years later. And
my NES work is somewhat blocked by this, so we need to figure it out fairly
soon!

[COMMENT]: http://www.postgresql.org/docs/9.1/interactive/sql-comment.html
[Versioning]: https://github.com/depesz/Versioning
[Sqitch]: http://sqitch.org/
[dynamic SQL]: http://www.postgresql.org/docs/9.1/interactive/plpgsql-statements.html#PLPGSQL-STATEMENTS-EXECUTING-DYN
[accidental complexity]: http://en.wikipedia.org/wiki/Accidental_complexity
[nes-playground]: https://github.com/ocharles/nes-playground
