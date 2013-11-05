-----
title: A Comparison Between Perl and Haskell
-----

Yesterday I finished porting a [MusicBrainz](http://musicbrainz.org/) production
service from Perl to Haskell. The port was done for no real reason other than
giving me something to do during my commute, but as work developed I became
interested in how the two applications might compare. In this post I will
discuss my experience, hopefully providing one more data point for people who
are considering using Haskell for production work.

## The `caa-indexer`

The project I ported was the
[`caa-indexer`](http://github.com/metabrainz/caa-indexer), a long running
service that achieves consistency between data stored in the MusicBrainz
database, and data stored by the Cover Art Archive itself. If you're not
familiar with the [Cover Art Archive](http://coverartarchive.org), here's a brief
technical overview of how it works.

The Cover Art Archive (CAA) stores artwork about music. Every release in
MusicBrainz has a MusicBrainz identifier, and the CAA is a separate database
associating images with these identifiers. Images are stored by the Internet
Archive, and can be managed via an S3-like protocol. The metadata about the
image - such as tracking which MusicBrainz release it belongs to, or what type
of image it is - is stored in the main MusicBrainz database. As artwork is
added, removed, or otherwise modified, events are propagated from the
MusicBrainz database over RabbitMQ to the `caa-indexer`, who responds to each
event accordindly. There are three events - indexing, moving and deleting:

* Indexing refreshes the `index.json` file, which is a JSON serialization of
  metadata that MusicBrainz stores. This is used by the
  http://coverartarchive.org web service. It also builds a `mb_metadata.xml`
  file, which is a snapshot of the release according to the MusicBrainz web
  service. This is used by the Internet Archive to enhance their own search
  results.

* Move events are used to move an image from one bucket to another. For example,
  when two releases are merged, all the images are moved to the same bucket.

* Delete events delete data from buckets - be that images or the `index.json`
  file.

Thus any `caa-indexer`-like project needs the ability to:

* Communicate with RabbitMQ to listen for events.
* Communicate with PostgreSQL to fetch metadata from the MusicBrainz database.
* Communicate with the Internet Archive's S3 store
* Produce JSON documents
* Make web requests to musicbrainz.org

## Porting the `caa-indexer`

### Process

My methodology for the porting was ad hoc, and not particularly coordinated. I
was happy with the existing architecture, so the Haskell rewrite is almost
taking code verbatim from Perl and doing the necessary rewriting. The general
architecture remained the same.

### Experience

#### Library Support

Everything the `caa-indexer` needs to do is supported by Haskell libraries on
[Hackage](http://hackage.haskell.org), which is the central location of Haskell
libraries (much like CPAN for Perl). I used the following libraries to meet the
requirements:

* [`postgresql-simple`](http://hackage.haskell.org/package/postgresql-simple)
  for talking to PostgreSQL.
* [`amqp`](http://hackage.haskell.org/package/amqp) for talking to RabbitMQ.
* [`aws`](http://hackage.haskell.org/package/aws) for talking to the Internet
  Archive.
* [`aeson`](http://hackage.haskell.org/package/aeson) for JSON serialization
* [`http-conduit`](http://hackage.haskell.org/package/http-conduit) for making
  web requests. I initially wanted to use
  [`http-streams`](http://hackage.haskell.org/package/http-streams) for this,
  but as `aws` already used `http-conduit`, it was easier to use the same
  library.
* [`xml`](http://hackage.haskell.org/package/xml) for trivial XML processing of
  responses.
* [`configurator`](http://hackage.haskell.org/package/configurator) to configure
  the application outside of the code itself.

The following were also used to produce more idiomatic Haskell code:

* [`uuid`](http://hackage.haskell.org/package/uuid) provides a `UUID` data type,
  allowing me to achieve a little more type safety.
* [`async`](http://hackage.haskell.org/package/async) was used to process events
  in a separate thread, in order to provide a somewhat ultimate `try`/`catch`.
  We'll see more on this later.
* [`errors`](http://hackage.haskell.org/package/errors), which is part of my
  standard toolkit for dealing with things going wrong.
* [`text`](http://hackeg.haskell.org/package/text) which is the canonical way to
  deal with text in Haskell, along with encoding and decoding bytestreams.

Together, these libraries make a formidable toolkit for solving problems in the
`caa-indexer`'s domain. Of these libraries, the `aws` library was new to me, and
I hadn't used `http-conduit` for a long time. Documentation on these libraries
was particularly helpful - and I didn't find it difficult to learn the new APIs.
The fact that Haskell provides type signatures means that Haddock (Haskell's
documentation tool) is already able to provide documentation even if the library
is otherwise undocumented. The `xml` library falls into this category, and while
it could have done with better documentation, being able to find functions
purely on their type signature was sufficient.

#### Code Size

The Haskell codebase measures at 389 lines of code, while the Perl codebase
measures at 399. I'm somewhat amazed at how similar both codebases are in size. Haskell
is a tiny bit more compact, though this is likely due to the fact that all code
is in a single file, where as the Perl code is split over multiple files. That
said, it's clear that the difference in code size is neglible. What's more
interesting to note is that the
Haskell code provides significantly more guarantees due to its type system, in
the same amount of code as Perl. I believe this is a testement to just how
succinct Haskell code can be.

That said, there are places in the Haskell code that are more verbose than Perl,
due to being more type safe. One example is dealing with UUIDs from the
MusicBrainz database. In Perl, almost everything coming out of the database is
just a string, so UUIDs are dealt with like any other bit of text.
`postgresql-simple`
however checks the types of columns against the type they are being
represented with in Haskell at runtime. Thus if you want to deal with UUIDs in
Haskell, you need to either cast it to plain text in the query, or write
(de)serializers for this data type. I chose the latter, and had to write the
following code, which has very little to do with the `caa-indexer`:

```haskell
instance Pg.ToField UUID.UUID where
  toField = Pg.Plain . Pg.inQuotes . Builder.fromString . UUID.toString


instance Pg.FromField UUID.UUID where
  fromField f Nothing = Pg.returnError Pg.UnexpectedNull f "UUID cannot be null"
  fromField f (Just v) = do
    t <- Pg.typename f
    if t /= "uuid" then incompatible else tryParse
    where
      incompatible = Pg.returnError Pg.Incompatible f "UUIDs must be PG type 'uuid'"
      tryParse = case UUID.fromString (Char8.unpack v) of
        Just uuid -> return uuid
        Nothing -> Pg.returnError Pg.ConversionFailed f "Not a valid UUID"
```

Hardly the end of the world, but it's code that otherwise detracts from the main
purpose of the `caa-indexer`. I will likely wrap this code up in a small library
and upload it to Hackage in the future.

The only other piece of code that feels a little more verbose in Haskell is the
JSON serialization itself. In Perl, I can write the following:

```perl
$json->objToJson({
    images => [
        map +{
            types => $_->{types},
            front => $_->{is_front} ? JSON::Any->true : JSON::Any->false,
            back => $_->{is_back} ? JSON::Any->true : JSON::Any->false,
            ...
        }, $self->dbh->query(
            'SELECT * FROM cover_art_archive.index_listing
                WHERE release = ?
                ORDER BY ordering',
            $release->{id}
        )->hashes
    ],
    release => 'http://musicbrainz.org/release/' . $release->{gid}
});
```

Which maps over the results of database query directly into JSON. In Haskell, I
chose to use first convert the row into a Haskell data type, and then write a
serializer for this data type. Thus, the code is more along the lines of:

```haskell
data IndexListing = IndexListing
  { indexRelease :: UUID.UUID
  , indexImages :: [ReleaseImage]
  }


data ReleaseImage = ReleaseImage
  { imageTypes :: Vector.Vector Text.Text
  , imageIsFront :: Bool
  ...
  }

instance Aeson.ToJSON IndexListing where
  toJSON IndexListing{..} = Aeson.object
    [ "images" .= indexImages
    , "release" .= ("http://musicbrainz.org/release/" ++ UUID.toString indexRelease)
    ]

instance Aeson.ToJSON ReleaseImage where
  toJSON ReleaseImage{..} = Aeson.object
    [ "types" .= imageTypes
    , "front" .= imageIsFront
    ...
    ]

-- later:
images <- Pg.query pg [sql| SELECT ... |]
Aeson.encode (IndexListing release images)
```

Which as you can see, is significantly more verbose, and has required me to
write the `data` declaration with selector names, only to later write this again
using `ToJSON`. Haskell does have
[generics](http://www.haskell.org/haskellwiki/GHC.Generics) support, but for JSON I find this is
usually suboptimal when you have a specific schema in mind. I also needed to
include properties in the JSON document that are a function of other value (such
as expanding the image ID into a full URL), so automatically deriving JSON
serialization is not sufficient, unless I want to store duplicate state in
`ReleaseImage` itself (which is certainly worse).

There is a price to pay for being more type safe, and while often this is
superficial, there are places where it leads to a little more friction. One
could argue that introducing the data type leads to better testability however, as
it's now possible for me to check the JSON serialization in a pure manner
without touching a database at all.

#### Development experience

A friend at a recent Haskell meetup once described Python development to me as
"backtrace driven development", and unfortunately Perl often feels very much the
same. I generally write code in Perl by sketching out an idea, and then trying
to run it. If it crashes, I refine my code and try again. It's a very haphazard
approach to building software. It can be improved by working in a test driven
manner, but ultimately you are still working with assurances over a subset of
the codebase.

In Haskell I find there are two main approaches, depending on how you are
solving problems. If you have a solution in mind, you can take a loosely "holes
driven" approach to programming, where you write top level types and leave
implementations blank; or you can do more bottom up programming by working in
the `ghci` REPL. Having already written this code once in Perl, I opted for the
former - first getting my types right, and then filling in the blanks later.

Structuring my types took a while - in fact the entire project didn't compile for about
4 hours. This may seem very strange to some people, but I find it very natural
in Haskell. The lack of compilation doesn't imply I'm not making progress - in
those 4 hours I learnt more about my program, developing an understanding about
how components
fit together. Once it compiled, it was then a case of replacing all calls
to `undefined` with a real implementation. Again, this inevitably lead to false
assumptions and compiler errors, so the cycle repeated.

Remarkably, once I expanded `undefined` and had things compiling, the majority
of the battle was won. Running the program connected to RabbitMQ, setup the
queues I expected, and was listening for events as I expected. This isn't to say
the job was done - there were still places where runtime execution was wrong, or
error handling was insufficient.

The problems at runtime where genuine problems, and the bugs that I find
interesting to fix. I wasn't spending time working out why something was
`null`, or if a method call had the wrong arguments, because the compiler had
already held my hand through that stage of programming. Overall the development
experience was *fun*.

#### Community Experience

I wrote this port almost entirely on my own, and didn't have to interact much
with the Haskell community. The `amqp` package didn't initially have support for
message headers, which is needed for some of the retry logic, so I raised a bug
on the `amqp` project's Github issue tracker. The author was responsive and
released a new version of the library with this support.

While extremely limited, this is consistent with my experience of the Haskell
community outside this work. A small group of people very passionate about their
work, and who embrace working with others.

### Where Haskell Shines

There are few things in the Haskell port that I really liked. This section is a
place to express my enjoyment of writing Haskell, and show off some of the
things I found cool. While these points are highly specific and won't help you
make a choice of whether you want to write your projects in Haskell, I hope that
some of my enthauiasm will reach you, and express my feeling that Haskell is
both fun and a constant learning experience.

#### `RankNTypes` for building `EventHandler`s

The data type of an `EventHandler` uses `RankNTypes`:

```haskell
data EventHandler = forall payload. EventHandler
  { eventRoutingKey :: Text.Text
  , eventParser :: String -> Maybe payload
  , eventAction :: HandlerEnv -> payload -> IO ()
  }
```

This lets me have lists of `EventHandler`s, each of which is able to parse a
`String` into a handler specific payload of a more refined type. Thus, I have a
few top level definitions such as `index :: EventHandler` and I can later map
over all `EventHandler`s to register them with RabbitMQ:

```haskell
mapM_ registerEventHandler [ index, move, delete ]
```

This is a simple way to achieve a somewhat hetrogeneous list. The alternative
approach of parameterizing an `EventHandler` by its payload type would make this
list construction harder to use, so by moving the choice of the payload to the
`EventHandler` value itself, the code is simplified.

#### Power in Monads

I use a few different monads inside my `caa-indexer`. For example, I use the
`Maybe` monad to simplify XML traversals:

```haskell
XML.parseXMLDoc (HTTP.responseBody response) >>=
XML.findChild (XML.unqual "Error") >>=
XML.findChild (XML.unqual "Resource") >>=
pure . XML.strContent
```

This parses a HTTP response as XML, and finds the text content inside the
`<Error><Resource>` element. If any of this fails, the whole computation aborts
and `Nothing` is returned. I find it particularly elegant that the monadic
sequencing took care of this for me.

Another monad that I really like is `EitherT`, which lets me sequence a series
of actions, aborting much like `Maybe` if anything fails, but with the ability
to provide a reason for failure. `EitherT` *transforms* a base monad and I use it
with IO to provide a try/catch like block:

```haskell
EitherT.eitherT
  (retry msg handlerChan eventRoutingKey)
  (const $ return ()) $
  (do payload <-
        eventParser messageBody ?? (toException MessageBodyParseFailure)

      EitherT.EitherT $
        Async.withAsync (eventAction env payload) Async.waitCatch)
```

The final `do` block is the code to execute. This code can either result in an
exception, at which point `retry` is invoked to retry the message in 4 hours
(this is done by dead lettering in RabbitMQ), or it can succeed by running to
completion.

I use `??` to lift the `Maybe` result of the `eventParser` into `EitherT`, and I
use `withAsync` to run a computation on a separate thread, and if this dies
because of an exception I lift the exception into `EitherT`. Thus we have a nice
try/catch-like piece of code, that required no language support - we just built
it ourselves.

## Personal Closing Comments

We all know that it's important to choose the right tool for the job, but I am
honestly yet to find problems where Haskell *isn't* a suitable tool. Perhaps a
while ago when the library coverage was sparse, Haskell wasn't a great choice -
but now that is patently not the case. I showed that it's easy to write software
that interacts with relational databases, message brokers and Amazon web
services, all without having to write any of this code myself.

Haskell is often claimed to drastically reduce code size, but I didn't find this
claim held up against other high level languages. On the other hand, I also
didn't find that Haskell was any more verbose either - so I feel that claims
about having to "work to please the type system" don't really hold up to much,
though there are cases where it does add a little more friction.

Code legibility is another important issue, and I can't provide much feedback
around this. Of course, *I* feel the Haskell is readable because I wrote it! The
only useful exercise here is for you to see for yourself:

* [The Perl `caa-indexer`](https://github.com/metabrainz/CAA-indexer/tree/bfc1381aac55f1b861b19702a049b778f510fa6a/lib/CoverArtArchive)
* [The Haskell `caa-indexer`](https://github.com/metabrainz/CAA-indexer/blob/4fb2f260f1b5dd66d3dcaef1a188818a89705002/src/Main.hs)

More important to me though, is that the compiler is always reading the code
with me. While I feel Perl code (in general) is readable, I have frequently
misinterpreted the meaning of some code and carried false assumptions forward
later in my project. Haskell isn't free of this either, but a lot of assumptions
are spelled out in the types. The more you use types (such as phantom types,
GADTs), the more we encode about data and the harder it is to misinterpret code.

The only remaining thing that makes me uncomfortable is getting the rest of my
team to write some Haskell. We have so much going on at MusicBrainz, there
hasn't been a good time to accept that things will have to slow down while
people learn a new language, and the idioms that go with it. I don't feel that
Haskell is particularly deserving of the scary/academic label it sometimes gets.
Yes, it will be alien, but if something doesn't require a little bit of rewiring
in order to show you a different perspective on things, why bother learning it?
Moving between Python and Ruby is easy, because there is very little between
them, but I think that also results in very little net gain in moving between
them as well.

With an extremely powerful type system, a vibrant community, a comprehensive set
of libraries, a new programming paradigm (purity and laziness) and a beautifully
succinct language, Haskell is still the ultimate language to me. If you haven't
yet picked up Haskell, I encourage you to go and [Learn You a Haskell for Great
Good](http://learnyouahaskell.com) today.
