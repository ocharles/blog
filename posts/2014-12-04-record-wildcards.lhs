---
title: 24 Days of GHC Extensions: Record Wildcards
---
 
Occasionally, you come across a little trick or method for doing something that
seems somewhat inconsequential - but rapidly becomes an indespensible item in
your programming toolbox. For me, the
[`RecordWildcards`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/syntax-extns.html#record-wildcards)
extension is a prime example of this scenario.

> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE RecordWildCards #-}
> import Data.Aeson

To start with, let's recap records in Haskell. A record is usually known to be a
data type with a single constructor, and the data type is populated with a
collection of *fields*. Records crop up all the time in programming, often when
we try to model the real world:

> data Worker = Worker
>   { workerName :: String
>   , workerPosition :: String
>   , workerFirstYear :: Int
>   }

Of course, data alone isn't much fun - we probably want to operate on this data
too. In this case we'd like to interact with other web services, and we'll use
the common JSON format for communication. If we have a specific schema that we
need to conform to, it may be easier to write this by hand:

```haskell
instance ToJSON Worker where
  toJSON w = object [ "name" .= workerName w
                    , "position" .= workerPosition w
                    , "first-year" .= workerFirstYear w
                    ]
```
 
Having to apply each record field getter to the `w` variable is a little
tedious, and `RecordWildCards` can allow us to eliminate that bit of
boilerplate:


> instance ToJSON Worker where
>   toJSON Worker{..} = object [ "name" .= workerName
>                              , "position" .= workerPosition
>                              , "first-year" .= workerFirstYear
>                              ]

Here we see the `Worker{..}` pattern match - this pattern matches on the
`Worker` constructor, and introduces bindings for *all* of the fields in
`Worker`. Each of these bindings will be named after the respective field in
the record. We can see on the RHS that we are now constructing our JSON object
just out of variables, rather than function applications.

If you were expecting a lot of ground breaking new features from
`RecordWildCards` you might be disappointed - that's about all it does! However,
did you know that you can also use `RecordWildCards` when creating data? For
example, we could also write a JSON deserializer as:

> instance FromJSON Worker where
>   parseJSON = withObject "Worker" $ \o -> do
>     workerName <- o .: "name"
>     workerPosition <- o .: "position"
>     workerFirstYear <- o .: "first-year"
>     return Worker{..}

Personally, I don't use this feature as much as creating bindings - in this
case I'd just use applicative syntax - but it can occasionally be handy.

`RecordWildCards` For Modules
-----------------------------

I've presented a fairly "vanilla" overview of `RecordWildCards` - and I imagine
this is probably how most people use them. However, when used with a record of *functions*,
you can do some interesting tricks to emulate localised imports.

In my [`engine-io`](http://hackage.haskell.org/package/engine-io) project, I
have a data type called
[`ServerAPI`](http://hackage.haskell.org/package/engine-io-1.2.3/docs/Network-EngineIO.html#t:ServerAPI) -
here's a snippet:

```haskell
data ServerAPI m = ServerAPI
  { srvGetQueryParams :: m (HashMap.HashMap BS.ByteString [BS.ByteString])
  , srvGetRequestMethod :: m BS.ByteString
  }
```

The intention here is that users provide a `ServerAPI` value when they initialize
`engine-io`, and I then have an abstraction of a web framework to play
with. People can instantiate `ServerAPI` for
[Snap](http://hackage.haskell.org/package/engine-io-snap-1.0.2/docs/Network-EngineIO-Snap.html#v:snapAPI)
or
[Yesod](http://hackage.haskell.org/package/engine-io-yesod-1.0.1/docs/Network-EngineIO-Yesod.html#v:yesodAPI),
and `engine-io` (should!) just work. In `engine-io`, by using `RecordWildCards`,
the programming experience is natural, as the abstraction created by
`ServerAPI` stays behind the scenes. For example:

```haskell
handlePoll :: MonadIO m => ServerAPI m -> Transport -> Bool -> m ()
handlePoll api@ServerAPI{..} transport supportsBinary = do
  requestMethod <- srvGetRequestMethod
  ...


handler :: MonadIO m => EngineIO -> (Socket -> m SocketApp) -> ServerAPI m -> m ()
handler eio socketHandler api@ServerAPI{..} = do
  queryParams <- srvGetQueryParams
  ...
```

This is very similar to using a type class - however, using type classes would
be very tricky in this situation. Either `engine-io` would have to depend on
both Snap and Yesod (though it needs neither), or I would have to use orphan
instances. Neither are particularly desirable. Furthermore, who's to say there
is only one choice of `ServerAPI` for Snap? It's entirely possible to provide a
debugging version that logs what's happening, or for people to switch out calls
however they see fit. This is possible with `newtype`s in type classes, but
pushes a lot of this work onto users.

[Gabriel Gonzalez](http://www.haskellforall.com/) has
[a blog post](http://www.haskellforall.com/2012/07/first-class-modules-without-defaults.html)
on this very technique that goes into more details, which is well worth a read.

----

*This post is part of
[24 Days of GHC Extensions](/pages/2014-12-01-24-days-of-ghc-extensions.html) -
for more posts like this, check out the
[calendar](/pages/2014-12-01-24-days-of-ghc-extensions.html)*.
