---
title: "Announcing engine-io and socket-io for Haskell"
---

I've just released three new libraries to Hackage:

1. [`engine-io`](http://hackage.haskell.org/package/engine-io)
2. [`engine-io-snap`](http://hackage.haskell.org/package/engine-io-snap)
3. [`socket-io`](http://hackage.haskell.org/package/socket-io)

## Engine.IO

Engine.IO is a new framework from [Automattic](http://automattic.com/), which
provides an abstraction for real-time client/server communication over the
web. You can establish communication channels with clients over XHR
long-polling, which works even through proxies and aggressive traffic rewriting,
and connections are upgraded to use HTML 5 web sockets if available to reduce
latency. Engine.IO also allows the transmission of binary data without overhead,
while also gracefully falling back to using base 64 encoding if the client
doesn't support raw binary packets.

This is all very desirable stuff, but you're going to have a hard time
convincing me that I should switch to Node.js! I'm happy to announce that we now
have a Haskell implementation for Engine.IO servers, which can be successfully
used with the Engine.IO JavaScript client. A simple application may look like
the following:

```haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forever)

import qualified Control.Concurrent.STM as STM
import qualified Network.EngineIO as EIO
import qualified Network.EngineIO.Snap as EIOSnap
import qualified Snap.CORS as CORS
import qualified Snap.Http.Server as Snap

handler :: EIO.Socket -> IO ()
handler s = forever $
  STM.atomically $ EIO.receive s >>= EIO.send s

main :: IO ()
main = do
  eio <- EIO.initialize
  Snap.quickHttpServe $ CORS.applyCORS CORS.defaultOptions $
    EIO.handler eio (pure handler) EIOSnap.snapAPI
```

This example uses `engine-io-snap` to run an Engine.IO application using Snap's
server, which allows me to concentrate on the important stuff. The body of the
application is the `handler`, which is called every time a socket connects. In
this case, we have a basic echo server, which constantly reads (blocking) from
the client, and echos the message straight back.

As mentioned, you can also do binary transmission - the following handler
transmits the lovable
[`doge.png`](http://img4.wikia.nocookie.net/__cb20131121015552/creepypasta/images/0/05/Doge.png)
to clients:

```haskell
handler s = do
  bytes <- BS.readFile "doge.png"
  STM.atomically $
    EIO.send socket (EIO.BinaryPacket bytes)
```

On the client side, this can be displayed as an image by using
[data URIs](https://en.wikipedia.org/wiki/Data_URI_scheme), or manipulated using
the HTML 5 [File API](http://www.w3.org/TR/FileAPI/).

## Socket.IO

[Socket.IO](http://socket.io) builds on top of Engine.IO to provide an
abstraction to build applications in terms of events. In Socket.IO, clients
connect to a server, and then receive and emit events, which can often provide a
simpler architecture for web applications.

My Socket.IO implementation in Haskell also strives for simplicity, by taking
advantage of the [`aeson`](http://hackage.haskell.org/package/aeson) library a
lot of the encoding and decoding of packets is hidden, allowing you to focus on
your application logic. I've implemented the example chat application,
originally written in Node.js, using my Haskell server:

```haskell
data AddUser = AddUser Text.Text

instance Aeson.FromJSON AddUser where
  parseJSON = Aeson.withText "AddUser" $ pure . AddUser


data NumConnected = NumConnected !Int

instance Aeson.ToJSON NumConnected where
  toJSON (NumConnected n) = Aeson.object [ "numUsers" .= n]


data NewMessage = NewMessage Text.Text

instance Aeson.FromJSON NewMessage where
  parseJSON = Aeson.withText "NewMessage" $ pure . NewMessage


data Said = Said Text.Text Text.Text

instance Aeson.ToJSON Said where
  toJSON (Said username message) = Aeson.object
    [ "username" .= username
    , "message" .= message
    ]

data UserName = UserName Text.Text

instance Aeson.ToJSON UserName where
  toJSON (UserName un) = Aeson.object [ "username" .= un ]


data UserJoined = UserJoined Text.Text Int

instance Aeson.ToJSON UserJoined where
  toJSON (UserJoined un n) = Aeson.object
    [ "username" .= un
    , "numUsers" .= n
    ]


--------------------------------------------------------------------------------
data ServerState = ServerState { ssNConnected :: STM.TVar Int }

server :: ServerState -> SocketIO.Router ()
server state = do
  userNameMVar <- liftIO STM.newEmptyTMVarIO
  let forUserName m = liftIO (STM.atomically (STM.tryReadTMVar userNameMVar)) >>= mapM_ m

  SocketIO.on "new message" $ \(NewMessage message) ->
    forUserName $ \userName ->
      SocketIO.broadcast "new message" (Said userName message)

  SocketIO.on "add user" $ \(AddUser userName) -> do
    n <- liftIO $ STM.atomically $ do
      n <- (+ 1) <$> STM.readTVar (ssNConnected state)
      STM.putTMVar userNameMVar userName
      STM.writeTVar (ssNConnected state) n
      return n

    SocketIO.emit "login" (NumConnected n)
    SocketIO.broadcast "user joined" (UserJoined userName n)

  SocketIO.on_ "typing" $
    forUserName $ \userName ->
      SocketIO.broadcast "typing" (UserName userName)

  SocketIO.on_ "stop typing" $
    forUserName $ \userName ->
      SocketIO.broadcast "stop typing" (UserName userName)
```

We define a few data types and their JSON representations, and then define our
server application below. Users of the library don't have to worry about parsing
and validating data for routing, as this is handled transparently by defining
event handlers. In the above example, we listen for the `add user` event, and
expect it to have a JSON payload that can be decoded to the `AddUser` data
type. This follows the best-practice of pushing validation to the boundaries of
your application, so you can spend more time working with stronger types.

By stronger types, I really do mean stronger types - at
[Fynder](http://fynder.io) we're using this very library with the
[`singletons`](http://hackage.haskell.org/package/singletons) library in order to
provide strongly typed publish/subscribe channels. If you're interested in this,
be sure to come along to the
[Haskell eXchange](https://skillsmatter.com/conferences/1907-haskell-exchange-2014),
where I'll be talking about exactly that!
