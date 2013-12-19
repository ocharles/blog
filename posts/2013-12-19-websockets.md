---
title: 24 Days of Hackage: websockets
---

Traditionally, the web has been modelled around a request/response
abstraction. Clients request information from a server, who respond to these
requests. However, this is all starting to change with the emergence of HTML5
technologies. One of these technologies is the
[WebSocket](https://en.wikipedia.org/wiki/WebSocket) protocol, which provides
full-duplex communications channels over a single TCP connection. This radically
changes how we build applications, as we now have the ability to have much
closer to real-time communication between web pages and
servers. The good news is, you don't have to go running off to Node.js to play
with this - thanks to the hard work of
[Jasper Van der Jeugt](http://jaspervdj.be/) of
[Erudify](http://www.erudify.com/), we have production grade support in Haskell
via the [`websocket`](http://hackage.haskell.org/package/websockets) library.

`websockets` is usable at a few different layers of the stack - the library
exposes both low level bytes, but also higher level abstractions if you
are working with text based messages. All of the framing and protocol
handshaking is dealt with for you, along with the various different versions of
WebSocket protocols. As has been the mantra for libraries this year -
`websockets` gets out of your way and lets you concentrate on the interesting
work. Let's jump in and see how this all fits together.

Today, we'll build a simple REPL for interacting with Santa's wish-list. There
are two commands: "I want *x*" is used to record a desired present, and "What do
I want?" is used to query the server for the list of desired presents. To begin
with, let's start up a server:

```haskell
main :: IO ()
main = runServer "127.0.0.1" 8080 handleConnection
```

`runServer` sets up a server (suitable for development purposes) on port 8080,
and `handleConnection` will be invoked every time a client connects to the
server. The interesting work is at `handleConnection`. Being a REPL, we can
easily write a read-evaluate loop for the body of this. First of all though, we
need to accept the connection to complete the handshake.

```haskell
handleConnection pending = do
  connection <- acceptRequest pending
  let loop wants = do
```

Once the connection has been accepted, we enter an infinite loop and block
awaiting a command. To await a command, we need to read from the socket, which
we can do with `receiveDataMessage`. `receiveDataMessage` is an example of a
slightly higher level abstraction, as it filters out the so-called *control
messages*.

```haskell
        commandMsg <- receiveDataMessage connection
```

There are two types of data messages, `Text` and `Binary`. We're writing our
own little text protocol, so we'll concern ourselves with the former first. We
check what type of message we have by doing case analysis on `commandMsg`. Even
better, we can pattern match on the contents of commands by using a fantastic
GHC extension called
[view patterns](http://www.haskell.org/ghc/docs/latest/html/users_guide/syntax-extns.html#view-patterns).

To use view patterns, rather than pattern matching against a variable or
literal, we pattern match using a function, and then pattern match the result of
the call. The `Text` constructor for messages is a lazy `ByteString`, so we can
write a function that transforms our lazy `ByteString` into what the user wants,
possible failing:

```haskell
parseWant :: LBS.ByteString -> Maybe Text
parseWant p = stripPrefix "I want " . decodeUtf8 . LBS.toStrict
```

This naturally fits in to our case analysis:

```haskell
        case commandMsg of
          Text (parseWant -> Just want) -> do
            sendTextData connection
              ("Hohoho, as long as you've been good this year!" :: Text)
            loop (want : wants)
```

If the pattern match succeeds then we learn what the user wants. We acknowledge
this by sending a text message back, and then we continue to loop keeping track
of their want.

For querying, it's very similar. However, this time we don't need to use a view
pattern because the command is a constant string:

```haskell
         Text "What do I want?" -> do
            mapM (sendTextData connection) wants
            loop wants
```

This time we send every wanted item as a single text message, and then continue
to await another command.

Finally, we need to handle the case where neither of these patterns match. In
this case, we'll transmit some HTML that the client should embed, and loop with
the same state once again:

```haskell
        _ -> do
          sendTextData connection ("<img src=\"http://bit.ly/1kmRC7Q\" />" :: Text)
          loop wants
```

To wrap it all up, we simply begin our loop with an empty wish-list:

```haskell
  loop []
```

And we're done! With
[a little HTML](https://github.com/ocharles/blog/blob/master/code/2013-12-19-client.html),
we get a web page that lets us have interactions as shown below:

<div style="text-align: center">
<img src="/img/2013-12-19-websockets.png" />
</div>

In production you'll want to use a better server than the development server,
and Hackage has your covered there too:

* [`wai-websockets`](http://hackage.haskell.org/package/wai-websockets) can be
  used intercept requests with the Warp server.
* [`websockets-snap`](http://hackage.haskell.org/package/websockets-snap) allows
  you to add WebSocket support to a Snap application.
