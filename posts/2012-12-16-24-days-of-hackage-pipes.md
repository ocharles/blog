---
title: 24 Days of Hackage: pipes
---

Today's article is going to be an interesting one... not just because the
library that I'll look at - `pipes` - is interesting, but it's also very new to
me!  However, we'll see that `pipes` is a library providing functionality that
can otherwise be difficult, providing an API that even us newbies can
understand. With that subtle disclaimer out of the way, let's get going!

Long running input/output in Haskell is unfortunately a hard problem. We're used
to writing code that is modular and composes elegantly, but naïve approaches to
dealing with IO quickly turn ugly - producing code that really doesn't compose
at all well. Somewhat even more dishearteningly, if you try and solve these
problems you quickly come across 'enumerators', 'iterators', 'iteratees' and all
sorts of other concepts. Unless you're willing to spend some time with these
theories, most people are likely left feeling a little deflated, not to mention
stuck. This really doesn't help in a work environment when you've got deadlines!

Now there are already a number of solutions on Hackage, but today we're going to
look at [`pipes`](http://hackage.haskell.org/package/pipes), as I feel the
theory behind it is extremely compelling, and the provided API is something that
doesn't require hours of academic reading to get results from.

## A Christmas List Manager

To dive right in, lets first understand the underlying idea of the library. As a
user, you will: build `Producer`s of values, the input to your computation;
`Consumer`s that will take values and do something with them, and `Pipe`s which
allow you to transform from one form of data to another. Once you've built these
components, you then compose things together to form a `Session`, which can be
ran to produce your actual application.

The "hello, world" of these libraries is some form of echo, but lets add a
Christmas twist. We're going to build an application which requests a name, and
a list of Christmas presents, until the user submits an empty line. Then it will
echo all of this back. The first thing we need is a something that *produces*
the name:

```haskell
name :: Proxy p => () -> Producer p String IO ()
name () = runIdentityP $ do
  lift $ putStr "Ho ho ho! What is your name? "
  lift getLine >>= respond
```

Our `Producer` prints out a prompt, and then responds with whatever the user
entered. Simple! We can test this in GHCi:

```
> runProxy $ name
Ho ho ho! What is your name? Oliver
```

Nothing was returned though, because we didn't connect a `Consumer`. A trivial
`Consumer` is provided by `pipes` in the form of `printD`. We attach a
`Consumer` to a `Producer` with the `>->` composition operator:

```
> runProxy $ name >-> printD
Ho ho ho! What is your name? Oliver
"Oliver"
```

Excellent, `printD` consumed the `String` produced by `name` and then printed it
back out. Now, how about a `Producer` for that stream of presents?

```haskell
data Present = Present String
  deriving (Show)

presents :: Proxy p => () -> Producer p Present IO ()
presents () = runIdentityP $
    lift (putStrLn "And what presents would you like?") >> go ()
  where
    go = getLineS >-> takeWhileD (not . null) >-> mapD Present
```

Before we look at what's going on here, what happens if we try and run this?

```
Main> runProxy $ presents >-> printD
And what presents would you like?
GameBoy
Present "GameBoy"
Nintendo 64
Present "Nintendo 64"
A Pony
Present "A Pony"

Main>
```

Well that's interesting! Everytime we entered a present, we *produced* a new
`Present` value, which immediately got sent to `printD`! This shows one of the
fundamental principles of `pipes` - you are working with streams of data -
something that I think is a very natural way of programming.

Now, what's going on in this `Producer`? Well, first of all we output a basic
prompt, and then produce a list of `Present`s. We do this by first reading a
line, and constantly consuming these lines until we encounter one that is
empty. We map over all lines that we read, turning them into `Present`. Again,
nothing that you wouldn't write in Haskell otherwise; `pipes` doesn't require
you to relearn these basic ideas - they simply extend nicely to `pipes`.

Now that we have all of this, lets compose everything together to build our
final application:

```haskell
main :: IO ()
main = do
  First (Just name) <- execWriterT $ runProxy $
    raiseK name >-> headD_
  presents <- execWriterT $ runProxy $
    raiseK presents >-> toListD
  putStrLn (name ++ " wants: " ++ show presents)
```

I've used the `headD_` utility to take the first value from the `name`
`Producer`, and then used `toListD` to consume everything in the `presents`
`Producer` and convert it to a list. I then output this using `putStrLn` and
`show`, as normal.

This might seem awfully verbose for what we've achieved, and for this example,
it is. But even so, the code seems very elegant to me as we've split things
apart into small, reasonable, pieces of code. For example, we might want to
store all of this in a database as it's entered - every time a present is added
it pass through a `storeInDatabase` pipe, which would store each `Present` the
moment it's entered. Or we might later expand what a `Present` consists of, and
now we only have to change our `Present` `Producer`.

## Echo (echo, echo)

[Gabriel Gonzalez](http://haskellforall.com) handed me a little example for
showing off `pipes`, in the form of a pair of applications - a client and a
server. This shows how pipes can be used with some slightly more interesting
(less contrived!) IO:

```haskell
server = withSocketsDo $ do
    s <- listenOn (PortNumber 5553)
    (h, _, _) <- accept s
    hSetBuffering h LineBuffering
    runProxy $ hGetLineS h >-> putStrLnD
    hClose h
```

The server simply opens a socket and listens port 5553, and then consumes lines
as they are sent in. The client is equally elegant:

```haskell
client = withSocketsDo $ do
    h <- connectTo "localhost" (PortNumber 5553)
    hSetBuffering h LineBuffering
    runProxy $ getLineS >-> takeWhileD (/= "quit") >-> hPutStrLnD h
    hClose h
```

The client connects to the server, and then consumes user input, until the user
types "quit". For every line typed, it is sent directly to the server.

## The Rest

As you can see, pipes really doesn't require you to learn a whole lot of stuff -
once you get to grips with `runProxy`, and composition via `>->` and `>=>`, you
are already able to do a *lot* of stuff. Gabriel has done a fantastic job with
documentation on this library too, a practice I really hope other authors will
follow. I highly recommend reading the
[tutorial](http://hackage.haskell.org/packages/archive/pipes/3.0.0/doc/html/Control-Proxy-Tutorial.html),
which will also show you how pipes can be used for:

- Bidirectional communication - sending requests upstream, possibly with
  metadata.
- Resource management
- Error handling
- More!

`pipes` is a really nice library, and while it's still not quite there, it's
well on the way to being a great solution to framing IO computations in
Haskell. Thanks Gabriel, and good luck with your future work!
