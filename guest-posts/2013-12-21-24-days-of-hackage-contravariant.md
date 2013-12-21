---
title: 24 Days of Hackage: contravariant
---

Today and tomorrow we take a brief excursion to the boundary of
[abstract nonsense](https://en.wikipedia.org/wiki/Abstract_nonsense) with a pair
of guest posts from [Tom Ellis](http://web.jaguarpaw.co.uk/~tom/blog/). Tom,
over to you!

---

In today and tommorow's posts I'm going to introduce the `Contravariant` and
`Profunctor` type classes, which are found in the
[`contravariant`](http://hackage.haskell.org/package/contravariant) and
[`profunctors`](http://hackage.haskell.org/package/profunctors) packages,
respectively.  They are closely related to the familiar `Functor` type class, so
let's refresh our understanding by first reviewing its definition:

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

We will use the intuition that a `Functor` is a sort of "producer of output"
that can have its type adapted.  If `f` is a functor, then `f a` represents a
"producer" of type `a`. Using a normal Haskell function of type `a -> b` we can
adapt the output to the type `b` by using `fmap`. Here are three examples of the
"producer of output" intuition for a `Functor`:

1. `Maybe a` represents an output of type `a` which may be either present
   (`Just a`) or absent (`Nothing`).  Using `fmap f :: Maybe a -> Maybe b` we
   can adapt that potentially absent `a` into a potentially absent `b`.

2. `[a]` represents a sequence of output values of type `a`, and with the
   `Functor` instance for `[]` we can adapt them to a sequence of output values
   of type `b`.

3. A value of type `r -> a` is a function taking an input of type `r` and giving
   an output of type `a`.  The `Functor` instance for `(->) r` - which is the
   type of functions taking `r` as input - allows us to convert this output to
   type `b`, by composition:

```haskell
instance Functor ((->) r) where
  fmap f g = f . g
```

   Incidentally, this is also the underlying `Functor` instance of the reader
   monad.

While `fmap` allowed us to change the result type of the function, notice how we
are unable to change the input type. The `Functor` type class does not allow to
change the *input*, and that's where `Contravariant` comes in. The intuition
behind a `Contravariant` is that it reflects a sort of "consumer of input" that
can have its type adapted.  For a contravariant functor `f`, `f a` represents
some sort of input of type `a`. As with `fmap`, we can use `contramap` with a
function of type `a -> b` to change, we can adapt the input to the type `b`.

```haskell
class Contravariant f where
  contramap :: (b -> a) -> f a -> f b
```

The function arrow is the prototypical example of something contravariant,
although because of the ordering of the type parameters, we have to give it a
`newtype`, which is commonly called `Op` (for `Op`posite):

```haskell
newtype Op z a = Op (a -> z)

instance Contravariant (Op z) where
  contramap h (Op g) = Op (g . h)
```

For example, we can use `contramap` to map over the input of a function that 
calculates the length of a list, to calculate the length of a `Set`:

```haskell
lengther :: Op Int [a]
lengther = Op Prelude.length

setLengther :: Op Int (Set a)
setLengther = contramap Set.toList lengther
```

In Haskell, we like to be able to reason about our code, so most type classes 
come with corresponding *laws*. The laws that the instance is required to
satisfy are dual to the `Functor' laws.  First, let's recall what the `Functor`
laws are:

* `fmap id = id`
* `fmap (f . g) = fmap f . fmap g`

The idea of "dual" laws here means that the order of the `f` and `g` switches
over, whereas in the functor law they keep their relative ordering. Therefore,
the laws for contravariant functors are:

* `contramap id = id`
* `contramap (f . g) = contramap g . contramap f`

A more interesting example of a contravariant functor is an actor that receives
messages of type `a`, performs some action and then listens for further messages
of type `a`, repeating this process indefinitely.  (You'll find a similar
definition to this in the
[`simple-actors`](http://hackage.haskell.org/package/simple-actors) package).

```haskell
data Behavior a = Behavior (a -> IO (Behavior a))

instance Contravariant Behavior where
  contramap f (Behavior r) = Behavior (\a -> fmap (contramap f) (r (f a)))

runBehavior :: Behavior a -> [a] -> IO ()
runBehavior _            []     = return ()
runBehavior (Behavior f) (a:as) = do
  newBehavior <- f a
  runBehavior newBehavior as
```

We can make a `Behavior` that just prints the strings it receives:

```haskell
printer :: Behavior String
printer = Behavior (\s -> putStrLn s >> return printer)

messages :: [String]
messages = [ "Hello", "world", "Haskell", "is", "great" ]

testPrinter = runBehavior printer messages
```

We run our `printer` with `testPrinter :: IO ()`, which gives the following
output:

```
> testPrinter
Hello
world
Haskell
is
great
```

Using `contramap` we can adapt it to a `Behaviour` which shouts:

```haskell
shoutyPrinter :: Behavior String
shoutyPrinter = contramap (\s -> (map toUpper s ++ "!")) printer

testShoutyPrinter = runBehavior shoutyPrinter messages
```

Who's output is now a bit louder:

```
> testShoutyPrinter
HELLO
WORLD
HASKELL
IS
GREAT
```

A more complicated example would be a "mailbox" that listens for messages and
prints out the messages it has received so far. These mailboxes will have a
limit on the amount of messages they can hold, and if attempt to push messages
into a full mailbox, then the message is dropped. We can model this as another
`Behaviour`:

```haskell
makeMailbox :: [(String, String)] -> Behavior (String, String)
makeMailbox messages = Behavior $ \message ->
  if length messages < 3
  then let newMessages = messages ++ [message]
       in do putStrLn "I contain messages:"
             mapM_ printMessage newMessages
             return (makeMailbox newMessages)
  else do putStrLn "I am full."
          putStrLn "I contain messages:"
          mapM_ printMessage messages
          return (makeMailbox messages)
 where
  printMessage (from, message) =
    putStrLn ("From " ++ from ++ ": " ++ message)

mailbox :: Behavior (String, String)
mailbox = makeMailbox []
```

Now we test the mailbox.  There are some messages we don't want to
receive, but luckily the mailbox gets full before they arrive!

```haskell
testMailbox = runBehavior mailbox messages
  where messages = [ ("ocharles", "hackage is great")
                   , ("edwardk", "I love Simon Peyton Jones")
                   , ("spj", "We all love lazy evaluation")
                   , ("Your spouse", "Make me a cup of tea")
                   , ("Your employer", "You must program in Scala") ]
```

If we run this, we'll see that the first 3 messages are delivered, but after
that the messages are dropped:

```
> testMailbox
I contain messages:
From ocharles: hackage is great
I contain messages:
From ocharles: hackage is great
From edwardk: I love Simon Peyton Jones
I contain messages:
From ocharles: hackage is great
From edwardk: I love Simon Peyton Jones
From spj: We all love lazy evaluation
I am full.
I contain messages:
From ocharles: hackage is great
From edwardk: I love Simon Peyton Jones
From spj: We all love lazy evaluation
I am full.
I contain messages:
From ocharles: hackage is great
From edwardk: I love Simon Peyton Jones
From spj: We all love lazy evaluation
```

A mailbox expects to receive a message consisting of a tuple of `(String,
String)` containing the "sender name" and "message body".  As long as we can
provide a function `a -> (String, String)` we can use `contramap` to make a
`Behavior a` from our mailbox.  Here's an example of using the mailbox as a
logger. A log entry is either success carrying an integer, or a failure
message. We can turn this into a message for a mailbox with a simple `logMsg`
function:

```haskell
logMsg :: Either String Int -> (String, String)
logMsg (Right x) = ("Logger daemon", "Everything is OK: " ++ show x)
logMsg (Left s) = ("Logger daemon", "FAILURE: " ++ s)
```

Now we can `contramap` our previous mailbox into a mailbox that accepts log
entries as input:

```haskell
loggerMailbox :: Behavior (Either String Int)
loggerMailbox = contramap logMsg mailbox

testLogger = runBehavior loggerMailbox messages
  where messages = [ Right 24
                   , Left "Oops, there was an error"
                   , Right 1
                   , Right 2 ]
```

A final run of this `testLogger` shows:

```
> testLogger
I contain messages:
From Logger daemon: Everything is OK: 24
I contain messages:
From Logger daemon: Everything is OK: 24
From Logger daemon: FAILURE: Oops, there was an error
I contain messages:
From Logger daemon: Everything is OK: 24
From Logger daemon: FAILURE: Oops, there was an error
From Logger daemon: Everything is OK: 1
I am full.
I contain messages:
From Logger daemon: Everything is OK: 24
From Logger daemon: FAILURE: Oops, there was an error
From Logger daemon: Everything is OK: 1
```

There are not many instances of `Contravariant` on Hackage, although there are
surely many contravariant type parameters, and perhaps this type class could be
used more widely.
