---
title: 24 Days of Hackage: optparse-applicative
---

We've already seen the `Applicative` type class pop up in various places through
this series of blog posts -
[parsing](posts/2012-12-10-24-days-of-hackage-parsec.html),
[form validation](http://ocharles.org.uk/blog/posts/2012-12-02-digestive-functors.html),
[JSON serialisation](http://ocharles.org.uk/blog/posts/2012-12-07-24-days-of-hackage-aeson.html),
not to mention all sorts of convenience uses in the IO monad. Today, we'll
continue to see just how versatile this type class is as we take a look at
`optparse-applicative`.

[`optparse-applicative`](http://hackage.haskell.org/package/optparse-applicative),
written by [Well Typed](http://well-typed.com)'s
[Paolo Capriotti](http://paolocapriotti.com/), is a library for parsing command
line arguments. This is a task that we usually want to do with as a little
typing as possible, but also as robustly as possible. I think
`optparse-applicative` fills this tricky requirement very nicely.

`optparse-applicative` makes use of a few `Applicative` functors, and also makes
good use `Monoid`s to add meta-data to arguments. Let's look at what this
practically means, with a little example:

```haskell
      data MyApp = MyApp { appGreet :: String }

runWithOptions :: MyApp -> IO ()
runWithOptions opts =
  putStrLn ("Merry Christmas, " ++ appGreet opts ++ "!")

main :: IO ()
main = execParser opts >>= runWithOptions
  where
    parser = MyApp <$> argument str (metavar "NAME")
    opts = info parser mempty
```

First of all, we created a data type to hold the arguments to our program. In
this case, our program will take one option - the name of the person to
greet. After that, I've gone on to write the program itself - nothing too
interesting there. Finally, in `main` we see `optparse-applicative` doing its
thing.

The `parser` is doing the bulk of the work - we use the `Parser` `Applicative`
functor to parse command line arguments into our `MyApp` data type. I've used
the `argument str (metavar "NAME")` parser to indicate that I want a positional
argument, using the `str` parser so it'll accept any input, and I've used
`metavar "NAME"` to inform `optparse-applicative` that this argument should be
referred to as NAME when it generates usage information. Now we have a binary
that does the following:

```
      > ./optparse
Usage: optparse NAME

> ./optparse Haskell
Merry Christmas, Haskell!
```

Perfect! We didn't even have to write that code to generate the usage
information, that came for free! We've seen how positional arguments work, what
about options? Let's add a switch to add more excitement to the application:

```haskell
      data MyApp = MyApp { appGreet :: String
                   , appSuperExcited :: Bool
                   }

runWithOptions :: MyApp -> IO ()
runWithOptions opts =
  putStrLn $ transform $
    "Merry Christmas, " ++ appGreet opts ++ "!"

  where
    transform = if appSuperExcited opts then map toUpper else id

main :: IO ()
main = execParser opts >>= runWithOptions
  where
    parser = MyApp <$> argument str (metavar "NAME")
                   <*> switch (short 'e' <>
                               long "excited" <>
                               help "Run in excited mode")
    opts = info parser mempty
```

We've introduced a new field to our `MyApp` data type, modified our application
(in `runWithOptions`), and extended our `parser` with the `switch` option. Here,
I've attached a good bit of meta-data to the option - the 'short' form is `-e`,
the long form is `--excited`, and I've also explained what the option does. The
`<>` thing is simply `mappend` for `Monoid`s - in layman's terms, it means
'combine these 2 small things into a bigger thing'. In this case, the small
things are the individual bits of meta-data and the bigger thing is the final
meta-data for the option. Again, running our application:

```
      > ./optparse
Usage: optparse NAME [-e|--excited]

Available options:
  -e,--excited             Run in excited mode

> ./optparse "Functional Programmers" --excited
MERRY CHRISTMAS, FUNCTIONAL PROGRAMMERS!
```

Perfect! As you can see, quoted arguments work just fine, and our option does
exactly what we'd expect. That's almost all there is to `optparse-applicative` -
because it's so straightforward. Options have a bit more meta-data that can be
attached to them, and you can also perform more validation/transformation at
parse time - such as reading a string into a number or a
date. `optparse-applicative` also has the ability to have 'commands' - which
lets you build Git-like applications.

If you haven't yet moved to writing your command line applications with Haskell,
you now have one more reason to give it a try! `optparse-applicative` has
comprehensive documentation, including a nice introduction on the Haskell page,
and also another overview in the
[readme](https://github.com/pcapriotti/optparse-applicative) on GitHub. I love
`optparse-applicative`, and I hope you'll love it too.
