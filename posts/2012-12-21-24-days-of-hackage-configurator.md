---
title: 24 Days of Hackage: configurator
---

These days, most users expect the ability to configure applications, and to
accomodate most users they also expect something flexible. However, as a
developer, dealing with configuration formats can be teadious. I'd rather work
on adding cool features, not parsing configurations! Luckily, Haskell already
has a solution for you, and it's only a `cabal install` away.

[Bryan O'Sullivan](http://serpentine.com) created the
[`configurator`](http://hackage.haskell.org/package/configurator) library, which
makes dealing with configuration files a breeze. Before we look at some
Haskell code, lets first take a look at how the configuration files look:

```
database
{
  username = "ocharles"
  database = "days_of_hackage"
}

import "$(HOME)/.apprc"
```

`configurator` files have bindings, nested configuration sections, and the
ability to import other files. This means that we have a nice predictable,
simple syntax, which scales extremely well to complex packaging policies that
require multiple files to be aggregated together.

Now that we've seen how the files are formatted, lets take a look at the Haskell
code required to consume them:

```haskell
config <- load [ Required "app.cfg" ]
user <- lookup config "database.username"
```

Simple! `load` takes a list of files, which can either be `Required` or
`Optional`, and will yield an aggregated configuration file. We then use
`lookup` to try and lookup configuration options. If the specified configuration
key can't be found, this will return `Nothing`. There is also `lookupDefault`,
which can make it easier than dealing with values inside `Maybe`.

This alone is enough for a lot of applications... but this is Haskell, so
there's even more cool stuff!

`configurator` supports loading files with the ability to *subscribe* to
changes. This is ideal for server software and other long running applications,
which need to remain running even while operating conditions change. This does
require a little bit more thought when writing your application, but the hot
reloading part comes free with `configurator`:

```haskell
cfg <- autoReload autoConfig [ Required "app.cfg" ]
```

Now `configurator` will watch (in a separate thread) for changes to `app.cfg`
and our `cfg` object will automatically update. Will this may appear to violate
a little bit of referential transparency, I find it acceptable as we still need
to read values out in the IO monad.

Finally, lets have a look at subscribing to changes to certain configuration
options. One thing I love about the PostgreSQL database server is how it informs
me about changes to configurations, so lets see how we could do this:

```haskell
subscribe config (prefix "database") onDbChange
  where
    onDbChange key newVal =
      putStrLn (key ++ " has been changed to: " ++ show newVal)
```

`configurator` follows the trend of offering a really simple API that you can
get results out of immediately, but also giving you something that happens to
scale extremely well to more complicated applications. If you're packaging
applications for end-users, I highly recommend you explore the options
`configurator` gives you. After all, you might as well let someone else deal
with this for you!
