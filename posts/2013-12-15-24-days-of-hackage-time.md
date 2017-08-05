---
title: "24 Days of Hackage: time"
---

If there's one task I don't have time for these days, it's dealing with
time. Time is a classic example of a problem that *appears* simple on the
outside, but is
[full of all sorts of complicated details](http://infiniteundo.com/post/25326999628/falsehoods-programmers-believe-about-time)
when you really start working with it. Naturally, these problems only manifest
themselves years down the line, when you've already committed to a broken
implementation. Considering all of that, I want as little as possible to do with
time, so I turn to a library rather than doing it myself. Today, we'll have a
look at [Ashley Yakeley's](http://semantic.org/)
[`time`](http://hackage.haskell.org/package/time) library.

`time` does a great job of abstracting away the aforementioned tricky details
into a powerful, type safe library. There are four main namespaces within this
library:

* `Data.Time.Calendar` deals with days in a calendar, and arithmetic on calendar
  dates. There is the common Gregorian calendar that we're all familiar with,
  the [Julian calendar](http://en.wikipedia.org/wiki/Julian_calendar) that
  predated this, and a few other modules for other ways of managing dates
  (especially ISO 8601).

* `Data.Time.Clock` deals with times in UT1, which is (loosely speaking) time
  measured by the Earth, "adjusted for various wobbles". This is the time that
  we as humans are most familiar with. This module exposes constructors for
  building time values, along with functions for time arithmetic. By default,
  the module at the top of this namespace ignores leap seconds, but if you do
  need this, there is always `Data.Time.Clock.TAI` which allows working with
  leap seconds.

* `Data.Time.Format` exposes an API to the standard `strftime`-like interface
  for reading and formatting times and dates.

* `Date.Time.LocalTime` introduces some types for working with time zones, and
  converting between the types offered by `time`.

Not only does `time` come with all of these data types and functions, it
provides instances for a lot of the type classes offered by the Haskell
prelude - and this can often be one of the hardest things to understand when
first approaching this library. Let's take stock of the most important type
classes, and see how `time` uses them:

* `Enum` gives us the ability to enumerate values, and this is what Haskell uses
  to desugar `[a..b]`. We can enumerate days in the Gregorian calendar (which
  lets us ask for the day before or after a given day), and we can enumerate
  durations of time (where the successor of a duration is one picosecond).

* `Num` gives us the ability to add, subtract and multiply values, and also
  gives us the ability to convert literal numbers in source code to other data
  types. This is very useful when working with intervals of time - we can say
  `5 :: NominalDiffTime` to construct a value that represents a 5 second interval.

* `Fractional` is for dealing with fractional values, and let us interpret float
  point literals. For example, if we want a value that represents half a second,
  we can use `0.5 :: NominalDiffTime`.

* `Real` allows converting to a `Rational` number, and `RealFrac` allow us to
  round values into an `Integral` value. A lot of the data types in `time` don't
  have `Integral instances`, but we can use `fromIntegral` to convert into the
  `Num` type class, which we do have instances of.

That's a lot to take in, so lets see how this all plays out in practice. First
of all, lets take our current time as a starting point. There are two ways of
thinking about "current time" - our time as we see it on our own clocks (the
time in our time zone), or the current time in UTC. For me, my timezone is
currently UTC anyway, so we'll work with the current UTC time:

```haskell
myTime <- getCurrentTime
putStrLn $ "It's currently: " ++ show myTime
```

Next, we'd like to answer the question: what will the time be 5 hours from now?
We already have a value to represent "now" (of type `UTCTime`), so we need a way
to represent "5 hours" and a way to combine the two. "5 hours" is a time
interval, so we can use `NominalDiffTime` for this. However, looking at the
library, it doesn't seem possible to create these values! This is where the type
classes come into play - remember how we said that `Num` lets use treat literals
as time data types? The `Num` instance for `NominalDiffTime` is used for
converting from an amount of seconds, so with a little arithmetic we can
represent 5 hours:

```haskell
let fiveHours = 5 * 60 * 60
```

To offset a `UTCTime` with a `NominalDiffTime` we can't use the normal `(+)`
operator, because this is only supported for values of the same type[^1]. `time`
exports the `addUTCTime :: NominalDiffTime -> UTCTime -> UTCTime`, which is
exactly what we want:

```haskell
let later = fiveHours `addUTCTime` myTime
putStrLn $ "Five hours later it will be: " ++ show later
```

Now we see:

```
It's currently: 2013-12-15 14:03:18.095702 UTC
Five hours later it will be: 2013-12-15 19:03:18.095702 UTC
```

Perfect! The default show instance is a little bit complex though, so lets see
if we can format this a bit better. I'd like to see a much more British time,
preferably "15/12/2013 7:03 PM". This is a breeze with `Data.Time.Format`:

```haskell
let format = formatTime defaultTimeLocale "%Y/%m/%e %l:%M %P"
putStrLn $ "Formatted, that is: " ++ format later
```

Now, the output is:

```haskell
It's currently: 2013-12-15 14:07:23.964489 UTC
Five hours later it will be: 2013-12-15 19:07:23.964489 UTC
Formatted, that is: 2013/12/15  7:07 pm
```

Finally, how about finding how many days we have to wait until Santa comes? We
could parse this as a `UTCTime`, but we really only care about calendar days. To
turn 2013/12/25 into a date, we use `fromGregorian`:

```haskell
let christmasDay = fromGregorian 2013 12 25
```

To turn the current time into the calendar day, we can use `utctDay` to extract
the day. To find out the difference of two days, we can't use `(-)` for similar
reasons, but we do have `diffDays` out our disposal:

```haskell
let n = christmasDay `diffDays` utctDay myTime
putStrLn $ "Only " ++ show n ++ " days to go until Christmas!"
```

And a final run of our application shows:

```haskell
It's currently: 2013-12-15 14:12:20.841326 UTC
Five hours later it will be: 2013-12-15 19:12:20.841326 UTC
Formatted, that is: 2013/12/15  7:12 pm
Only 10 days to go until Christmas!
```

Which puts me into blind panic as I realise... I still haven't got my family
their gifts!

[^1]: Curious readers might be left frustrated that we don't have a general
abstraction here, but it turns out there is a mathematical abstraction behind
this: [torsors](http://math.ucr.edu/home/baez/torsors.html). Roman Cheplyaka has
[blogged about torsors and dates](http://ro-che.info/articles/2013-01-08-torsors.html),
which Haskell programmers may find more accessible.
