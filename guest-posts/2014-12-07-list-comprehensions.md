---
title: 24 Days of GHC Extensions: List Comprehensions
---

After doing a fantastic job explaining [rebindable syntax](/guest-posts/2014-12-06-rebindable-syntax.html) to us yesterday, [Benjamin Kovach](http://kovach.me) has a second post for us today. This time, we're again going to look at an extension to re-purpose existing Haskell syntax. Ben, it's over to you!

---

```haskell
{-# LANGUAGE ParallelListComp, TransformListComp, MonadComprehensions, RecordWildCards #-}

import GHC.Exts
import qualified Data.Map as M
import Data.Ord (comparing)
```

List Comprehensions are sparsely used in Haskell. Often we opt to instead use `Applicative`s or `Monad`s with `do` notation to construct lists instead. Let's make them better with some GHC extensions!

## ParallelListComp

Let's look at a simple, normal list comprehension to start:

```haskell
regularListComp :: [Int]
regularListComp = [ x + y * z
                  | x <- [0..10]
                  , y <- [10..20]
                  , z <- [20..30]
                  ]
```

This takes the sum of each element of `x` paired with *each* element of `y` and of `z` and collects the results. Another useful way to process one ore more lists together is to *zip* them and process each tuple. This is what `ParallelListComprehensions` gives us: a list comprehension-like syntax that allows us to process lists *in parallel*, as if the lists were zipped together and then processed.

```haskell
parallelListComp :: Int
parallelListComp = [ x + y * z
                   | x <- [0..10]
                   | y <- [10..20]
                   | z <- [20..30]
                   ]
```

This will produce the expression:

```haskell
zipWith3 (\(x, y, z) -> x + y * z) [0..10] [10..20] [20..30]
```

but in a more readable way.

The first example of `zip` I always think of is the canonical `fibonacci` list generating function `fibs`:

```haskell
fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
```

It's a really interesting bit of Haskell code that I implore you to investigate if you haven't! If you look closely, we're doing a parallel list comprehension here, it's just recursive. Rewritten with our extension, we get:

```haskell
fibs :: [Int]
fibs = 0 : 1 : [ x + y
               | x <- fibs
               | y <- tail fibs
               ]
```

Which is pretty neat! Further, this is easily extensible. Say we want to express a similar recurrence relation like this:

```haskell
fiblikes :: [Int]
fiblikes = 0 : 1 : [ x + y + z
                   | x <- fibs
                   | y <- tail fibs
                   | z <- tail (tail fibs)
                   ]
```

The function this generates looks pretty ugly compared to `fibs`, but written with `ParallelListComprehension` syntax, we get something nicer.

## TransformListComp

`TransformListComp` gives us something really powerful and really strange: an SQL-like syntax to process lists as if they were database tables. Let's construct a simple "table" that we can use to run queries on.

```haskell
data Character = Character
  { firstName :: String
  , lastName :: String
  , birthYear :: Int
  } deriving (Show, Eq)

friends :: [Character]
friends = [ Character "Phoebe" "Buffay" 1963
          , Character "Chandler" "Bing" 1969
          , Character "Rachel" "Green" 1969
          , Character "Joey" "Tribbiani" 1967
          , Character "Monica" "Geller" 1964
          , Character "Ross" "Geller" 1966
          ]
```

We can use the fields of `Character` as we would columns of a database table and process them using  `TransformListComprehensions`. Let's say we want to collect the names the oldest `k` `friend`s from our group. Here's a function that will do just that (note the use of `RecordWildCards`!):

```haskell
oldest :: Int -> [Character] -> [String]
oldest k tbl = [ firstName ++ " " ++ lastName
               | Character{..} <- tbl
               , then sortWith by birthYear
               , then take k
               ]
```

Perhaps we also want to know in which year the most `friend`s were born, and who they are:

```haskell
groupByLargest :: Ord b => (a -> b) -> [a] -> [[a]]
groupByLargest f = sortBy (comparing (negate . length)) . groupWith f

bestBirthYears :: [Character] -> [(Int, [String])]
bestBirthYears tbl = [ (the birthYear, firstName)
                     | Character{..} <- tbl
                     , then group by birthYear using groupByLargest
                     ]
```

It's kind of wacky, but it works! First we pull out all of the `Character`s, then we group them together by their birth years. We then sort these grouped lists by their negative length (to get the largest first) and finally return the sorted list of most popular birth years, paired up with the first names of the friends born in those years.

This is only the tip of the iceberg when it comes to `TransformListComp`; for a more in-depth explanation of how everything works, [check out the docs](https://downloads.haskell.org/~ghc/7.8.3/docs/html/users_guide/syntax-extns.html#generalised-list-comprehensions). Onwards!

## MonadComprehensions

Recall that the list comprehension:

```haskell
[(a, b) | a <- xs, b <- ys]
```

desugars to:

```haskell
do a <- xs
   b <- ys
   return (a, b)
```

Only, we're constrained to the `[]` monad when using list comprehensions. Why not work on an arbitrary monad? That's exactly what `MonadComprehensions` allows us to do.

Consider a map of squares to their square roots built using a parallel list comprehension from before:

```haskell
sqrts :: M.Map Int Int
sqrts = M.fromList $ [ (x, sx)
                     | x  <- map (^2) [1..100]
                     | sx <- [1..100]
                     ]
```

One of the most common things to do with a `Map` is to `lookup` a value inside it. In this case, we might want to know if a number is a perfect square, and if so, spit out its square root. Let's say we want to add two numbers together *only if they're both perfect squares*. We could do this using the `Monad` instance for `Maybe`, and put it into a list comprehension using `MonadComprehensions`:

```haskell
sumIntSqrts :: Int -> Int -> Maybe Int
sumIntSqrts a b = [ x + y
                  | x <- M.lookup a sqrts
                  , y <- M.lookup b sqrts
                  ]
```

This is effectively just a different syntax for `do` notation, since we're no longer constrained to only lists. For instance, we can even use `IO` in monad comprehensions:

```haskell
greet :: IO String
greet = [ name
        | name <- getLine
        , _ <- putStrLn $ unwords ["Hello, ", name, "!"]
        ]
```

It should be noted that `MonadComprehensions` generalize the both `TransformListComp` (guards in comprehensions are translated into the `guard` function if your monad is a `MonadPlus`) and `ParallelListComp` (parallel statements are translated into `mzip` expressions). You can read about the actual transformations that take place [here](https://downloads.haskell.org/~ghc/7.8.3/docs/html/users_guide/syntax-extns.html#monad-comprehensions).

----

*This post is part of
[24 Days of GHC Extensions](/pages/2014-12-01-24-days-of-ghc-extensions.html) -
for more posts like this, check out the
[calendar](/pages/2014-12-01-24-days-of-ghc-extensions.html)*.
