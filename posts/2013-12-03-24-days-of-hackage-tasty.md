---
title: 24 Days of Hackage: tasty
---

Last year, when discussing
[QuickCheck](/posts/2012-12-08-24-days-of-hackage.html), I said:

> While we try and constrain our types as much as possible, there is always a
> trade off between exact types and pragmatism, not to mention that there are
> some invariants that are very difficult to encode in the Haskell type
> system. As such, without rigorous testing, there is still a risk of exceptions
> or unexpected behaviors at runtime.

The necessessity of testing is just as essential today. Last year we looked at a
specific testing techinque, namely the use of `QuickCheck`, but we didn't look
at testing from the bigger perspective: how do you formulate entire test suites?
Hackage has a lot of options available to us here - including `test-framework`,
`hspec`, the ellusive `detailed-1.0` `test-suite` setting of Cabal, and the
newest entry to field: [`tasty`](http://documentup.com/feuerbach/tasty).

For a long time, I was perfectly content with `test-framework`. However, as time
has gone on, `test-framework` has failed to stay up to date. The Github
repository doesn't show much activity and is accumulating pull requests, and
it's said that the codebase itself can make it difficult to make
modifications. This is not meant to be critiscism of Max - these things
happen. For these reasons, and a few others,
[Roman Cheplyaka](http://ro-che.info/) created his own testing framework -
`tasty`, which is now my testing framework of choice.

There are two main parts to `tasty`: test trees and ingredients.

Test trees specify the hierarchy of tests. You can specify groups of tests using
`testGroup`, or you can create individual tests using `TestTree` builders for
specific testing tools. `tasty-hunit` gives us the `testCase` builder, while
`tasty-smallcheck` gives us `testProperty`.

To illustrate the formulation of a test tree, allow me to reproduce a subset of
the example in `tasty`s documentation:

```haskell
main :: IO ()
main = defaultMain $
  testGroup "Tests"
    [ testGroup "(checked by SmallCheck)"
        [ testProperty "sort == sort . reverse" $
            \list -> sort (list :: [Int]) == sort (reverse list)

        , testProperty "Fermat's last theorem" $
            \x y z n -> (n :: Integer) >= 3 ==>
              x^n + y^n /= (z^n :: Integer)
        ]

    , testGroup "Unit tests"
        [ testCase "List comparison (different length)" $
            [1, 2, 3] `compare` [1,2] @?= GT

        , testCase "List comparison (same length)" $
            [1, 2, 3] `compare` [1,2,2] @?= LT
        ]
    ]
```

As you can see, it's both easy and consistent to form a test tree using
different testing tools. This helps encourage us to use the right testing tool
for the job. Above we see the use of both SmallCheck and HUnit.

Once you have a `TestTree`, you presumably want to do something with it - such
as actually running the tests! That's what *ingredients* are all
about. Ingredients are small units of functionality that have the ability to
parse command line options, and can conditionally choose to run a
`TestTree`. `tasty` itself ships with an ingredient to run the tests and output
pretty ANSI-coloured terminal output, and also an ingredient that simply lists
the names of all tests.

Ingredients provide an extension point for `tasty`, which is one area where this
framework trumps the competition. As a case in point, I wanted the ability to
run tests on our [Jenkins](http://jenkins-ci.org/) continuous integration
server - which expects test runs to produce an XML file in a specific
schema. `test-framework` ships with this baked right into the library itself,
but for `tasty` I was able to write my own ingredient that observes a test run
and renders XML as it goes. And to top it off, it took me little more than 100
lines of code and a few hours of hacking (the result is
[`tasty-ant-xml`](http://hackage.haskell.org/package/tasty-ant-xml)).

`tasty`'s interpretation of test trees is also a massive productivity win when
we're developing, as we can run specific parts of the tree by using *test
patterns*. Test patterns let us run only test cases who's name match a specific
pattern, with the ability to match the test hierarchy too. For example, if
you're tweaking the serialisation format of a library you might run with
`--pattern 'Serialization/**'` to avoid the longer IO tests. Or maybe you have
added a new PostgreSQL database backend, and are only interested in tests that
refer to that, so you would use `--patern **/*PostgreSQL*/**`.

Roman has already done a lot of work getting `tasty` usable with the other big
testing tools. Specifically, right now the tasty suite (menu?) consists of:

* [`tasty-hunit`](http://hackage.haskell.org/package/tasty-hunit) — for unit
  tests (based on HUnit)
* [`tasty-golden`](http://hackage.haskell.org/package/tasty-golden) — for golden
  tests, which are unit tests whose results are kept in files
* [`tasty-smallcheck`](http://hackage.haskell.org/package/tasty-smallcheck) —
  exhaustive property-based testing (based on smallcheck)
* [`tasty-quickcheck`](http://hackage.haskell.org/package/tasty-quickcheck) —
  for randomized property-based testing (based on QuickCheck)
* [`tasty-hspec`](http://hackage.haskell.org/package/tasty-hspec) — for Hspec
  tests
* [`tasty-ant-xml`](http://hackage.haskell.org/package/tasty-ant-xml) — to run
  tests on Jenkins

Code for today's example can be found in
[my blog's Github repository](https://github.com/ocharles/blog/tree/master/code).
