---
title: "Announcing tasty-rerun"
---

If you're a firm believer of writing tests for your work, it shouldn't take much
to convince you that having your tests run fast is of the utmost
importance. Every second the tests are running is a second that you drift out of
the zone and have to context switch when you return back to coding. Sadly, I
don't have a magic solution to make all your tests run faster, but I do have
something to that can help ease the pain, and that is
[`tasty-rerun`](http://hackage.haskell.org/package/tasty-rerun).

Inspired by
[`prove`](https://metacpan.org/pod/release/LEONT/Test-Harness-3.30/bin/prove) --
Perl's standard testing tool -- `tasty-rerun` adds the ability to run your tests
by filtering the test tree based on what happened in a previous test run. For
example, `tasty-rerun` allows you to only run tests that failed when the test
suite was last ran, or you can chose to run only tests that have been added
since the last run. I think this is a massive win for productivity, as the
majority of the time we have the type system to keep our refactorings in check,
and now we can use our tests to focus on the design of specific functionality.

`tasty-rerun` works by providing an *ingredient transformer*. If you're not
familiar with `tasty`,
[ingredients](http://documentup.com/feuerbach/tasty#packages/ingredients) are
used to decide how tests are ran, or how test progress should be observed. For
`tasty-rerun`, we only need to do a little bit of filtering before the test run
and save some state after the test run, so we hand off the bulk of the work to
another ingredient after transforming the test tree. Let's see some code to see
how this works.

## An Example `tasty-rerun` Session

We begin with our test file:

```haskell
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners

main :: IO ()
main = defaultMainWithIngredients [ consoleTestReporter ] tests

tests :: TestTree
tests = testGroup "Sums"
  [ testCase "Addition" $ 1 + 1 @?= 3
  , testCase "Multiplication" $ 2 * 2 @?= 4
  ]
```

To add `tasty-rerun` support to this we simply import
`Test.Tasty.Ingredients.Rerun` and then transform our ingredients
(`[ consoleTestReporter ]`) with `rerunningTests`:

```haskell
import Test.Tasty.Ingredients.Rerun

...

main = defaultMainWithIngredients [ rerunningTests [ consoleTestReporter ] ] tests
```

Simple! Now, when we first run our test suite we supply the `--rerun-update`
flag, which indicates that `tasty-rerun` should save the results of this test
file for use in future sessions.

```
> ./tests --rerun-update
Sums
  Addition:       FAIL
    expected: 3
     but got: 2
  Multiplication: OK

1 out of 2 tests failed
```

We'd like to focus on just this failing addition test, so we now run our tests
with the `--rerun-filter failures` flag. This indicates that `tasty-rerun`
should first filter the test tree to only those tests that failed in a previous
test run (and still exist in the current test tree). With this, another run of
the tests shows:

```
> ./tests --rerun-update --rerun-filter failures
Sums
  Addition: FAIL
    expected: 3
     but got: 2

1 out of 1 tests failed
```

`tasty-rerun` noticed that multiplication passed without problems on the
previous test run, and has filtered it out of this test run. When we fix our
addition test...

```haskell
testCase "Addition" $ 1 + 1 @?= 3
```

Then running with `rerun-filter failures` shows:

```
> ./tests --rerun-update --rerun-filter failures
Sums
  Addition: OK

All 1 tests passed
```

The addition test previously failed, so we try it again. Now it passes, the test
state is updated and a further test run (with `--rerun-filter failures`) shows
that there are no tests to run:

```
> ./tests --rerun-update --rerun-filter failures
All 0 tests passed
```

At this point, we're ready to add another test! We'll add in one final test as a
QuickCheck property:


```haskell
testProperty "Negation involution" $ \x -> negate (negate x) == (x :: Int)
```

If we run our tests as we did before, nothing will happen:

```
> ./tests --rerun-update --rerun-filter failures
All 0 tests passed
```

This is because we told `tasty-rerun` that we are *only* interested in tests
that have previously failed. However, `tasty-rerun` allows the `--rerun-filter`
flag to take multiple filters, so we can change this to `--rerun-filter
failures,new` to run new tests:

```
> ./tests --rerun-update --rerun-filter failures,new
Sums
  Addition:            OK
  Multiplication:      OK
  Negation involution: OK
    +++ OK, passed 100 tests.

All 3 tests passed
```

In this case all the tests are ran because our last test ran no tests at all -
thus *all* the tests are new.

In practice, you probably want to run once with `--rerun-update` to build your
initial test state, and then subsequent runs with only
`--rerun-filter failures,new`. This will allow you to repeatedly run your tests
focusing on only tests that have been newly added or were previously
broken. Updating the state on every test run can be slightly confusing (as the
above example may demonstrate), though I have some ideas on how I might be able
to make this a bit more useful.

I hope you find `tasty-rerun` useful -- it's on Hackage already, so you can start
using it today. As always, please report
[issues on Github](https://github.com/ocharles/tasty-rerun/issues) so we can
make this project even more awesome. Many thanks to
[Roman Cheplyaka](http://ro-che.info) for explaining to me how this project
could be achieved in `tasty`, providing thorough code/style reviews, and of
course for `tasty` itself.
