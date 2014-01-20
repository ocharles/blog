import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.Runners
import Test.Tasty.Ingredients.Rerun

main :: IO ()
main = defaultMainWithIngredients [ rerunningTests [ consoleTestReporter ] ] tests

tests :: TestTree
tests = testGroup "Sums"
  [ testCase "Addition" $ 1 + 1 @?= 3
  , testCase "Multiplication" $ 2 * 2 @?= 4
  , testProperty "Negation involution" $ \x -> negate (negate x) == (x :: Int)
  ]
