import qualified Data.MemoCombinators as Memo

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibFast :: Int -> Int
fibFast = Memo.integral fib'
 where fib' 0 = 1
       fib' 1 = 1
       fib' n = fibFast (n - 1) + fibFast (n - 2)
