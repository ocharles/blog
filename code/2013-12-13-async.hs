import Control.Concurrent.Async
import Control.Concurrent

type Child = String

naughtyChildrenAround :: String -> Int -> IO [Child]
naughtyChildrenAround _ _ = do
  threadDelay 1000000
  return [ "Ben", "Renzo" ]

waitExample :: IO ()
waitExample = do
  putStrLn "Requesting a list of naughty children"
  search <- async (naughtyChildrenAround "London" 1)
  putStrLn "Searching!"
  searchResults <- wait search
  print searchResults

pollExample :: IO ()
pollExample = do
  search <- async (naughtyChildrenAround "London" 1)
  let loop = do
        maybeResults <- poll search
        case maybeResults of
          Nothing -> do
            putStrLn "Still searching..."
            threadDelay 100000
            loop
          Just r -> return r
  loop >>= print
