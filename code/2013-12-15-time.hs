import Data.Time
import System.Locale

main :: IO ()
main = do
  myTime <- getCurrentTime
  putStrLn $ "It's currently: " ++ show myTime

  let fiveHours = 5 * 60 * 60
  let later = fiveHours `addUTCTime` myTime
  putStrLn $ "Five hours later it will be: " ++ show later

  let format = formatTime defaultTimeLocale "%Y/%m/%e %l:%M %P"
  putStrLn $ "Formatted, that is: " ++ format later

  let christmasDay = fromGregorian 2013 12 25
      n = christmasDay `diffDays` utctDay myTime
  putStrLn $ "Only " ++ show n ++ " days to go until Christmas!"
