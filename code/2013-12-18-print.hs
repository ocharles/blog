module Printer where

{-|
>>> :{
      let callback name = do
            putStrLn $ "Hello. Yes, this is " ++ name
>>> :}

>>> printer "Dog" callback
Dog says:
Hello. Yes, this is Dog
-}
printer :: String -> (String -> IO ()) -> IO ()
printer name callBack = do
  putStrLn $ name ++ " says:"
