{-# LANGUAGE DeriveGeneric #-}
import Data.HashMap.Strict
import Data.Hashable
import GHC.Generics

data Child = Child { childName :: String
                   , childLocation :: String
                   } deriving (Eq, Generic, Show)

data Priority = Please | PrettyPlease | PleasePleasePlease
  deriving (Eq, Generic, Show)

data Request = Request { requestPresent :: String
                       , requestPriority :: Priority
                       } deriving (Eq, Generic, Show)

instance Hashable Child
instance Hashable Priority
instance Hashable Request

olliesWishList :: HashMap Child [Request]
olliesWishList = fromList $
  let ollie = Child { childName = "ocharles"
                    , childLocation = "London"
                    }
  in [(ollie, [ Request "Artisan Coffee" Please
              , Request "Dependent Types in Haskell" PleasePleasePlease
              , Request "Lambda Fridge Magnets" PrettyPlease
              ]) ]

main = do
  traverseWithKey showWishList olliesWishList
 where
  showWishList child wants = do
    putStrLn (childName child ++ " wants...")
    mapM_ (putStrLn . requestPresent) wants
