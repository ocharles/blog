{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Applicative
import Control.Monad.Reader (ask)
import Control.Monad.State (modify)
import Data.Acid
import Data.Function
import Data.IntMap (IntMap)
import Data.List
import Data.Ord
import Data.SafeCopy
import Data.Typeable
import Data.Time

import qualified Data.IntMap as IntMap

data Failure = Failure { failureReason :: String
                       , failureTime :: UTCTime
                       } deriving (Show, Typeable)

data FailureDb = FailureDb { allFailures :: IntMap Failure }
  deriving (Typeable)

failuresOverTime :: Query FailureDb [Failure]
failuresOverTime =
  sortBy (comparing failureTime) . IntMap.elems . allFailures <$> ask

addFailure :: Failure -> Update FailureDb ()
addFailure failure = modify go
 where
  go (FailureDb db) = FailureDb $
    case IntMap.maxViewWithKey db of
      Just ((max, _), _) -> IntMap.insert (max + 1) failure db
      Nothing            -> IntMap.singleton 1 failure

deriveSafeCopy 0 'base ''Failure
deriveSafeCopy 0 'base ''FailureDb
makeAcidic ''FailureDb ['failuresOverTime, 'addFailure]

main :: IO ()
main = do
  state <- openLocalState (FailureDb IntMap.empty)

  -- Record a new failure
  now <- getCurrentTime
  update state (AddFailure $ Failure "ENOMISSLES" now)

  -- Query for all failures
  allFailures <- query state FailuresOverTime
  
  mapM_ print allFailures
