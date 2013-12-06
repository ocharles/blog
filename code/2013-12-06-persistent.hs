{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Data.Text
import Data.Time
import Database.Esqueleto
import Database.Persist hiding ((==.))
import Database.Persist.Sqlite (runSqlite)
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
  name Text
  deriving Show

StatusUpdate
  producer PersonId
  update Text
  createdAt UTCTime
  mood Text Maybe
  deriving Show

FriendOf
  socialite PersonId
  friend PersonId
  deriving Show
|]

main :: IO ()
main = runSqlite ":memory:" $ do
  runMigration migrateAll
  ollie <- insert (Person "Oliver Charles")
  now <- liftIO getCurrentTime
  insertMany
    [ StatusUpdate ollie "Writing another blog post!" now Nothing ]

  sortedNames >>= mapM_ (liftIO . print)

  latestUpdates >>= mapM_ (liftIO . print) 

  return ()

sortedNames =
  select $
  from $ \person -> do
  orderBy [asc (person ^. PersonName)]
  limit 5
  return $ person ^. PersonName

latestUpdates =
  select $
  from $ \(person `InnerJoin` update) -> do
  on (person ^. PersonId ==. update ^. StatusUpdateProducer)
  orderBy [asc (update ^. StatusUpdateCreatedAt)]
  limit 5
  return (person ^. PersonName, update ^. StatusUpdateUpdate)
