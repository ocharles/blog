{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Control.Applicative
import Data.IORef
import Control.Monad (join)
import Control.Concurrent.MVar
import Data.Map (Map)
import Data.Traversable (for)
import qualified Data.Map as Map
import Data.Functor.Compose
import Data.Foldable

getAllUsers :: IO [Bool]
getAllUsers = undefined
getUserById = undefined
doSomething = undefined

example1 :: IO ()
example1 = do
  userIds <- getAllUsers
  users <- for userIds $ \userId -> do
    getUserById userId

  doSomething users

data ExpandedEntity = ExpandedEntity Bool (Maybe Bool) (Maybe Bool)

getAllEntities :: IO [Bool]
getAllEntities = undefined
getEntityTypeById = undefined
getEntityOwnerById = undefined
entityTypeId :: Bool -> Int
entityTypeId = undefined
entityOwnerId :: Bool -> Int
entityOwnerId = undefined

better :: IO ()
better = do
  entities <- getAllEntities
  expandedEntities <- for entities $ \entity -> do
    entityType <- getEntityTypeById (entityTypeId entity)
    entityOwner <- getEntityOwnerById (entityOwnerId entity)
    return $ ExpandedEntity entity entityType entityOwner
  doSomething expandedEntities

getEntityTypesById = undefined
getEntityOwnersById = undefined

correct = do
  entities <- getAllEntities
  let entityTypeIds = map entityTypeId entities
      entityOwnerIds = map entityOwnerId entities
  entityTypes <- getEntityTypesById entityTypeIds
  entityOwners <- getEntityOwnersById entityOwnerIds
  doSomething $ flip map entities $ \entity ->
   ExpandedEntity entity
                  (entityTypeId entity `lookup` entityTypes)
                  (entityOwnerId entity `lookup` entityOwners)

data Query k v = Query (IORef (Map k [MVar (Maybe v)]))

(Query keys) @? k = do
  result <- newEmptyMVar
  modifyIORef' keys (Map.insertWith (++) k [result])
  return (takeMVar result)

getEntityById = undefined
ohNo :: IO ()
ohNo = do entity <- getEntityById 1
          if entityOwnerId entity `mod` 2 == 0
            then do owner <- getEntityOwnerById (entityOwnerId entity)
                    return (entity, Just owner)
            else return (entity, Nothing)
          return ()

newtype Querying a = Querying { unQuerying :: Compose IO IO a }
  deriving (Functor, Applicative)

(@?!) :: (Ord k, Eq k) => Query k v -> k -> Querying (Maybe v)
(Query keys) @?! k = Querying $ Compose $ do
  result <- newEmptyMVar
  modifyIORef' keys (Map.insertWith (++) k [result])
  return (takeMVar result)

--withQuery :: (Ord k, Eq k) => ([k] -> IO (Map.Map k v)) -> (Query k v -> Querying a) -> Querying a
withQuery runner k = Querying $ Compose $ do
  -- Create a IORef to keep track of requested keys and result MVars
  keysRef <- newIORef Map.empty

  -- Run the first phase of the Querying action
  getResponse <- getCompose $ unQuerying (k (Query keysRef))

  -- Check which keys were requested and perform a query
  keys <- readIORef keysRef
  qResults <- runner (Map.keys keys)

  -- Populate all MVars with results
  flip Map.traverseWithKey keys $ \k mvars ->
    for_ mvars $ \mvar ->
      putMVar mvar (Map.lookup k qResults)

  -- Return the IO action that reads from the MVar
  return getResponse

runQuerying :: Querying a -> IO a
runQuerying (Querying (Compose io)) = join io

getUserAgesById :: [Int] -> IO (Map.Map Int Int)
getUserAgesById keys = do
  putStrLn $ "Looking up " ++ show keys
  return $ Map.fromList $ [(1, 1), (2, 2)]

example :: IO (Maybe Int)
example = runQuerying $
  withQuery getUserAgesById $ \usersAgeById ->
    liftA2 (+) <$> (usersAgeById @?! 1) <*> (usersAgeById @?! 2)
