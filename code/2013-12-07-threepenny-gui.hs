import Control.Monad (void)
import Control.Concurrent.STM
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

type Database = Map UserName ToDoList
type UserName = String
type ToDoList = Set String

main :: IO ()
main = do
  database <- atomically $ newTVar (Map.empty)
  startGUI defaultConfig (setup database)

setup :: TVar Database -> Window -> UI ()
setup database rootWindow = void $ do
  userNameInput <- UI.input # set (attr "placeholder") "User name"
  loginButton <- UI.button #+ [ string "Login" ]
  getBody rootWindow #+
    map element [ userNameInput, loginButton ]

  on UI.click loginButton $ \_ -> do
    userName <- get value userNameInput

    currentItems <- fmap Set.toList $ liftIO $ atomically $ do
      db <- readTVar database
      case Map.lookup userName db of
        Nothing -> do
           writeTVar database (Map.insert userName Set.empty db)
           return Set.empty

        Just items -> return items

    let showItem item = UI.li #+ [ string item ]
    toDoContainer <- UI.ul #+ map showItem currentItems
      
    newItem <- UI.input

    on UI.sendValue newItem $ \input -> do
      liftIO $ atomically $ modifyTVar database $
        Map.adjust (Set.insert input) userName

      set UI.value "" (element newItem)
      element toDoContainer #+ [ showItem input ]

    header <- UI.h1 #+ [ string $ userName ++ "'s To-Do List" ]
    set children
      [ header, toDoContainer, newItem ]
      (getBody rootWindow)
