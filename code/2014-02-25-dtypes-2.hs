{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.Singletons
import Data.Singletons.TH
import Data.Typeable
import Language.Haskell.Interpreter

-- Only used for demonstration purposes
import Data.Dynamic
import Data.Maybe

$(singletons [d|
  data PgType = PgInt | PgString

  data PgFunction = PgReturn PgType | PgArrow PgType PgFunction
  |])

-- These are exactly as before
type family InterpretType (t :: PgType) :: *
type instance InterpretType 'PgInt = Int
type instance InterpretType 'PgString = String

type family InterpretFunction (t :: PgFunction) :: *
type instance InterpretFunction ('PgReturn t) = InterpretType t
type instance InterpretFunction ('PgArrow t ts) = InterpretType t -> InterpretFunction ts

funType :: SPgFunction t -> InterpretFunction t
funType (SPgReturn _) = undefined
funType (SPgArrow _ ts) = \_ -> funType ts

{-

-- This doesn't type check:

firstAttempt :: String -> PgFunction -> IO ()
firstAttempt code sig = withSomeSing sig $ \s -> do
  runInterpreter $ do
    setImports [("Prelude")]
    interpret code (funType s)

  putStrLn "It didn't crash!"

-}

data Dict a where
  Dict :: a => Dict a

pgTypeTypeable :: SPgType t -> Dict (Typeable (InterpretType t))
pgTypeTypeable SPgInt = Dict
pgTypeTypeable SPgString = Dict

pgFunTypeTypeable :: SPgFunction t -> Dict (Typeable (InterpretFunction t))
pgFunTypeTypeable (SPgReturn t) = pgTypeTypeable t
pgFunTypeTypeable (SPgArrow t ts) =
  case pgTypeTypeable t of
    Dict ->
      case pgFunTypeTypeable ts of
        Dict ->
          Dict

goal :: String -> PgFunction -> IO ()
goal code signature = withSomeSing signature $ \s ->
  case pgFunTypeTypeable s of
    Dict -> do
      f <- runInterpreter $ do
          setImports [("Prelude")]
          interpret code (funType s)

      case f of
        Left error ->
          print error

        Right f' ->
          -- Apply arguments to f'
          putStrLn "Everything type checked!"

final :: String -> PgFunction -> [Dynamic] -> IO ()
final code signature args = withSomeSing signature $ \s ->
  case pgFunTypeTypeable s of
    Dict -> do
      f <- runInterpreter $ do
          setImports [("Prelude")]
          interpret code (funType s)

      case f of
        Left error ->
          print error

        Right f' ->
          apply s args f'

  where
  apply :: SPgFunction t -> [Dynamic] -> InterpretFunction t -> IO ()
  apply (SPgReturn SPgInt) _ i = print i
  apply (SPgReturn SPgString) _ i = print i
  apply (SPgArrow t ts) (x : xs) f = case pgTypeTypeable t of
    Dict -> apply ts xs (f (fromJust $ fromDynamic x))
