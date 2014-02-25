{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
data PgType = PgInt | PgString

type family InterpretType (t :: PgType) :: *
type instance InterpretType 'PgInt = Int
type instance InterpretType 'PgString = String

data SPgType :: PgType -> * where
  SPgInt :: SPgType 'PgInt
  SPgString :: SPgType 'PgString

exampleValues :: SPgType t -> InterpretType t
exampleValues SPgInt = 42
exampleValues SPgString = "Forty two"

data PgFunction
  = PgReturn PgType
  | PgArrow PgType PgFunction

type family InterpretFunction (t :: PgFunction) :: *
type instance InterpretFunction ('PgReturn t) = InterpretType t
type instance InterpretFunction ('PgArrow t ts) = InterpretType t -> InterpretFunction ts

data SPgFunction :: PgFunction -> * where
  SPgReturn :: SPgType t -> SPgFunction ('PgReturn t)
  SPgArrow :: SPgType t -> SPgFunction ts -> SPgFunction ('PgArrow t ts)

exampleFunctions :: SPgFunction t -> InterpretFunction t
exampleFunctions (SPgReturn SPgInt) = 42
exampleFunctions (SPgArrow SPgInt (SPgReturn SPgInt)) = \x -> x * x
exampleFunctions (SPgArrow SPgString (SPgReturn SPgInt)) = length
