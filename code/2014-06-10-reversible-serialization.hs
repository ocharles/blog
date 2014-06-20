{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeOperators, ConstraintKinds, TypeFamilies, StandaloneDeriving, UndecidableInstances, FunctionalDependencies, RankNTypes, TemplateHaskell, FlexibleInstances #-}
import Prelude hiding ((.), id)

import Control.Lens
import Control.Lens.TH
import GHC.Exts
import Control.Category (Category(..))
import Control.Monad ((>=>), void, mplus, mzero, guard)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Monoid

--------------------------------------------------------------------------------
-- type Serializer a = (Get a, a -> Put)

-- string :: Serializer String
-- string = (get, put)


--------------------------------------------------------------------------------
-- data Serializing a b = Serializing (a -> PutM b)

-- pString2 :: Serializing (String, String) String
-- pString2 = Serializing $ \(a, b) -> do put a; return b

-- pString1 :: Serializing String ()
-- pString1 = Serializing $ \a -> put a

-- (.) :: Serializing b c -> Serializing a b -> Serializing a c
-- (Serializing g) . (Serializing f) = Serializing (f >=> g)

-- putTwoStrings :: Serializing (String, String) ()
-- putTwoStrings = pString1 . pString2


--------------------------------------------------------------------------------
-- data Deserializing a b = Deserializing (a -> Get b)

-- getString1 :: Deserializing () String
-- getString1 = Deserializing $ \() -> get

-- getString2 :: Deserializing String (String, String)
-- getString2 = Deserializing $ \s -> do { s' <- get; return (s, s') }

-- (.) :: Deserializing b c -> Deserializing a b -> Deserializing a c
-- (Deserializing g) . (Deserializing f) = Deserializing (f >=> g)

-- getTwoStrings :: Deserializing () (String, String)
-- getTwoStrings = getString2 . getString1


--------------------------------------------------------------------------------
-- data Serializer a b = Serializer (a -> Get b) (b -> PutM a)

-- string2 :: Serializer String (String, String)
-- string2 = Serializer (\s -> do { s' <- get; return (s', s) })
--                      (\(a, b) -> do put a; return b)

-- string1 :: Serializer () String
-- string1 = Serializer (\() -> get) put

-- (.) :: Serializer b c -> Serializer a b -> Serializer a c
-- (Serializer g g') . (Serializer f f') = Serializer (f >=> g) (g' >=> f')

-- twoStrings :: Serializer () (String, String)
-- twoStrings = string2 . string1


--------------------------------------------------------------------------------
-- data Serializer a b = Serializer (Get (a -> b)) (b -> PutM a)

-- instance Category Serializer where
--   (Serializer g g') . (Serializer f f') =
--     Serializer (do buildF <- g
--                    buildG <- f
--                    return (buildF . buildG))
--                (g' >=> f')

--   id = Serializer (return id) return

--------------------------------------------------------------------------------
data List :: [*] -> * where
  Nil :: List '[]
  Cons :: a -> List as -> List (a ': as)

type family AllShow (as :: [*]) :: Constraint where
  AllShow '[] = ()
  AllShow (a ': as) = (Show a, AllShow as)

deriving instance AllShow as => Show (List as)

-- data Serializer :: [*] -> [*] -> * where
--   Serializer :: (Get (List a -> List b))
--              -> (List b -> PutM (List a))
--              -> Serializer a b

-- instance Category Serializer where
--   (Serializer g g') . (Serializer f f') =
--     Serializer (do mkB <- g
--                    mkA <- f
--                    return (mkB . mkA))
--                (g' >=> f')

--   id = Serializer (return id) return

-- string :: Serializer as (String ': as)
-- string = Serializer (do a <- get; return (Cons a))
--                     (\(Cons a as) -> do put a; return as)

-- twoStrings :: Serializer as (String ': String ': as)
-- twoStrings = string . string


--------------------------------------------------------------------------------
class ListIso a b | a -> b, b -> a where
  _HList :: Iso' b (List a)

-- usePrism :: ListIso a b  => Prism' d b -> Serializer a '[d]
-- usePrism p = Serializer get put
--   where
--   put (Cons d Nil) = do
--     Just tuple <- return (preview p d)
--     return (tuple ^. _HList)

--   get =
--     return $ \hlist -> Cons (review p (hlist ^. from _HList)) Nil


--------------------------------------------------------------------------------
instance ListIso '[a, b] (a, b) where
  _HList = iso (\(a, b) -> Cons a (Cons b Nil)) (\(Cons a (Cons b Nil)) -> (a, b))

instance ListIso '[a, b, c] (a, b, c) where
  _HList = iso (\(a, b, c) -> Cons a (Cons b (Cons c Nil))) (\(Cons a (Cons b (Cons c Nil))) -> (a, b, c))

-- data PairOfStrings = PairOfStrings String String
-- makePrisms ''PairOfStrings

-- pairOfStrings :: Serializer '[] '[PairOfStrings]
-- pairOfStrings = usePrism _PairOfStrings . string . string


--------------------------------------------------------------------------------
data Strings = PairOfStrings String String | ThreeStrings String String String deriving Show
makePrisms ''Strings

-- pairOfStrings :: Serializer '[] '[Strings]
-- pairOfStrings = usePrism _PairOfStrings . string . string


--------------------------------------------------------------------------------
data Serializer :: [*] -> [*] -> * where
  Serializer :: (Get (List a -> List b)) -> (List b -> Maybe (PutM (List a))) -> Serializer a b

instance Category Serializer where
  (Serializer g g') . (Serializer f f') =
    Serializer (g >>= \b -> f >>= \a -> return (b . a))
               (\a -> do putF <- g' a
                         let (b, lbs) = runPutM putF
                         putG <- f' b
                         return (putLazyByteString lbs >> putG))

  id = Serializer (return id) (return . return)

instance Monoid (Serializer a b) where
  mempty = Serializer mzero (const mzero)
  (Serializer g p) `mappend` (Serializer g' p') =
    Serializer (g `mplus` g') (\i -> p i `mplus` p' i)

usePrism :: ListIso a b => Prism' d b -> Serializer a '[d]
usePrism p = Serializer get put
  where
  put (Cons d Nil) = do
    tuple <- preview p d
    Just $ return (tuple ^. _HList)

  get = return $ \hlist -> Cons (review p (hlist ^. from _HList)) Nil

string :: Serializer as (String ': as)
string = Serializer (do a <- get; return (Cons a))
                    (\(Cons a as) -> Just $ do put a; return as)

disambiguate :: Int -> Serializer s s
disambiguate a = Serializer (do a' <- get; guard (a == a'); return id)
                            (\x -> Just (do put a; return x))

strings :: Serializer '[] '[Strings]
strings = mconcat
  [ usePrism _PairOfStrings . disambiguate 1 . string . string
  , usePrism _ThreeStrings . disambiguate 2 . string . string . string
  ]
