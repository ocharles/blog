{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Querying where

import Control.Applicative
import Control.Monad
import Data.Functor.Compose
import Data.Monoid
import Data.Traversable (for)

data Querying :: (* -> *) -> [KV * *] -> * -> * where
  Querying :: KeyList kvs -> Compose m (ResultF kvs) a -> Querying m kvs a

instance Functor m => Functor (Querying m kvs) where
  fmap f (Querying kvs x) = Querying kvs (fmap f x)

instance (Applicative m, Applicative (ResultF kvs), Monoid (KeyList kvs)) => Applicative (Querying m kvs) where
  pure a = Querying mempty (pure a)
  (Querying kvs f) <*> (Querying kvs' x) = Querying (kvs <> kvs') (f <*> x)

data ResultF :: [KV * *] -> * -> * where
  ResultConst :: a -> ResultF '[] a
  ResultFunc :: ([(k, v)] -> ResultF kvs a) -> ResultF ('KV k v ': kvs) a

instance Functor (ResultF kvs) where
  fmap f (ResultConst x) = ResultConst (f x)
  fmap f (ResultFunc g) = ResultFunc (fmap f . g)

instance Applicative (ResultF '[]) where
  pure = ResultConst
  (ResultConst f) <*> (ResultConst x) = ResultConst (f x)

instance Applicative (ResultF kvs) => Applicative (ResultF ('KV k v ': kvs)) where
  pure x = ResultFunc (const (pure x))
  (ResultFunc f) <*> (ResultFunc x) = ResultFunc (\args -> f args <*> x args)

data KV k v  = KV k v

data KeyList :: [KV * *] -> * where
  Nil :: KeyList '[]
  Cons :: [k] -> KeyList kvs -> KeyList ('KV k v ': kvs)

data Where :: [KV * *] -> (KV * *) -> * where
  Here :: (Monoid (KeyList tail), IdentityResultF tail) => Where (kv ': tail) kv
  There :: Where kvs kv -> Where ('KV k v ': kvs) kv

class Somewhere (kvs :: [KV * *]) (kvs' :: [KV * *]) (kv :: (KV * *)) where
  somewhere :: Where kvs kv -> Where kvs' kv

instance (Monoid (KeyList kvs'), IdentityResultF kvs') => Somewhere ('KV k v ': kvs') ('KV k v ': kvs') ('KV k v) where
  somewhere _ = Here

instance Somewhere kvs kvs' kv => Somewhere kvs ('KV k v ': kvs') kv where
  somewhere = There . somewhere

instance Monoid (KeyList '[]) where
  mempty = Nil
  mappend _ _ = Nil

instance (Monoid (KeyList kvs)) => Monoid (KeyList ('KV k v ': kvs)) where
  mempty = Cons mempty mempty
  mappend (Cons l ls) (Cons r rs) = Cons (l ++ r) (mappend ls rs)

withQuery
  :: (IdentityResultF kvs, Monoid (KeyList kvs), Monad m)
  => ([k] -> m [(k, v)])
  -> (Where ('KV k v ': kvs) ('KV k v) -> Querying m ('KV k v ': kvs) a)
  -> Querying m kvs a
withQuery query k =
  case k Here of
    Querying (keys `Cons` kvs) (Compose m) ->
      Querying kvs $ Compose $ do
        ResultFunc f <- m
        results <- query keys
        return (f results)

ask :: (Eq k, Monad m, Somewhere kvs kvs' ('KV k v)) => Where kvs ('KV k v) -> k -> Querying m kvs' (Maybe v)
ask q k = Querying (mkKeyList (somewhere q) k) (Compose $ return (mkResultF (somewhere q) k))

mkKeyList :: Where kvs ('KV k v) -> k -> KeyList kvs
mkKeyList Here k = Cons [k] (mempty)
mkKeyList (There path) k = Cons [] (mkKeyList path k)

mkResultF :: (Eq k) => Where kvs ('KV k v) -> k -> ResultF kvs (Maybe v)
mkResultF Here k = ResultFunc (identityResultF . lookup k)
mkResultF (There path) k = ResultFunc (const (mkResultF path k))

class IdentityResultF (kvs :: [KV * *]) where
  identityResultF :: a -> ResultF kvs a

instance IdentityResultF '[] where
  identityResultF = ResultConst

instance IdentityResultF xs => IdentityResultF ('KV k v ': xs) where
  identityResultF a = ResultFunc (const (identityResultF a))

runQuerying :: Monad m => Querying m '[] a -> m a
runQuerying (Querying Nil (Compose m)) = liftM (\(ResultConst a) -> a) m

example :: IO [(Maybe Int, Maybe Int)]
example =
  runQuerying $
  withQuery getUserNameById $ \userNameById ->
  withQuery getUserAgeById $ \userAgeById ->
    for [1..10] $ \userId ->
      (,) <$> ask userNameById userId <*> ask userAgeById userId

  where
  getUserNameById :: [Int] -> IO [(Int, Int)]
  getUserNameById keys = do
    print keys
    return [(1, 42)]

  getUserAgeById :: [Int] -> IO [(Int, Int)]
  getUserAgeById = const (return [(1, 5)])

main :: IO ()
main = example >>= print
