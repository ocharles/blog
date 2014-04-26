{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Applicative
import Data.Functor.Compose (Compose(..))
import GHC.Generics

newtype Country = Country String deriving (Show)
data BrewMethod = Chemex | V60 | Aeropress deriving (Show)

data Coffee = Coffee { coffeeBeans :: String
                     , coffeeOriginCountry :: Country
                     , coffeeBrewMethod :: BrewMethod
                     }
  deriving (Generic, Show)


class Functor f => Mk rep f | rep -> f where
  mk :: f (rep a)

instance Mk (K1 i c) ((->) c) where
  mk = \x -> K1 x

instance (Mk l fl, Mk r fr) => Mk (l :*: r) (Compose fl fr) where
  mk = Compose (fmap (\l -> fmap (\r -> l :*: r) mk) mk)

instance (Mk f f') => Mk (M1 i c f) f' where
  mk = M1 <$> mk

class Functor f => Apply f a b | f a -> b where
  apply :: f a -> b

instance Apply ((->) a) b (a -> b) where
  apply = id

instance (Apply g a b, Apply f b c) => Apply (Compose f g) a c where
  apply (Compose x) = apply (fmap apply x)

type family Returns (f :: *) :: * where
  Returns (a -> b) = Returns b
  Returns r = r

make :: forall a b f z. (Generic (Returns b), Apply f (Returns b) b, Mk (Rep (Returns b)) f) => b
make = apply (fmap (to :: Rep (Returns b) z -> (Returns b)) (mk :: f (Rep (Returns b) z)))

--foo :: forall b r f z. (Returns b r, Generic r, Apply f r b, Mk (Rep r) f) => b
--foo = undefined
--foo = apply (fmap ((to :: Rep r z -> r)) (mk :: f (Rep r z)))

mkCoffee :: String -> Country -> BrewMethod -> Coffee
mkCoffee = make
