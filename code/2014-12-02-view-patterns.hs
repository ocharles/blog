{-# LANGUAGE ViewPatterns #-}

import Data.Map as M
import Data.Sequence

type HaskellPackage = String

lensDownloadsOld :: Map HaskellPackage Int -> Int
lensDownloadsOld packages =
  case M.lookup "lens" packages of
    Just n -> n
    Nothing -> 0

lensDownloads :: Map HaskellPackage Int -> Int
lensDownloads (M.lookup "lens" -> Just n) = n
lensDownloads _                           = 0

downloadsFor :: HaskellPackage -> Map HaskellPackage Int -> Int
downloadsFor pkg (M.lookup pkg -> Just downloads) = downloads
downloadsFor _   _                                = 0

data List a = Nil | Cons a (List a)

last :: Seq a -> Maybe a
last (viewr -> xs :> x) = Just x
last (viewr -> EmptyR) = Nothing
