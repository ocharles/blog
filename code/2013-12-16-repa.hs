{-# LANGUAGE FlexibleContexts #-}
import Control.Applicative
import Control.Monad (replicateM)
import Codec.Picture (DynamicImage(..), Image(..), saveBmpImage, savePngImage)
import Data.Array.Repa
import Data.Array.Repa.Repr.Vector (toVector, computeVectorP)
import Data.Word
import Data.Vector.Storable (convert)
import System.Random (randomRIO)

import qualified Codec.Picture as JP

import qualified Data.Vector as V

loadImage :: FilePath -> IO (Array D DIM3 Word8)
loadImage path = do
  Right (JP.ImageRGBA8 img) <- JP.readImage path
  return $ fromFunction
    (Z :. imageHeight img :. imageWidth img :. 4)
    (\(Z :. y :. x :. c) -> case JP.pixelAt img x y of
                              JP.PixelRGBA8 r g b a ->
                                case c of
                                  0 -> r
                                  1 -> g
                                  2 -> b
                                  3 -> a)

main :: IO ()
main = do
  me <- loadImage "ocharles.png"
  snowflake <- loadImage "snowflake.png"
  
  let (Z :. height :. width :. _) = extent me

  snowflakeLocations <- replicateM 10 ((,) <$> randomRIO (0, width)
                                           <*> randomRIO (0, height))
  
  let withSnowflake =
        foldl (\img pos -> addSnowflake snowflake pos img)
              me
              snowflakeLocations

  putStrLn "Computifying!"
  vector <- fmap toVector (computeVectorP withSnowflake)
  putStrLn "Saving!"

  savePngImage "festive-ocharles.bmp" $ ImageRGBA8 
    Image { imageWidth = width
          , imageHeight = height
          , imageData = convert vector
          }

addSnowflake
  :: (Source r1 Word8, Source r2 Word8)
  => Array r1 DIM3 Word8
  -> (Int, Int)
  -> Array r2 DIM3 Word8
  -> Array D DIM3 Word8
addSnowflake snowflake (offsetX, offsetY) source =
  traverse2 source snowflake resize blend

 where

  resize sourceSize _ = sourceSize

  blend lookupSource lookupSnowflake p@(Z :. y :. x :. 3) =
    lookupSource p

  blend lookupSource lookupSnowflake p@(Z :. y :. x :. chan) =
    let (snowflakeX, snowflakeY) = (x - offsetX, y - offsetY)
        sourcePos = (Z :. snowflakeY :. snowflakeX :. chan)
        alpha = fromIntegral (lookupSnowflake (Z :. snowflakeY :. snowflakeX :. 3)) / 255
 
    in if inShape (extent snowflake) sourcePos
         then let a = fromIntegral (lookupSource p)
                  b = fromIntegral (lookupSnowflake sourcePos)
              in round $ a + (b - a) * alpha
         else lookupSource p
