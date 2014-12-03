{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Foreign.C

{-

data BlendMode = NoBlending -- | AlphaBlending | AdditiveBlending | ColourModulatedBlending

toBlendMode :: BlendMode -> CInt
toBlendMode NoBlending = 0 -- #{const SDL_BLENDMODE_NONE}
-- toBlendMode AlphaBlending = #{const SDL_BLENDMODE_BLEND}

fromBlendMode :: CInt -> Maybe BlendMode
fromBlendMode 0 = Just NoBlending

-}

{-

pattern AlphaBlending = (1) :: CInt -- #{const SDL_BLENDMODE_BLEND} :: CInt

setUpBlendMode :: CInt -> IO ()
setUpBlendMode AlphaBlending = do
  putStrLn "Enabling Alpha Blending"
  activateAlphaBlendingForAllTextures
  activateRenderAlphaBlending

-}

newtype BlendMode = MkBlendMode { unBlendMode :: CInt }

pattern NoBlending = MkBlendMode 0 -- #{const SDL_BLENDMODE_NONE}
pattern AlphaBlending = MkBlendMode 1 -- #{const SDL_BLENDMODE_BLEND}

setUpBlendMode :: BlendMode -> IO ()
setUpBlendMode AlphaBlending = do
  putStrLn "Enabling Alpha Blending"
  activateAlphaBlendingForAllTextures
  activateRenderAlphaBlending

data Renderer

setRenderAlphaBlending :: Renderer -> IO ()
setRenderAlphaBlending r =
  sdlSetRenderDrawBlendMode r (unBlendMode AlphaBlending)

activateAlphaBlendingForAllTextures = return ()
activateRenderAlphaBlending = return ()

sdlSetRenderDrawBlendMode _ _ = return ()
