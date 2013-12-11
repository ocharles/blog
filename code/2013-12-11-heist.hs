{-# LANGUAGE OverloadedStrings #-}
import Blaze.ByteString.Builder (toByteString)
import qualified Data.ByteString.Char8 as BS
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Either
import Data.Monoid (mempty)
import Data.Foldable (forM_)
import Heist
import Heist.Interpreted
import Text.XmlHtml (Node(TextNode), renderHtmlFragment, Encoding(UTF8))

billy :: IO ()
billy = eitherT (putStrLn . unlines) return $ do
  heist <- initHeist mempty
    { hcTemplateLocations = [ loadTemplates "templates" ]
    , hcInterpretedSplices = defaultInterpretedSplices
    } 

  dom <- evalHeistT
           (callTemplate "billy" mempty)
           (TextNode "")
           heist

  liftIO . BS.putStrLn . toByteString . renderHtmlFragment UTF8 $ dom


names :: IO ()
names = eitherT (putStrLn . unlines) return $ do
  heist <- initHeist mempty
    { hcTemplateLocations = [ loadTemplates "templates" ]
    , hcInterpretedSplices = defaultInterpretedSplices
    }

  names <- liftIO getNames

  forM_ names $ \name -> do
    dom <- evalHeistT
             (callTemplate "merry-christmas" $ do
                "kiddo" ## textSplice name)
             (TextNode "")
             heist

    liftIO . BS.putStrLn . toByteString . renderHtmlFragment UTF8 $ dom


getNames = return [ "Tom", "Dick", "Harry" ]


summary :: IO ()
summary = eitherT (putStrLn . unlines) return $ do
  heist <- initHeist mempty
    { hcTemplateLocations = [ loadTemplates "templates" ]
    , hcInterpretedSplices = defaultInterpretedSplices
    }

  dom <- evalHeistT
           (callTemplate "summary" $ do
              "names" ## namesSplice)
           (TextNode "")
           heist

  liftIO . BS.putStrLn . toByteString . renderHtmlFragment UTF8 $ dom


namesSplice =
  liftIO getNames >>=
    mapSplices (\name -> runChildrenWithText ("name" ##  name))
