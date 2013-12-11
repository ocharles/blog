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

  Just (output, _) <- renderTemplate heist "billy" 

  liftIO . BS.putStrLn . toByteString $ output


names :: IO ()
names = eitherT (putStrLn . unlines) return $ do
  heist <- initHeist mempty
    { hcTemplateLocations = [ loadTemplates "templates" ]
    , hcInterpretedSplices = defaultInterpretedSplices
    }

  names <- liftIO getNames

  forM_ names $ \name -> do
    Just (output, _) <- renderTemplate
      (bindSplice "kiddo" (textSplice name) heist)
      "merry-christmas"

    liftIO . BS.putStrLn . toByteString $ output


getNames = return [ "Tom", "Dick", "Harry" ]


summary :: IO ()
summary = eitherT (putStrLn . unlines) return $ do
  heist <- initHeist mempty
    { hcTemplateLocations = [ loadTemplates "templates" ]
    , hcInterpretedSplices = defaultInterpretedSplices
    }

  Just (output, _) <- renderTemplate
    (bindSplice "names" namesSplice heist)
    "summary"

  liftIO . BS.putStrLn . toByteString $ output


namesSplice =
  liftIO getNames >>=
    mapSplices (\name -> runChildrenWithText ("name" ##  name))
