{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import Data.Text.Encoding (decodeUtf8)
import Network.WebSockets
import qualified Data.ByteString.Lazy as LBS

main :: IO ()
main = runServer "127.0.0.1" 8080 handleConnection

handleConnection pending = do
  connection <- acceptRequest pending
  let loop wants = do
        commandMsg <- receiveDataMessage connection
        case commandMsg of
          Text (parseWant -> Just want) -> do
            sendTextData connection ("Hohoho, as long as you've been good this year!" :: Text)
            loop (want : wants)

          Text "What do I want?" -> do
             mapM (sendTextData connection) wants
             loop wants

          _ -> do
            sendTextData connection ("<img src=\"http://bit.ly/1kmRC7Q\" />" :: Text)
            loop wants

  loop []

parseWant :: LBS.ByteString -> Maybe Text
parseWant = stripPrefix "I want " . decodeUtf8 . LBS.toStrict
