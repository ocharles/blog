{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Prelude hiding ((.))
import Control.Applicative
import Control.Category ((.))
import Data.Foldable (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mconcat)
import Text.Boomerang.TH
import Web.Routes.Boomerang
import Web.Routes.RouteT
import Web.Routes.Site
import qualified Data.Text as Text
import Data.Text (Text)

type PostId = Integer

data Sitemap
  = Index
  | Post PostId
  | Tagged [Text]

data BlogPost = BlogPost { postTitle :: Text, postId :: PostId }

makeBoomerangs ''Sitemap

siteRouter = mconcat
  [ rIndex
  , rPost . "post" </> integer
  , rTagged . "tags" </> (satisfyStr (not . Text.null) `rListSep` "+")
  ]

handler :: Sitemap -> RouteT Sitemap IO ()
handler route = case route of
  Index -> do
    posts <- liftIO getPosts
    liftIO $ putStrLn "Posts:"
    forM_ posts $ \post -> do
      postUrl <- showURL (Post (postId post))
      liftIO $ putStrLn $ Text.unpack (postTitle post) ++ " - " ++ Text.unpack postUrl

  Post pId -> liftIO $ do
    [post] <- filter ((== pId) . postId) <$> getPosts
    putStrLn $ "You are reading \"" ++ Text.unpack (postTitle post) ++ "\""

site :: Site Sitemap (IO ())
site = boomerangSiteRouteT handler siteRouter

getPosts :: Monad m => m [BlogPost]
getPosts = return
  [ BlogPost "24 Days of Hackage" 42
  , BlogPost "10 Reasons Why P = NP" 91
  ]
