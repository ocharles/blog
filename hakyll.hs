{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Control.Monad
import Data.Monoid
import Hakyll
import Text.Pandoc

main :: IO ()
main = hakyll $ do
  match "templates/*" $
    compile $ templateCompiler

  match "css/***.css" $ do
    route idRoute
    compile copyFileCompiler

  match "img/***" $ do
    route idRoute
    compile copyFileCompiler

  match "posts/***.md" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/post.html" defaultContext
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  create ["index.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst <$> loadAll "posts/*"
      postItem <- loadBody "templates/postitem.html"
      postStr <- applyTemplateList postItem defaultContext posts
      makeItem ""
        >>= loadAndApplyTemplate "templates/posts.html"
              (constField "posts" postStr <> defaultContext)
        >>= loadAndApplyTemplate "templates/default.html"
              (constField "title" "Posts" <> defaultContext)
        >>= relativizeUrls

  create ["posts.rss"] $ do
    route idRoute
    compile $ do
      posts <- take 10 . recentFirst <$> loadAllSnapshots "posts/*" "content"
      renderRss feedConfiguration
        (bodyField "description" <> defaultContext)
        posts
  where
    postContext = dateField "date" "%Y-%m-%d"

feedConfiguration = FeedConfiguration
    { feedTitle = "Inside ocharles"
    , feedDescription = "MusicBrainz hacker. Haskell geek. Wannabe mathematician. Electronic music fanatic."
    , feedAuthorName = "Oliver Charles"
    , feedRoot = "http://ocharles.org.uk/blog"
    , feedAuthorEmail = "ollie@ocharles.org.uk"
    }
