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

  match "posts/***.lhs" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/post.html" defaultContext
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  match "guest-posts/***.md" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/guest-post.html" defaultContext
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  match "pages/***.md" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  match "private/***.md" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  create ["index.html"] $ do
    route idRoute
    compile $ do
      posts <- do
        myPosts <- loadAll "posts/*"
        guestPosts <- loadAll "guest-posts/*"
        recentFirst (myPosts <> guestPosts)

      postItem <- loadBody "templates/postitem.html"
      postStr <- applyTemplateList postItem defaultContext posts

      pages <- loadAll "pages/*" >>= recentFirst
      pagesStr <- applyTemplateList postItem defaultContext pages

      makeItem ""
        >>= loadAndApplyTemplate "templates/posts.html"
              (constField "pages" pagesStr <>
               constField "posts" postStr <>
               defaultContext)
        >>= loadAndApplyTemplate "templates/default.html"
              (constField "title" "ocharles.org.uk" <> defaultContext)
        >>= relativizeUrls

  create ["posts.rss"] $ do
    route idRoute
    compile $ do
      posts <- take 10 <$> (loadAllSnapshots "posts/*" "content" >>= recentFirst >>= mapM relativizeUrls)
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
