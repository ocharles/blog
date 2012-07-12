{-# LANGUAGE Arrows, OverloadedStrings #-}
import Control.Arrow
import Control.Monad
import Data.Monoid
import Hakyll

main = hakyll $ do
  match "templates/*.html" $ compile templateCompiler

  match "css/***.css" $ route idRoute >> compile copyFileCompiler
  match "img/***" $ route idRoute >> compile copyFileCompiler

  match "posts/***.md" $ do
    route $ setExtension "html"
    compile $ pageCompiler
      >>> arr (renderDateField "date" "%Y-%m-%d" "Date Unknown")
      >>> applyTemplateCompiler "templates/post.html"
      >>> applyTemplateCompiler "templates/default.html"
      >>> relativizeUrlsCompiler

  match "index.html" $ route idRoute
  create "index.html" $ constA mempty
    >>> arr (setField "title" "Posts")
    >>> setFieldPageList recentFirst
            "templates/postitem.html" "posts" "posts/*"
    >>> applyTemplateCompiler "templates/posts.html"
    >>> applyTemplateCompiler "templates/default.html"
    >>> relativizeUrlsCompiler

  match "posts.rss" $ route idRoute
  create "posts.rss" $
    requireAll_ "posts/*"
      >>> mapCompiler (arr $ copyBodyToField "description")
      >>> renderRss feedConfiguration

feedConfiguration = FeedConfiguration
    { feedTitle = "Inside ocharles"
    , feedDescription = "MusicBrainz hacker. Haskell geek. Wannabe mathematician. Electronic music fanatic."
    , feedAuthorName = "Oliver Charles"
    , feedRoot = "http://ocharles.org.uk/blog"
    }
