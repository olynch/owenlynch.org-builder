{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Helpers where

import Hakyll

import qualified Network.URI.Encode as URI
import System.FilePath.Posix (replaceExtension, takeBaseName, takeDirectory, (</>))

import Compilers

hostURL :: String
hostURL = "https://owenlynch.org"

cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
      where
        p = toFilePath ident

makeHtmlRoute :: Routes
makeHtmlRoute = customRoute (flip replaceExtension "html" . toFilePath)

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration =
  FeedConfiguration
    { feedTitle = "Owen's Blog",
      feedDescription = "A personal blog; that means I don't have to stick to one thing. But expect math.",
      feedAuthorName = "Owen Lynch",
      feedAuthorEmail = "root@owenlynch.org",
      feedRoot = "https://owenlynch.org"
    }

makePost :: Compiler (Item String)
makePost = do
  (post, customCtx) <- postCompiler
  let ctx = customCtx <> postCtx
  loadAndApplyTemplate "templates/post.html" ctx post
    >>= standardFilter ctx

makeBarePost :: Compiler (Item String)
makeBarePost = do
  (post, customCtx) <- postCompiler
  return post

getRecentPosts :: Maybe Int -> Compiler [Item String]
getRecentPosts limit =
  (++) <$> loadAll "posts/*" <*> loadAll "posts/*/index.*"
    >>= (maybe id (fmap . take) limit) . recentFirst

postCtx :: Context String
postCtx =
  mapContext (URI.encode . (hostURL ++)) (urlField "urlencoded")
    `mappend` dateField "date" "%B %e, %Y"
    `mappend` constField "author" "Owen Lynch"
    `mappend` constField "authorwebsite" "https://owenlynch.org"
    `mappend` defaultContext

type FeedRenderer =
  FeedConfiguration ->
  Context String ->
  [Item String] ->
  Compiler (Item String)

feedCompiler :: FeedRenderer -> Compiler (Item String)
feedCompiler renderer = do
  let feedCtx =
        bodyField "description"
          `mappend` mapContext takeDirectory (urlField "url")
          `mappend` postCtx
  posts <- getRecentPosts (Just 20)
  renderer myFeedConfiguration feedCtx posts
