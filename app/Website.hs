{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Compilers
import Helpers
import Hakyll

main :: IO ()
main =
  hakyll $ do
    match "static/**" $ do
      route idRoute
      compile copyFileCompiler
    match "css/*.css" $ do
      route idRoute
      compile compressCssCompiler
    match "css/**" $ do
      route idRoute
      compile copyFileCompiler
    match "posts/*/index.*" $ do
      route makeHtmlRoute
      compile makePost
    match "posts/*/**" $ do
      route idRoute
      compile copyFileCompiler
    match "posts/*" $ do
      route cleanRoute
      compile makePost
    match "output.md" $ do
      route cleanRoute
      compile $
        pandocCompilerWith pandocReadOpts pandocWriteOpts
          >>= standardFilter postCtx
    match "blog.md" $ do
      route cleanRoute
      compile $ do
        posts <- getRecentPosts Nothing
        let blogCtx =
              listField "posts" postCtx (return posts)
                <> defaultContext
        getResourceBody >>= applyAsTemplate blogCtx
          >>= renderPandocWith pandocReadOpts pandocWriteOpts
          >>= standardFilter postCtx
    match "index.md" $ do
      route makeHtmlRoute
      compile $
        pandocCompilerWith pandocReadOpts pandocWriteOpts
          >>= standardFilter postCtx
    match "templates/*" $ compile templateBodyCompiler
    create ["rss.xml"] $ do
      route idRoute
      compile $ do
        bodyTemplate <- loadBody "templates/rss.xml"
        itemTemplate <- loadBody "templates/rss-item.xml"
        feedCompiler (renderRssWithTemplates bodyTemplate itemTemplate)
    create ["atom.xml"] $ do
      route idRoute
      compile $ do
        bodyTemplate <- loadBody "templates/atom.xml"
        itemTemplate <- loadBody "templates/atom-item.xml"
        feedCompiler (renderAtomWithTemplates bodyTemplate itemTemplate)
