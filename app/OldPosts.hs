{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Hakyll
import Helpers

main = hakyll $ do
    match "templates/*" $ compile templateBodyCompiler
    match "posts/*/index.*" $ do
      route makeHtmlRoute
      compile makeBarePost
    match "posts/*/**" $ do
      route idRoute
      compile copyFileCompiler
    match "posts/*" $ do
      route cleanRoute
      compile makeBarePost
