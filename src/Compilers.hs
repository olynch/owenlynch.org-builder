{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Filters for my site
module Compilers where

import           Control.Monad                  ( (>=>) )
import           Data.ByteString.Lazy.Char8     ( pack
                                                , unpack
                                                )
import           Data.List                      ( isSuffixOf )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Hakyll
import           Network.URI.Encode            as URI
import           System.IO.Temp
import           System.Process
import           Text.Pandoc
import           Text.Pandoc.Highlighting       ( pygments )
import           Text.Pandoc.Walk

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndex :: String -> String
cleanIndex url | idx `isSuffixOf` url = take (length url - length idx) url
               | otherwise            = url
  where idx = "index.html"

pandocReadOpts :: ReaderOptions
pandocReadOpts =
  defaultHakyllReaderOptions { readerExtensions = pandocExtensions }

pandocWriteOpts :: WriterOptions
pandocWriteOpts = defaultHakyllWriterOptions
  { writerHTMLMathMethod = KaTeX ""
  , writerHighlightStyle = Just pygments
  }

standardFilter :: Context a -> Item a -> Compiler (Item String)
standardFilter ctx =
  loadAndApplyTemplate "templates/default.html" ctx
    >=> relativizeUrls
    >=> cleanIndexUrls

imageBlock :: Text -> [(Text, Text)] -> Text -> Block
imageBlock id namevals fname =
  Para [Image (id, ["tikzpicture"], namevals) [] (fname, "")]

tikzFilter :: Text -> FilePath -> Block -> Compiler Block
tikzFilter tag template x@(CodeBlock (id, classes, namevals) contents) =
  if classes == [tag]
    then
      (imageBlock id namevals
      . T.pack
      . ("data:image/svg+xml;utf8," ++)
      . URI.encode
      . filter (/= '\n')
      . T.unpack
      . itemBody <$>
      )
      $   makeItem (T.unpack contents)
      >>= loadAndApplyTemplate (fromFilePath template) (bodyField "body")
      >>= withItemBody
            (   return
            .   pack
            .   T.unpack
            >=> unixFilterLBS "rubber-pipe" ["--pdf"]
            >=> unixFilterLBS "pdftocairo"  ["-svg", "-", "-"]
            >=> return
            .   T.pack
            .   unpack
            )
      .   fmap T.pack
    else return x
tikzFilter _ _ x = return x

myPandocCompiler :: Compiler (Item String)
myPandocCompiler =
  pandocCompilerWithTransformM pandocReadOpts pandocWriteOpts $ walkM
    (   tikzFilter "tikz"   "templates/tikz.tex"
    >=> tikzFilter "tikzcd" "templates/tikzcd.tex"
    >=> tikzFilter "tikzit" "templates/tikzit.tex"
    )

inlinesToString :: [Inline] -> String
inlinesToString = concatMap inlineToString
 where
  inlineToString (Str a) = T.unpack a
  inlineToString Space   = " "

texmacsCompiler :: Compiler (Item String, Context String)
texmacsCompiler = do
  fp       <- getResourceFilePath
  htmlFile <- unsafeCompiler $ withTempDirectory "." "" $ \tmpdir -> do
    let tmp = tmpdir ++ "/out.html"
    callProcess "xvfb-run" ["texmacs", "-c", fp, tmp, "-q"]
    readFile tmp
  (m, bs) <- case runPure $ readHtml pandocReadOpts (T.pack htmlFile) of
    Left  err           -> fail $ "texmacsCompiler: parse failed: " ++ show err
    Right (Pandoc m bs) -> return (m, bs)
  doc <- makeItem $ Pandoc m bs
  let title = inlinesToString $ docTitle m
  return
    ( writePandocWith pandocWriteOpts doc
    , constField "title" $ inlinesToString $ docTitle m
    )

postCompiler :: Compiler (Item String, Context String)
postCompiler = do
  ext <- getUnderlyingExtension
  if ext == ".tm" then texmacsCompiler else (, mempty) <$> myPandocCompiler
