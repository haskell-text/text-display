{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Strict #-}

module Main where

import Data.Foldable
import Data.Function
import Development.Shake
import Development.Shake.FilePath
import qualified LiterateX
import LiterateX.Renderer (Options (..))
import qualified LiterateX.Renderer as Renderer
import qualified LiterateX.Types.SourceFormat as SourceFormat
import qualified System.Directory as Directory
import LiterateX.Types.TargetFormat (TargetFormat(..))

data FileFormat
  = Haskell
  | Markdown
  deriving stock (Eq, Show)

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles = "_build"} $ do
  phony "process" $ do
    putInfo "[+] Processing files…"
    liftIO $ Directory.createDirectoryIfMissing True "./doc/.markdown"
    liftIO Directory.getCurrentDirectory
    haskellFiles' <- getDirectoryFiles "./doc/src" ["//*.hs"]
    let haskellFiles = filter (/= "Main.hs") haskellFiles'
    markdownFiles <- getDirectoryFiles "./doc/src" ["//*.md"]
    liftIO $ processFiles Haskell haskellFiles
    liftIO $ processFiles Markdown markdownFiles

processFiles :: FileFormat -> [FilePath] -> IO ()
processFiles Haskell files = do
  forM_ files $ \f -> do
    let fileName = toMarkdownFile f
    liftIO $
      LiterateX.transformFileToFile
        SourceFormat.DoubleDash
        literatexOptions
        ("./doc/src" </> f)
        fileName
processFiles Markdown files = do
  forM_ files $ \f -> do
    let fileName = toMarkdownFile f
    Directory.copyFile ("./doc/src" </> f) fileName

literatexOptions :: Options
literatexOptions =
  Renderer.defaultOptions
    { codeLanguage = Just "haskell"
    , numberCodeLines = False
    , targetFormat = MdBook
    }

toMarkdownFile :: FilePath -> FilePath
toMarkdownFile f =
  f
    & dropExtension
    & appendExtension ".md"
    & prependPath "./doc/.markdown"

{-| Prepend the second argument to the first.
 To be used in pipelines.

 > "foobar" & prependPath "book"
 "book/foobar"
-}
prependPath :: FilePath -> FilePath -> FilePath
prependPath prefixPath path = prefixPath </> path

{-| Append the first argument to the second as an extension.
 To be used in pipelines.

 > "foobar" & appendExtension ".md"
 "foobar.md"
-}
appendExtension :: FilePath -> FilePath -> FilePath
appendExtension extension path = path <.> extension
