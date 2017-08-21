{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Directory (getDirectoryContents,
                         doesFileExist,
                         getCurrentDirectory,
                         removeFile)
import System.FilePath ((</>),
                        takeFileName,
                        dropExtension)
import Control.Monad (filterM, forM_)
import Data.List (intercalate)
import System.IO as IO
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import qualified Data.ByteString.Lazy as BS
import Web.Scotty
import Data.Text.Lazy (pack)

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  targetDir  <- return $ currentDir </> "stuff"
  targetFile <- return $ targetDir </> "index.html"
  isIndexed  <- doesFileExist targetFile
  if isIndexed then removeFile targetFile else return ()
  fileList   <- listFiles targetDir
  toWrite    <- return $ createIndex fileList
  BS.writeFile targetFile toWrite
  scotty 80 $ do
    get (literal "/") (file targetFile)
    forM_ fileList (\f -> get (literal $ '/' : f) (file $ targetDir </> f))

createIndex :: [FilePath] -> BS.ByteString
createIndex files = renderMarkup $ H.docTypeHtml $ do
  H.head $ do
    H.title "Available Files"
  H.body $ do
    H.p "The following files can be accessed:"
    H.ul $ forM_ files createLink
    where
  createLink name = H.li $ do 
      H.a H.! A.href (H.stringValue $ "/" ++ name) $ H.toHtml name 

listFiles :: FilePath -> IO [FilePath]
listFiles target = do
  itemPaths <- map (target </>) <$> getDirectoryContents target
  map takeFileName <$> filterM doesFileExist itemPaths
