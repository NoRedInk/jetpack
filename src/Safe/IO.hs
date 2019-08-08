{-# LANGUAGE ScopedTypeVariables #-}

module Safe.IO
  ( writeFile
  , writeFileByteString
  )
where

import qualified Control.Exception as Exception
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.Directory as Dir
import System.FilePath ((<.>))
import Prelude hiding (writeFile)

writeFile :: FilePath -> T.Text -> IO ()
writeFile path content = do
  let writeThenMove tmp = do
        TIO.writeFile tmp content
        Dir.renameFile tmp path
      tmp = path <.> "tmp"
  writeThenMove tmp `Exception.finally`
    (Dir.removeFile tmp `Exception.catch` \(_ :: IOError) -> return ())

writeFileByteString :: FilePath -> BL.ByteString -> IO ()
writeFileByteString path content = do
  let writeThenMove tmp = do
        BL.writeFile tmp content
        Dir.renameFile tmp path
      tmp = path <.> "tmp"
  writeThenMove tmp `Exception.finally`
    (Dir.removeFile tmp `Exception.catch` \(_ :: IOError) -> return ())
