{-# LANGUAGE OverloadedStrings #-}
module Message where


import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Error
import Rainbow (Chunk, brightRed, chunk, fore, green, yellow, brightYellow, putChunkLn, red, (&))
import qualified System.Console.Terminal.Size as TermSize

termWidth :: IO Int
termWidth =
  max 20
    <$> M.maybe 20 TermSize.width
    <$> TermSize.size

success :: IO ()
success = do
  width <- termWidth
  _ <- putChunkLn (separator width "*" & fore green)
  _ <- putChunkLn (successMessage width)
  putChunkLn (separator width "*" & fore green)

error :: [Error.Error] -> IO ()
error err = do
  width <- termWidth
  _ <- putStrLn ""
  putChunkLn (separator width "~" & fore red)
  _ <- traverse (putChunkLn
          . fore brightRed
          . chunk
          . Error.description
          ) err
  _ <- putChunkLn (separator width "~" & fore red)
  _ <- traverse putChunkLn (errorMessage width)
  putChunkLn (separator width "~" & fore red)

warning :: T.Text -> IO ()
warning warnings = do
  width <- termWidth
  _ <- putChunkLn (chunk warnings & fore brightYellow)
  _ <- putChunkLn (separator width "*" & fore yellow)
  _ <- putChunkLn (warningMessage width)
  putChunkLn (separator width "*" & fore yellow)

successMessage :: Int -> Chunk T.Text
successMessage width =
  fore green $ chunk
    $ center width "~*~Compilation Succeeded~*~"

warningMessage :: Int -> Chunk T.Text
warningMessage width =
  fore yellow $ chunk
    $ center width "~*~Compilation Succeeded with Warnings~*~"

separator :: Int -> T.Text -> Chunk T.Text
separator width c =
  chunk $ T.replicate width c

errorMessage :: Int -> [Chunk T.Text]
errorMessage width =
  fmap (fore red . chunk . center width . T.pack)
      [ "¡Compilation failed!"
      , "¯\\_(ツ)_/¯"
      ]

center :: Int -> T.Text -> T.Text
center width msg =
  T.append (T.replicate n " ") msg
  where
    textLength = T.length msg
    half = quot width 2
    halfText = quot textLength 2
    n = half - halfText
