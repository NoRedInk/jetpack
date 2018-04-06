module Message
  ( warning
  , success
  , error
  , list
  ) where

import Data.Foldable (traverse_)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Prelude hiding (error)
import Rainbow
       (Chunk, Radiant, back, black, brightGreen, brightRed, brightWhite,
        chunk, cyan, fore, green, only256, putChunkLn, red, white)

success :: T.Text -> IO ()
success = block Theme {bg = green <> only256 brightGreen, fg = black}

warning :: T.Text -> IO ()
warning = block Theme {bg = white <> only256 brightWhite, fg = black}

error :: T.Text -> IO ()
error = block Theme {bg = red <> only256 brightRed, fg = black}

list :: [T.Text] -> IO ()
list = traverse_ (putChunkLn . fore cyan . chunk . (<>) ("- "))

data Theme = Theme
  { bg :: Radiant
  , fg :: Radiant
  }

block :: Theme -> T.Text -> IO ()
block Theme {bg, fg} = surroundedByNL . back bg . fore fg . chunk . spaced

surroundedByNL :: Chunk T.Text -> IO ()
surroundedByNL msg = do
  _ <- TIO.putStrLn ""
  _ <- putChunkLn msg
  TIO.putStrLn ""

spaced :: T.Text -> T.Text
spaced text = "  " <> text <> "  "
