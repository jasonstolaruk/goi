module Goi.Util.IO (putStrFlush) where

import Data.Text (Text)
import qualified Data.Text.IO as T (putStr)

import System.IO (hFlush, stdout)

----------

putStrFlush :: Text -> IO ()
putStrFlush t = T.putStr t >> hFlush stdout
