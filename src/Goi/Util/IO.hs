module Goi.Util.IO ( nl
                   , putStrFlush ) where

import Data.Text (Text)
import qualified Data.Text.IO as T (putStr)
import System.IO (hFlush, stdout)

----------

nl :: IO ()
nl = putChar '\n'

putStrFlush :: Text -> IO ()
putStrFlush t = T.putStr t >> hFlush stdout
