module Goi.Util.IO ( nl
                   , putStrFlush ) where

import Data.Text (Text)
import qualified Data.Text.IO as T (putStr)
import System.IO (hFlush, stdout)

----------

nl :: IO ()
nl = putChar '\n'

-- TODO: Delete after adopting Haskeline.
putStrFlush :: Text -> IO ()
putStrFlush t = T.putStr t >> hFlush stdout
