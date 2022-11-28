{-# LANGUAGE ViewPatterns #-}

module Goi.Util.Haskeline (getInputLine') where

import Goi.Data

import Control.Monad.Trans.Class (lift)
import Data.Text (Text)
import qualified Data.Text as T
import System.Console.Haskeline (getInputLine)

----------

getInputLine' :: Text -> Stack Text
getInputLine' (T.unpack -> s) = maybe T.empty (T.strip . T.pack) <$> x
  where
    x = lift . lift . getInputLine $ s
