{-# LANGUAGE LambdaCase, ViewPatterns #-}

module Goi.Util.Haskeline (getInputLine') where

import Goi.Data

import Control.Monad.Trans.Class (lift)
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text as T
import System.Console.Haskeline (getInputLine)

----------

getInputLine' :: Text -> Stack Text
getInputLine' (T.unpack -> s) = f <&> g
  where
    f = lift . lift . getInputLine $ s
    g = \case Just (T.pack -> t) -> T.strip t
              Nothing            -> T.empty
