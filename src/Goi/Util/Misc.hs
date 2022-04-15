{-# LANGUAGE ViewPatterns #-}

module Goi.Util.Misc ( both
                     , dup
                     , nl
                     , showText
                     , split ) where

import Control.Arrow ((***), (&&&), second)
import Control.Monad (join)
import Data.Text (Text)
import qualified Data.Text as T

----------

both :: (a -> b) -> (a, a) -> (b, b)
both = join (***)

dup :: a -> (a, a)
dup = id &&& id

nl :: IO ()
nl = putChar '\n'

showText :: Show a => a -> Text
showText = T.pack . show

split :: Char -> Text -> (Text, Text)
split (T.singleton -> c) = second T.tail . T.breakOn c
