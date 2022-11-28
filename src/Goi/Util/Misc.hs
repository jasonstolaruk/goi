{-# LANGUAGE ViewPatterns #-}

module Goi.Util.Misc ( allValues
                     , both
                     , dup
                     , nubSort
                     , on'
                     , safeHead
                     , safeLast
                     , showText
                     , split
                     , twice
                     , unadulterated ) where

import Control.Arrow ((***), (&&&))
import Control.Monad (join)
import Data.Text (Text)
import qualified Data.Set as S (fromList, toList)
import qualified Data.Text as T

----------

allValues :: Enum a => [a]
allValues = [toEnum 0..]

both :: (a -> b) -> (a, a) -> (b, b)
both = join (***)

dup :: a -> (a, a)
dup = id &&& id

nubSort :: Ord a => [a] -> [a]
nubSort = S.toList . S.fromList

{-
"on":  (f `on`  g     ) x y == f (g x) (g y)
"on'": (f `on'` (g, h)) x   == f (g x) (h x)
Sample usage:
((==) `on'` (fmap (+1), second (+1))) ('a', 0)
-}
infixl 0 `on'`
on' :: (b -> c -> d) -> (a -> b, a -> c) -> a -> d
on' f (g, h) = uncurry f . (g &&& h)
--(.*.) `on'` (f, g) = \x -> (f x) .*. (g x)

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

safeLast :: [a] -> Maybe a
safeLast []     = Nothing
safeLast [x]    = Just x
safeLast (_:xs) = safeLast xs

showText :: Show a => a -> Text
showText = T.pack . show

split :: Char -> Text -> (Text, Text)
split (T.singleton -> c) = fmap T.tail . T.breakOn c

twice :: (a -> a) -> a -> a
twice f = f . f

unadulterated :: (Monad m, Applicative f) => a -> m (f a)
unadulterated = pure . pure
