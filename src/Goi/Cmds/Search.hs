{-# LANGUAGE MonadComprehensions, OverloadedStrings, TupleSections, ViewPatterns #-}

module Goi.Cmds.Search (searchCmd) where

import Goi.Data
import Goi.FilePaths
import Goi.Util.Color
import Goi.Util.Db
import Goi.Util.Haskeline
import Goi.Util.Misc
import Goi.Util.State

import Control.Arrow ((***))
import Control.Monad (forM_, unless)
import Data.Char (ord)
import Data.List (singleton)
import Data.Text (Text)
import Database.SQLite.Simple (NamedParam(..), Query(..), queryNamed)
import qualified Data.Text as T
import qualified Data.Text.IO as T (putStrLn, readFile)

----------

searchCmd :: Stack ()
searchCmd = (,) <$> goiFile <*> yonmojiFile >>= \(("goi.txt", ) *** ("yonmoji.txt", ) -> (goiPair, yonmojiPair)) -> do
    searchHist  <- getSearchHist
    searchHist' <- do
        search <- [ f t | let f t' | T.null t' = searchHist | otherwise = t', t <- getInputLine' "| " ]
        withConnection' $ \conn ->
          let queryHelper tblName t | q <- Query $ "SELECT id, kanji, kana FROM " <> tblName <> " WHERE instr(kanji, :t) > 0 OR instr(kana, :t) > 0" = do
                  T.putStrLn . colorize sourceColor $ tblName <> ":"
                  rs <- queryNamed conn q . singleton $ ":t" := t :: IO [(Int, Text, Text)]
                  forM_ rs $ \(i, kanjiText, kanaText) -> T.putStrLn . T.concat $ [ colorize prefixColor . showText $ i, "　", colorQuote kanjiColor t kanjiText, divider, colorQuote kanaColor t kanaText ]
              fileHelper t (n, fn) = do T.putStrLn . colorize sourceColor $ n <> ":"
                                        mapM_ (T.putStrLn . prefix . colorHelper t) . filter (t `T.isInfixOf`) . T.lines =<< T.readFile fn
                where
                  prefix = (colorize prefixColor "＃" <>)
          in (>> return search) . unless (T.null search) $ do mapM_ (`queryHelper` search) [ "goi", "yonmoji" ]
                                                              mapM_ (fileHelper search) [ goiPair, yonmojiPair ]
    setSearchHist searchHist'
  where
    sourceColor = fgRed
    prefixColor = fgYellow
    kanjiColor  = bgBlue
    kanaColor   = bgGreen
    eitherColor = bgYellow
    divider     = colorize fgMagenta "／"
    colorHelper s t | [x, y] <- T.splitOn "／" t                     = f x y
                    | [x, y] <- T.splitOn "　" t, not . hasKanji $ y = f x y
                    | otherwise                                      = colorQuote eitherColor s t
      where
        f x y = colorQuote kanjiColor s x <> divider <> colorQuote kanaColor s y
    hasKanji = T.any $ ((&&) `on'` ((>= 0x4E00), (<= 0x9FFF))) . ord
