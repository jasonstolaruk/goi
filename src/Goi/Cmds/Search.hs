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
                  T.putStrLn . colorize fgGreen $ tblName <> ":"
                  rs <- queryNamed conn q . singleton $ ":t" := t :: IO [(Int, Text, Text)]
                  forM_ rs $ \(i, kanjiText, kanaText) -> T.putStrLn . T.concat $ [ showText i, " / ", colorQuote bgBlue t kanjiText, colorize fgMagenta " / ", colorQuote bgBlue t kanaText ]
              fileHelper t (n, fn) = do T.putStrLn . colorize fgGreen $ n <> ":"
                                        mapM_ (T.putStrLn . colorDivider . colorQuote bgBlue t) . filter (t `T.isInfixOf`) . T.lines =<< T.readFile fn
          in (>> return search) . unless (T.null search) $ do mapM_ (`queryHelper` search) [ "goi", "yonmoji" ]
                                                              mapM_ (fileHelper search) [ goiPair, yonmojiPair ]
    setSearchHist searchHist'
  where
    colorDivider t | "／" `T.isInfixOf` t = colorQuote fgMagenta "／" t
                   | [_, x] <- T.splitOn "　" t, not . hasKanji $ x = colorQuote bgYellow "　" t
                   | otherwise = t
    hasKanji = T.any $ ((&&) `on'` ((>= 0x4E00), (<= 0x9FFF))) . ord
