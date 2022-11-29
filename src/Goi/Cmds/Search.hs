{-# LANGUAGE MonadComprehensions, OverloadedStrings, TupleSections, ViewPatterns #-}

module Goi.Cmds.Search (searchCmd) where

import Goi.Data
import Goi.FilePaths
import Goi.Util.Db
import Goi.Util.Haskeline
import Goi.Util.Misc
import Goi.Util.State

import Control.Arrow ((***))
import Control.Monad (forM_, unless)
import Data.Function ((&))
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
          let queryHelper tblName col t | q  <- Query . T.concat $ [ "SELECT id, kanji, kana FROM ", tblName, " WHERE instr(", col, ", :t) > 0" ] = do
                  T.putStrLn . T.concat $ [ tblName, " - ", col, ":" ]
                  rs <- queryNamed conn q . singleton $ ":t" := t :: IO [(Int, Text, Text)]
                  forM_ rs $ \(i, kanjiText, kanaText) -> T.putStrLn . T.intercalate " / " $ [ showText i, colorize t kanjiText, colorize t kanaText ]
              fileHelper t (n, fn) = do T.putStrLn $ n <> ":"; mapM_ (T.putStrLn . colorize t) . filter (t `T.isInfixOf`) . T.lines =<< T.readFile fn
          in (>> return search) . unless (T.null search) $ do
              mapM_ (search &) $ [ queryHelper x y | x <- [ "goi", "yonmoji" ], y <- [ "kanji", "kana" ] ]
              mapM_ (fileHelper search) [ goiPair, yonmojiPair ]
    setSearchHist searchHist'

colorize :: Text -> Text -> Text
colorize t = (<> "\ESC[K") . T.replace t ("\ESC[44m" <> t <> "\ESC[0m") -- TODO: Append "\STX" if using Haskeline for output. See "https://github.com/judah/haskeline/wiki/ControlSequencesInPrompt".
