{-# LANGUAGE MonadComprehensions, OverloadedStrings, TupleSections, ViewPatterns #-}

module Goi.Cmds.Search ( compoundSearchCmd
                       , searchCmd ) where

import Goi.Data
import Goi.FilePaths
import Goi.Util.Db
import Goi.Util.Haskeline
import Goi.Util.IO
import Goi.Util.Misc
import Goi.Util.State

import Control.Arrow ((***))
import Control.Monad (forM_, unless)
import Data.Function ((&))
import Data.List (singleton)
import Data.Text (Text)
import Database.SQLite.Simple (NamedParam(..), Query(..), queryNamed)
import qualified Data.Text as T
import qualified Data.Text.IO as T (getLine, putStrLn, readFile)

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
                  forM_ rs $ \(i, kanjiText, kanaText) -> T.putStrLn . T.intercalate " / " $ [ showText i, kanjiText, kanaText ]
              fileHelper t (n, fn) = do T.putStrLn $ n <> ":"; mapM_ T.putStrLn . filter (t `T.isInfixOf`) . T.lines =<< T.readFile fn
          in (>> return search) . unless (T.null search) $ do
              mapM_ (search &) $ [ queryHelper x y | x <- [ "goi", "yonmoji" ], y <- [ "kanji", "kana" ] ]
              mapM_ (fileHelper search) [ goiPair, yonmojiPair ]
    setSearchHist searchHist'

compoundSearchCmd :: Stack ()
compoundSearchCmd = getCompoundSearchHist >>= \(kanjiSearchHist, kanaSearchHist) -> do
    searchHistPair <- withConnection' $ \conn ->
      let queryHelper tblName (t1, t2) = do
              T.putStrLn $ tblName <> ":"
              rs <- queryNamed conn q [ ":kanjiSearch" := t1, ":kanaSearch" := t2 ] :: IO [(Int, Text, Text)]
              forM_ rs $ \(i, kanjiText, kanaText) -> T.putStrLn . T.intercalate " / " $ [ showText i, kanjiText, kanaText ]
            where
              q = Query $ "SELECT id, kanji, kana FROM " <> tblName <> " WHERE instr(kanji, :kanjiSearch) > 0 AND instr(kana, :kanaSearch)"
          f t = putStrFlush t >> T.strip <$> T.getLine
          g s t | T.null t = s | otherwise = t
      in (,) <$> f "kanji: " <*> f "kana: " >>= \(g kanjiSearchHist *** g kanaSearchHist -> p) ->
          (>> return p) . unless (uncurry (&&) . both T.null $ p) . mapM_ (p &) $ [ queryHelper x | x <- [ "goi", "yonmoji" ] ]
    setCompoundSearchHist searchHistPair
