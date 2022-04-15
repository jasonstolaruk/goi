{-# LANGUAGE MonadComprehensions, OverloadedStrings, TupleSections, ViewPatterns #-}

module Goi.Cmds.Search ( compoundSearchCmd
                       , searchCmd ) where

import Goi.Data
import Goi.FilePaths
import Goi.Util.Db
import Goi.Util.Misc
import Goi.Util.State

import Control.Arrow ((***))
import Control.Monad (forM_, unless)
import Data.Function ((&))
import Data.Text (Text)
import Database.SQLite.Simple (NamedParam(..), Query(..), queryNamed)
import qualified Data.Text as T
import qualified Data.Text.IO as T (getLine, putStr, putStrLn, readFile)

----------

-- TODO: Search commands should support backspace etc.

searchCmd :: Stack ()
searchCmd = (,) <$> goiFile <*> yonmojiFile >>= \(("goi.txt", ) *** ("yonmoji.txt", ) -> (goiPair, yonmojiPair)) -> do
    searchText  <- getSearch
    searchText' <- withConnection' $ \conn ->
      let queryHelper tblName col t | q  <- Query . T.concat $ [ "SELECT id, kanji, kana FROM ", tblName, " WHERE instr(", col, ", :t) > 0" ] = do
              T.putStrLn . T.concat $ [ tblName, " - ", col, ":" ]
              rs <- queryNamed conn q . pure $ ":t" := t :: IO [(Int, Text, Text)]
              forM_ rs $ \(i, kanjiText, kanaText) -> T.putStrLn . T.intercalate " / " $ [ showText i, kanjiText, kanaText ]
          fileHelper t (n, fn) = do { T.putStrLn $ n <> ":"; mapM_ T.putStrLn . filter (t `T.isInfixOf`) . T.lines =<< T.readFile fn }
      in do T.putStr "| "
            t <- [ f t | let f (T.strip -> t') | T.null t' = searchText | otherwise = t', t <- T.getLine ]
            unless (T.null t) $ do
                mapM_ (t &) $ [ queryHelper x y | x <- [ "goi", "yonmoji" ], y <- [ "kanji", "kana" ] ]
                mapM_ (fileHelper t) [ goiPair, yonmojiPair ]
            return t
    setSearch searchText'

compoundSearchCmd :: Stack ()
compoundSearchCmd = getCompoundSearch >>= \(kanjiSearch, kanaSearch) -> do
    searchPair <- withConnection' $ \conn ->
      let queryHelper tblName (t1, t2) = do
              T.putStrLn $ tblName <> ":"
              rs <- queryNamed conn q [ ":kanjiSearch" := t1, ":kanaSearch" := t2 ] :: IO [(Int, Text, Text)]
              forM_ rs $ \(i, kanjiText, kanaText) -> T.putStrLn . T.intercalate " / " $ [ showText i, kanjiText, kanaText ]
            where
              q = Query $ "SELECT id, kanji, kana FROM " <> tblName <> " WHERE instr(kanji, :kanjiSearch) > 0 AND instr(kana, :kanaSearch)"
          f t = T.putStr t >> T.strip <$> T.getLine
          g s t | T.null t = s | otherwise = t
      in (,) <$> f "kanji: " <*> f "kana: " >>= \(g kanjiSearch *** g kanaSearch -> p) ->
          (>> return p) . unless (uncurry (&&) . both T.null $ p) . mapM_ (p &) $ [ queryHelper x | x <- [ "goi", "yonmoji" ] ]
    setCompoundSearch searchPair
