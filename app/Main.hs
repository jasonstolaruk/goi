{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Goi.Cmds.Dump
import Goi.Cmds.Load
import Goi.Cmds.Search
import Goi.Cmds.Test
import Goi.Cmds.Undo
import Goi.Data
import Goi.FilePaths
import Goi.Util.Db
import Goi.Util.Misc

import Control.Monad.Reader
import Control.Monad.State
import Database.SQLite.Simple (Query(..), execute_)
import qualified Data.Text as T
import qualified Data.Text.IO as T (putStr, putStrLn)
import System.Directory (copyFile)

----------

-- TODO: "Report" command.
-- TODO: Command to aid with merging a "kanji" file and a "hiragana" file and appending to "goi.txt".
-- TODO: Command to find rows with katakana in the "kanji" column but no katakana in the "kana" column.

main :: IO ()
main = f . head . lines =<< readFile "path"
  where
    f path = let fs = [ initialize, liftIO . T.putStrLn $ "Welcome to goi.", promptUser ]
             in void . runStateT (runReaderT (sequence_ fs) path) . GoiState noUndo T.empty . dup $ T.empty

initialize :: Stack ()
initialize = do { liftIO . uncurry copyFile =<< (,) <$> dbFile <*> dbBackupFile; withConnection' $ forM_ qs . execute_ }
  where
    qs = map Query [ "CREATE TABLE IF NOT EXISTS goi (id INTEGER PRIMARY KEY, kanji TEXT NOT NULL, kana TEXT NOT NULL, \
                     \read_success INTEGER, read_fail INTEGER, write_success INTEGER, write_fail INTEGER)"
                   , "CREATE TABLE IF NOT EXISTS yonmoji (id INTEGER PRIMARY KEY, kanji TEXT NOT NULL, kana TEXT NOT NULL, \
                     \read_success INTEGER, read_fail INTEGER, write_success INTEGER, write_fail INTEGER)" ]

promptUser :: Stack ()
promptUser = do { liftIO . T.putStr $ "> "; liftIO getChar >>= interp }

interp :: Char -> Stack ()
interp = \case
    '\n' -> promptUser
    ' '  -> next . liftIO $ nl
    '\t' -> next . liftIO $ nl
    ----------
    'd'  -> next' dumpGoiCmd
    'D'  -> next' dumpYonmojiCmd
    ----------
    'l'  -> next' loadGoiCmd
    'L'  -> next' loadYonmojiCmd
    ----------
    'r'  -> next' testReadCmd
    'R'  -> next' testReadYonmojiCmd
    '1'  -> next' testReadRandomCmd
    '!'  -> next' testReadYonmojiRandomCmd
    ----------
    'w'  -> next' testWriteCmd
    'W'  -> next' testWriteYonmojiCmd
    '2'  -> next' testWriteRandomCmd
    '@'  -> next' testWriteYonmojiRandomCmd
    ----------
    'u'  -> next' undoCmd
    ----------
    's'  -> next' searchCmd
    'S'  -> next' compoundSearchCmd
    ----------
    'q'  -> void  . liftIO $ nl
    _    -> next' . liftIO . T.putStrLn $ "?"
  where
    next  = (>> promptUser)
    next' = next . (liftIO nl >>)

----------

{-
((+1) *** (*5)) (0,1) -- (1,5)
(succ &&& pred) 1 -- (2,0)
-}
