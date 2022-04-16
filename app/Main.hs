{-# LANGUAGE LambdaCase, OverloadedStrings, ScopedTypeVariables #-}

module Main (main) where

import Goi.Cmds.Dump
import Goi.Cmds.Load
import Goi.Cmds.Search
import Goi.Cmds.TestReadWrite
import Goi.Cmds.Undo
import Goi.Data
import Goi.FilePaths
import Goi.Util.Db
import Goi.Util.IO
import Goi.Util.Misc

import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, runStateT)
import Database.SQLite.Simple (Query(..), execute_)
import qualified Data.Text as T
import qualified Data.Text.IO as T (putStrLn)
import System.Directory (copyFile)
import System.IO (BufferMode(..), hSetBuffering, stdin)

----------

-- TODO: "Report" command.
-- TODO: Command to aid with merging a "kanji" file and a "hiragana" file and appending to "goi.txt".
-- TODO: Command to find rows with katakana in the "kanji" column but no katakana in the "kana" column.
-- TODO: Color?

main :: IO ()
main = do path :: Env <- head . lines <$> readFile "path"
          void . getIO $ path
  where
    getIO :: Env -> IO ((), GoiState)
    getIO env = let initState = GoiState noUndo T.empty . dup $ T.empty
                in runStateT' (getStateT env) initState

    runStateT' :: StateT GoiState IO () -> GoiState -> IO ((), GoiState)
    runStateT' = runStateT

    getStateT :: Env -> StateT GoiState IO ()
    getStateT = runReaderT' f
      where
        f :: ReaderT Env (StateT GoiState IO) () -- Stack ()
        f = sequence_ [ liftIO . hSetBuffering stdin $ NoBuffering, initialize, liftIO . T.putStrLn $ "Welcome to goi.", promptUser ]

    runReaderT' :: ReaderT Env (StateT GoiState IO) () -> Env -> StateT GoiState IO ()
    runReaderT' = runReaderT

initialize :: Stack ()
initialize = do { liftIO . uncurry copyFile =<< (,) <$> dbFile <*> dbBackupFile; withConnection' $ forM_ qs . execute_ }
  where
    qs = map Query [ "CREATE TABLE IF NOT EXISTS goi (id INTEGER PRIMARY KEY, kanji TEXT NOT NULL, kana TEXT NOT NULL, \
                     \read_success INTEGER, read_fail INTEGER, write_success INTEGER, write_fail INTEGER)"
                   , "CREATE TABLE IF NOT EXISTS yonmoji (id INTEGER PRIMARY KEY, kanji TEXT NOT NULL, kana TEXT NOT NULL, \
                     \read_success INTEGER, read_fail INTEGER, write_success INTEGER, write_fail INTEGER)" ]

promptUser :: Stack ()
promptUser = do { liftIO . putStrFlush $ "> "; liftIO getChar >>= interp }

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
