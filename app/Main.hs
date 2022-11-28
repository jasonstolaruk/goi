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
import System.Console.Haskeline (InputT, Settings, defaultSettings, runInputT)
import System.Directory (copyFile)
import System.IO (BufferMode(..), hSetBuffering, stdin)

----------

-- TODO: "Report" command.
-- TODO: Color.
-- TODO: Command to aid with merging a "kanji" file and a "hiragana" file and appending to "goi.txt".
-- TODO: Command to find rows with katakana in the "kanji" column but no katakana in the "kana" column.

main :: IO ()
main = do path :: Env <- head . lines <$> readFile "path"
          void . runInputTToGetIO $ path
  where
    -- Helper functions with explicit type signatures to illustrate how the monad transformer stack is unraveled:
    runInputTToGetIO :: Env -> IO ((), GoiState)
    runInputTToGetIO = runInputT' defaultSettings . runStateTToGetInputT
      where
        runInputT' :: Settings IO -> InputT IO ((), GoiState) -> IO ((), GoiState)
        runInputT' = runInputT

    runStateTToGetInputT :: Env -> InputT IO ((), GoiState)
    runStateTToGetInputT env = let initState = GoiState noUndo T.empty . dup $ T.empty
                               in runStateT' (runReaderTToGetStateT env) initState
      where
        runStateT' :: StateT GoiState (InputT IO) () -> GoiState -> InputT IO ((), GoiState)
        runStateT' = runStateT

    runReaderTToGetStateT :: Env -> StateT GoiState (InputT IO) ()
    runReaderTToGetStateT = runReaderT' f
      where
        runReaderT' :: ReaderT Env (StateT GoiState (InputT IO)) () -> Env -> StateT GoiState (InputT IO) ()
        runReaderT' = runReaderT
        f :: ReaderT Env (StateT GoiState (InputT IO)) () -- Stack ()
        f = go

go :: Stack ()
go = sequence_ [ liftIO . hSetBuffering stdin $ NoBuffering {- TODO: To be deleted. -}, initialize, liftIO . T.putStrLn $ "Welcome to goi.", promptUser ]
  where
    initialize = do liftIO . uncurry copyFile =<< (,) <$> dbFile <*> dbBackupFile; withConnection' $ forM_ qs . execute_
    qs         = map Query [ "CREATE TABLE IF NOT EXISTS goi (id INTEGER PRIMARY KEY, kanji TEXT NOT NULL, kana TEXT NOT NULL, \
                             \read_success INTEGER, read_fail INTEGER, write_success INTEGER, write_fail INTEGER)"
                           , "CREATE TABLE IF NOT EXISTS yonmoji (id INTEGER PRIMARY KEY, kanji TEXT NOT NULL, kana TEXT NOT NULL, \
                             \read_success INTEGER, read_fail INTEGER, write_success INTEGER, write_fail INTEGER)" ]

promptUser :: Stack ()
promptUser = do liftIO . putStrFlush $ "> "; interp =<< liftIO getChar

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
    ----------
    'q'  -> void  . liftIO $ nl
    _    -> next' . liftIO . T.putStrLn $ "?"
  where
    next  = (>> promptUser) -- TODO: Make a helper function for "(>> f)"?
    next' = next . (liftIO nl >>)
