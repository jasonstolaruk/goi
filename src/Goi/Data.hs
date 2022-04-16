{-# LANGUAGE TypeApplications #-}

module Goi.Data ( Env
                , GoiState(..)
                , KanaSearchHist
                , KanjiSearchHist
                , Record(..)
                , SearchHist
                , Stack
                , Undo ) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Data.Text (Text)
import Database.SQLite.Simple (Connection, FromRow, ToRow, field, fromRow, toRow)
import System.Console.Haskeline (InputT)

----------

type Stack = ReaderT Env (StateT GoiState (InputT IO))

type Env = FilePath

data GoiState = GoiState { _undo           :: Undo
                         , _search         :: SearchHist
                         , _compoundSearch :: (KanjiSearchHist, KanaSearchHist) }

type Undo            = Connection -> IO ()
type SearchHist      = Text
type KanjiSearchHist = Text
type KanaSearchHist  = Text

----------

data Record = Record { _kanji        :: Text
                     , _kana         :: Text
                     , _readSuccess  :: Int
                     , _readFail     :: Int
                     , _writeSuccess :: Int
                     , _writeFail    :: Int }

instance ToRow Record where
  toRow (Record a b c d e f) = toRow (a, b, c, d, e, f)

instance FromRow Record where
  fromRow = Record <$ field @Int <*> field <*> field <*> field <*> field <*> field <*> field
