module Goi.Cmds.Dump ( dumpGoiCmd
                     , dumpYonmojiCmd ) where

import Goi.Data
import Goi.Util.Db

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Database.SQLite.Simple (Query(..), query_)
import qualified Data.Text as T
import qualified Data.Text.IO as T (putStrLn)

----------

dumpGoiCmd :: Stack ()
dumpGoiCmd = dumpHelper "goi"

dumpYonmojiCmd :: Stack ()
dumpYonmojiCmd = dumpHelper "yonmoji"

dumpHelper :: Text -> Stack ()
dumpHelper tblName = withConnection' (flip query_ . Query $ "SELECT * FROM " <> tblName) >>=
    let f r = liftIO . T.putStrLn $ _kanji r <> T.singleton '／' <> _kana r in mapM_ f
