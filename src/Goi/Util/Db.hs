module Goi.Util.Db (withConnection') where

import Goi.Data
import Goi.FilePaths

import Control.Monad.IO.Class (liftIO)
import Database.SQLite.Simple (Connection, withConnection)

----------

withConnection' :: (Connection -> IO a) -> Stack a
withConnection' f = liftIO . flip withConnection f =<< dbFile
