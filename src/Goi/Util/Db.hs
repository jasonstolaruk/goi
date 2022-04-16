module Goi.Util.Db (withConnection') where

import Goi.Data
import Goi.FilePaths

import Control.Monad.IO.Class (liftIO)
import Database.SQLite.Simple (Connection, withConnection)

----------

-- TODO: Could "(Connection -> IO a)" be changed to "(Connection -> InputT IO a)"?
withConnection' :: (Connection -> IO a) -> Stack a
withConnection' f = liftIO . flip withConnection f =<< dbFile
