module Goi.FilePaths ( dbBackupFile
                     , dbFile
                     , goiFile
                     , logFile
                     , yonmojiFile ) where

import Goi.Data

import Control.Monad.Reader (asks)

----------

mkPath :: FilePath -> Stack FilePath
mkPath xs = asks (++ xs)

dbFile :: Stack FilePath
dbFile = mkPath "goi.db"

dbBackupFile :: Stack FilePath
dbBackupFile = mkPath "goi.db.bak"

goiFile :: Stack FilePath
goiFile = mkPath "goi.txt"

yonmojiFile :: Stack FilePath
yonmojiFile = mkPath "yonmoji.txt"

logFile :: Stack FilePath
logFile = mkPath "log.txt"
