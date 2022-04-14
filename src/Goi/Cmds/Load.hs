module Goi.Cmds.Load ( loadGoiCmd
                     , loadYonmojiCmd ) where

import Goi.Data
import Goi.FilePaths
import Goi.Util.Db
import Goi.Util.Misc

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Database.SQLite.Simple (Query(..), execute)
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)

----------

loadGoiCmd :: Stack ()
loadGoiCmd = loadHelper "goi" =<< goiFile

loadYonmojiCmd :: Stack ()
loadYonmojiCmd = loadHelper "yonmoji" =<< yonmojiFile

-- TODO: Prompt yes/no.
loadHelper :: Text -> FilePath -> Stack ()
loadHelper tblName fn = let process ts conn = mapM_ f ts
                              where
                                f t | T.singleton '／' `T.isInfixOf` t = insert '／' t
                                    | otherwise = insert '　' t
                                insert c t = let (kanjiText, kanaText) = split c t
                                                 r = Record { _kanji        = kanjiText
                                                            , _kana         = kanaText
                                                            , _readSuccess  = 0
                                                            , _readFail     = 0
                                                            , _writeSuccess = 0
                                                            , _writeFail    = 0 }
                                                 q = Query $ "INSERT INTO " <> tblName <> " (kanji, kana, read_success, read_fail, write_success, write_fail) VALUES (?, ?, ?, ?, ?, ?)"
                                             in execute conn q r
                        in withConnection' . process . filter (not . T.null) . map T.strip . T.lines =<< liftIO (T.readFile fn)
