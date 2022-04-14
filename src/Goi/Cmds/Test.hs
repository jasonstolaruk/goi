{-# LANGUAGE LambdaCase, ViewPatterns #-}

module Goi.Cmds.Test ( testReadCmd
                     , testReadRandomCmd
                     , testReadYonmojiCmd
                     , testReadYonmojiRandomCmd
                     , testWriteCmd
                     , testWriteRandomCmd
                     , testWriteYonmojiCmd
                     , testWriteYonmojiRandomCmd ) where

import Goi.Data
import Goi.FilePaths
import Goi.Util.Db
import Goi.Util.Misc
import Goi.Util.State

import Data.Char (toLower)
import Data.Text (Text)
import Database.SQLite.Simple (Connection, Query(..), execute, query_)
import qualified Data.Text as T
import qualified Data.Text.IO as T (appendFile, putStr, putStrLn)

----------

type QueryResult = (Int, Text, Text, Int, Int)

kanji :: QueryResult -> Text
kanji (_, a, _, _, _) = a

kana :: QueryResult -> Text
kana (_, _, a, _, _) = a

----------

helper :: Text -> (QueryResult -> Text) -> (QueryResult -> Text) -> Text -> Text -> Stack ()
helper queryText questionFun answerFun readOrWrite tblName = logFile >>= \lf -> do
    maybeUndoer <- withConnection' $ \conn -> do
        rs <- query_ conn . Query $ queryText
        case rs of []    -> return Nothing
                   (r:_) -> do T.putStr . questionFun $ r
                               checkForCancel . const $ do
                                   T.putStrLn . answerFun $ r
                                   T.putStr $ "Could you " <> readOrWrite <> " it? "
                                   checkForCancel $ \c -> do
                                       (s, f, undoer) <- update conn r c
                                       nl
                                       T.putStrLn . T.concat $ [ showText s, " successes, ", showText f, " failures." ]
                                       T.appendFile lf . (<> T.singleton '\n') . kana $ r
                                       return . return $ undoer
    mapM_ setUndo maybeUndoer
  where
    checkForCancel :: (Char -> IO (Maybe Undo)) -> IO (Maybe Undo)
    checkForCancel f = getChar >>= \(toLower -> c) -> if c == 'c' then nl >> T.putStrLn "Cancelling." >> pure mempty else f c
    update :: Connection -> QueryResult -> Char -> IO (Int, Int, Undo)
    update conn (i, _, _, s, f) = \case
      'y' | s' <- succ s
          , q  <- Query . T.concat $ [ "UPDATE ", tblName, " SET ", readOrWrite, "_success = ? WHERE id = ?" ]
          -> execute conn q (s', i) >> return (s', f, \conn' -> execute conn' q (s, i))
      _   | f' <- succ f
          , q  <- Query . T.concat $ [ "UPDATE ", tblName, " SET ", readOrWrite, "_fail    = ? WHERE id = ?" ]
          -> execute conn q (f', i) >> return (s, f', \conn' -> execute conn' q (f, i))

----------

testReadCmd :: Stack ()
testReadCmd = helper "SELECT id, kanji, kana, read_success, read_fail FROM (SELECT * FROM goi ORDER BY RANDOM()) ORDER BY read_success LIMIT 1" kanji kana "read" "goi"

testReadYonmojiCmd :: Stack ()
testReadYonmojiCmd = helper "SELECT id, kanji, kana, read_success, read_fail FROM (SELECT * FROM yonmoji ORDER BY RANDOM()) ORDER BY read_success LIMIT 1" kanji kana "read" "yonmoji"

testReadRandomCmd :: Stack ()
testReadRandomCmd = helper "SELECT id, kanji, kana, read_success, read_fail FROM goi ORDER BY RANDOM() LIMIT 1" kanji kana "read" "goi"

testReadYonmojiRandomCmd :: Stack ()
testReadYonmojiRandomCmd = helper "SELECT id, kanji, kana, read_success, read_fail FROM yonmoji ORDER BY RANDOM() LIMIT 1" kanji kana "read" "yonmoji"

----------

testWriteCmd :: Stack ()
testWriteCmd = helper "SELECT id, kanji, kana, write_success, write_fail FROM (SELECT * FROM goi ORDER BY RANDOM()) ORDER BY write_success LIMIT 1" kana kanji "write" "goi"

testWriteYonmojiCmd :: Stack ()
testWriteYonmojiCmd = helper "SELECT id, kanji, kana, write_success, write_fail FROM (SELECT * FROM yonmoji ORDER BY RANDOM()) ORDER BY write_success LIMIT 1" kana kanji "write" "yonmoji"

testWriteRandomCmd :: Stack ()
testWriteRandomCmd = helper "SELECT id, kanji, kana, write_success, write_fail FROM goi ORDER BY RANDOM() LIMIT 1" kana kanji "write" "goi"

testWriteYonmojiRandomCmd :: Stack ()
testWriteYonmojiRandomCmd = helper "SELECT id, kanji, kana, write_success, write_fail FROM yonmoji ORDER BY RANDOM() LIMIT 1" kana kanji "write" "yonmoji"
