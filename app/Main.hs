{-# LANGUAGE FlexibleContexts, LambdaCase, OverloadedStrings, TypeApplications, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -Werror -Wno-type-defaults #-}

module Main (main) where

import Control.Arrow (second)
import Control.Monad.State (StateT, forM_, get, liftIO, put, runStateT, unless, void)
import Data.Char (toLower)
import Data.Function ((&))
import Data.Text (Text)
import Database.SQLite.Simple (Connection, FromRow, NamedParam(..), Query(..), ToRow, execute, execute_, field, fromRow, toRow, query_, queryNamed, withConnection)
import System.Directory (copyFile)
import qualified Data.Text as T
import qualified Data.Text.IO as T (appendFile, getLine, putStr, putStrLn, readFile)

type Stack = StateT Undo IO
type Undo  = Connection -> IO ()

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

main :: IO ()
main = void . runStateT (sequence_ fs) $ noUndo
  where
    fs = [ liftIO initialize, liftIO . T.putStrLn $ "Welcome to goi.", promptUser ]

noUndo :: Undo
noUndo = const . return $ ()

initialize :: IO ()
initialize = (,) <$> dbFile <*> dbBackupFile >>= \(dbf, buf) -> do
    copyFile dbf buf
    withConnection dbf $ \conn -> forM_ ts $ execute_ conn . Query
  where
    ts = [ "CREATE TABLE IF NOT EXISTS goi (id INTEGER PRIMARY KEY, kanji TEXT NOT NULL, kana TEXT NOT NULL, read_success INTEGER, read_fail INTEGER, write_success INTEGER, write_fail INTEGER)"
         , "CREATE TABLE IF NOT EXISTS yonmoji (id INTEGER PRIMARY KEY, kanji TEXT NOT NULL, kana TEXT NOT NULL, read_success INTEGER, read_fail INTEGER, write_success INTEGER, write_fail INTEGER)" ]

promptUser :: Stack ()
promptUser = do { liftIO . T.putStr $ "> "; liftIO getChar >>= interp }

interp :: Char -> Stack ()
interp = \case
    '\n' -> promptUser
    ' '  -> next  . liftIO $ nl
    '\t' -> next  . liftIO $ nl
    ----------
    'd'  -> next' . liftIO $ dumpGoi
    'D'  -> next' . liftIO $ dumpYonmoji
    ----------
    'l'  -> next' . liftIO $ loadGoi
    'L'  -> next' . liftIO $ loadYonmoji
    ----------
    'r'  -> next' testRead
    'R'  -> next' testReadYonmoji
    '1'  -> next' testReadRandom
    '!'  -> next' testReadYonmojiRandom
    ----------
    'w'  -> next' testWrite
    'W'  -> next' testWriteYonmoji
    '2'  -> next' testWriteRandom
    '@'  -> next' testWriteYonmojiRandom
    ----------
    'u'  -> next' undo
    ----------
    's'  -> next' . liftIO $ search
    ----------
    'q'  -> void  . liftIO $ nl
    _    -> next' . liftIO . T.putStrLn $ "?"
  where
    next  = (>> promptUser)
    next' = next . (liftIO nl >>)

dumpHelper :: Text -> IO ()
dumpHelper tblName = withConnectionHelper $ \conn ->
    mapM_ (\r -> T.putStrLn $ _kanji r <> T.singleton '／' <> _kana r) =<< query_ conn (Query $ "SELECT * FROM " <> tblName)

withConnectionHelper :: (Connection -> IO a) -> IO a
withConnectionHelper f = flip withConnection f =<< dbFile

dumpGoi :: IO ()
dumpGoi = dumpHelper "goi"

dumpYonmoji :: IO ()
dumpYonmoji = dumpHelper "yonmoji"

----------

loadHelper :: Text -> FilePath -> IO ()
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
                        in withConnectionHelper . process . filter (not . T.null) . map T.strip . T.lines =<< T.readFile fn

loadGoi :: IO ()
loadGoi = loadHelper "goi" =<< goiFile

loadYonmoji :: IO ()
loadYonmoji = loadHelper "yonmoji" =<< yonmojiFile

----------

type QueryResult = (Int, Text, Text, Int, Int)

helper :: Text -> (QueryResult -> Text) -> (QueryResult -> Text) -> Text -> Text -> Stack ()
helper queryText questionFun answerFun readOrWrite tblName = liftIO ((,) <$> dbFile <*> logFile) >>= \(dbf, lf) -> do
    maybeUndoer <- liftIO . withConnection dbf $ \conn -> do
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
    mapM_ put maybeUndoer
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

kanji :: QueryResult -> Text
kanji (_, a, _, _, _) = a

kana :: QueryResult -> Text
kana (_, _, a, _, _) = a

----------

testRead :: Stack ()
testRead = helper "SELECT id, kanji, kana, read_success, read_fail FROM (SELECT * FROM goi ORDER BY RANDOM()) ORDER BY read_success LIMIT 1" kanji kana "read" "goi"

testReadYonmoji :: Stack ()
testReadYonmoji = helper "SELECT id, kanji, kana, read_success, read_fail FROM (SELECT * FROM yonmoji ORDER BY RANDOM()) ORDER BY read_success LIMIT 1" kanji kana "read" "yonmoji"

testReadRandom :: Stack ()
testReadRandom = helper "SELECT id, kanji, kana, read_success, read_fail FROM goi ORDER BY RANDOM() LIMIT 1" kanji kana "read" "goi"

testReadYonmojiRandom :: Stack ()
testReadYonmojiRandom = helper "SELECT id, kanji, kana, read_success, read_fail FROM yonmoji ORDER BY RANDOM() LIMIT 1" kanji kana "read" "yonmoji"

----------

testWrite :: Stack ()
testWrite = helper "SELECT id, kanji, kana, write_success, write_fail FROM (SELECT * FROM goi ORDER BY RANDOM()) ORDER BY write_success LIMIT 1" kana kanji "write" "goi"

testWriteYonmoji :: Stack ()
testWriteYonmoji = helper "SELECT id, kanji, kana, write_success, write_fail FROM (SELECT * FROM yonmoji ORDER BY RANDOM()) ORDER BY write_success LIMIT 1" kana kanji "write" "yonmoji"

testWriteRandom :: Stack ()
testWriteRandom = helper "SELECT id, kanji, kana, write_success, write_fail FROM goi ORDER BY RANDOM() LIMIT 1" kana kanji "write" "goi"

testWriteYonmojiRandom :: Stack ()
testWriteYonmojiRandom = helper "SELECT id, kanji, kana, write_success, write_fail FROM yonmoji ORDER BY RANDOM() LIMIT 1" kana kanji "write" "yonmoji"

----------

undo :: Stack ()
undo = do { liftIO . T.putStrLn $ "Undoing."; liftIO . withConnectionHelper =<< get; put noUndo }

----------

search :: IO ()
search = withConnectionHelper $ \conn ->
    let f tblName col t | q  <- Query . T.concat $ [ "SELECT id, kanji, kana FROM ", tblName, " WHERE instr(", col, ", :t) > 0" ] = do
            rs <- queryNamed conn q . pure $ ":t" := t :: IO [(Int, Text, Text)]
            T.putStrLn . T.concat $ [ tblName, " - ", col, ":" ]
            forM_ rs $ \(i, kanjiText, kanaText) -> T.putStrLn . T.intercalate " / " $ [ showText i, kanjiText, kanaText ]
    in T.putStr "| " >> T.getLine >>= \t -> unless (T.null t) . mapM_ (t &) $ [ f x y | x <- [ "goi", "yonmoji" ], y <- [ "kanji", "kana" ] ]

----------

nl :: IO ()
nl = T.putStr . T.singleton $ '\n'

mkPath :: FilePath -> IO FilePath
mkPath xs = (++ xs) . head . lines <$> readFile "path"

dbFile :: IO FilePath
dbFile = mkPath "goi.db"

dbBackupFile :: IO FilePath
dbBackupFile = mkPath "goi.db.bak"

goiFile :: IO FilePath
goiFile = mkPath "goi.txt"

yonmojiFile :: IO FilePath
yonmojiFile = mkPath "yonmoji.txt"

logFile :: IO FilePath
logFile = mkPath "log.txt"

split :: Char -> Text -> (Text, Text)
split (T.singleton -> c) = second T.tail . T.breakOn c

showText :: Show a => a -> Text
showText = T.pack . show

{-
both :: (a -> b) -> (a, a) -> (b, b)
both = join (***)

mIf :: Monad m => m Bool -> m a -> m a -> m a
mIf p (flip bool -> f) = (p >>=) . f
-}
