{-# LANGUAGE FlexibleContexts, LambdaCase, MonadComprehensions, OverloadedStrings, TupleSections, TypeApplications, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -Werror -Wno-type-defaults #-}

module Main (main) where

import Control.Arrow ((***), (&&&), second)
import Control.Monad.State
import Control.Monad.Reader
import Data.Char (toLower)
import Data.Function ((&))
import Data.Text (Text)
import Database.SQLite.Simple (Connection, FromRow, NamedParam(..), Query(..), ToRow, execute, execute_, field, fromRow, toRow, query_, queryNamed, withConnection)
import System.Directory (copyFile)
import qualified Data.Text as T
import qualified Data.Text.IO as T (appendFile, getLine, putStr, putStrLn, readFile)

type Stack = ReaderT Env (StateT GoiState IO)
type Env   = FilePath
type Undo  = Connection -> IO ()

data GoiState = GoiState { _undo           :: Undo
                         , _search         :: Search
                         , _compoundSearch :: (KanjiSearch, KanaSearch) }

type Search      = Text
type KanjiSearch = Text
type KanaSearch  = Text

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

----------

{- HLINT ignore "Redundant <$>" -}
main :: IO ()
main = head . lines <$> readFile "path" >>= \path ->
    void . runStateT (runReaderT (sequence_ fs) path) . GoiState noUndo T.empty . dup $ T.empty
  where
    fs = [ initialize, liftIO . T.putStrLn $ "Welcome to goi.", promptUser ]

noUndo :: Undo
noUndo = const . return $ ()

initialize :: Stack ()
initialize = do { liftIO . uncurry copyFile =<< (,) <$> dbFile <*> dbBackupFile; withConnection' $ forM_ qs . execute_ }
  where
    qs = map Query [ "CREATE TABLE IF NOT EXISTS goi (id INTEGER PRIMARY KEY, kanji TEXT NOT NULL, kana TEXT NOT NULL, \
                     \read_success INTEGER, read_fail INTEGER, write_success INTEGER, write_fail INTEGER)"
                   , "CREATE TABLE IF NOT EXISTS yonmoji (id INTEGER PRIMARY KEY, kanji TEXT NOT NULL, kana TEXT NOT NULL, \
                     \read_success INTEGER, read_fail INTEGER, write_success INTEGER, write_fail INTEGER)" ]

withConnection' :: (Connection -> IO a) -> Stack a
withConnection' f = liftIO . flip withConnection f =<< dbFile

promptUser :: Stack ()
promptUser = do { liftIO . T.putStr $ "> "; liftIO getChar >>= interp }

interp :: Char -> Stack ()
interp = \case
    '\n' -> promptUser
    ' '  -> next . liftIO $ nl
    '\t' -> next . liftIO $ nl
    ----------
    'd'  -> next' dumpGoi
    'D'  -> next' dumpYonmoji
    ----------
    'l'  -> next' loadGoi
    'L'  -> next' loadYonmoji
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
    's'  -> next' search
    'S'  -> next' compoundSearch
    ----------
    'q'  -> void  . liftIO $ nl
    _    -> next' . liftIO . T.putStrLn $ "?"
  where
    next  = (>> promptUser)
    next' = next . (liftIO nl >>)

----------

dumpHelper :: Text -> Stack ()
dumpHelper tblName = withConnection' (flip query_ . Query $ "SELECT * FROM " <> tblName) >>=
    let f r = liftIO . T.putStrLn $ _kanji r <> T.singleton '／' <> _kana r in mapM_ f

dumpGoi :: Stack ()
dumpGoi = dumpHelper "goi"

dumpYonmoji :: Stack ()
dumpYonmoji = dumpHelper "yonmoji"

----------

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

loadGoi :: Stack ()
loadGoi = loadHelper "goi" =<< goiFile

loadYonmoji :: Stack ()
loadYonmoji = loadHelper "yonmoji" =<< yonmojiFile

----------

type QueryResult = (Int, Text, Text, Int, Int)

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
undo = do { liftIO . T.putStrLn $ "Undoing."; withConnection' =<< getUndo; setUndo noUndo }

----------

search :: Stack ()
search = (,) <$> goiFile <*> yonmojiFile >>= \(("goi.txt", ) *** ("yonmoji.txt", ) -> (goiPair, yonmojiPair)) -> do
    searchText  <- getSearch
    searchText' <- withConnection' $ \conn ->
      let queryHelper tblName col t | q  <- Query . T.concat $ [ "SELECT id, kanji, kana FROM ", tblName, " WHERE instr(", col, ", :t) > 0" ] = do
              T.putStrLn . T.concat $ [ tblName, " - ", col, ":" ]
              rs <- queryNamed conn q . pure $ ":t" := t :: IO [(Int, Text, Text)]
              forM_ rs $ \(i, kanjiText, kanaText) -> T.putStrLn . T.intercalate " / " $ [ showText i, kanjiText, kanaText ]
          fileHelper t (n, fn) = do { T.putStrLn $ n <> ":"; mapM_ T.putStrLn =<< filter (t `T.isInfixOf`) . T.lines <$> T.readFile fn }
      in do T.putStr "| "
            t <- [ f t | let f (T.strip -> t') | T.null t' = searchText | otherwise = t', t <- T.getLine ]
            unless (T.null t) $ do
                mapM_ (t &) $ [ queryHelper x y | x <- [ "goi", "yonmoji" ], y <- [ "kanji", "kana" ] ]
                mapM_ (fileHelper t) [ goiPair, yonmojiPair ]
            return t
    setSearch searchText'

compoundSearch :: Stack ()
compoundSearch = getCompoundSearch >>= \(kanjiSearch, kanaSearch) -> do
    searchPair <- withConnection' $ \conn ->
      let queryHelper tblName (t1, t2) = do
              T.putStrLn $ tblName <> ":"
              rs <- queryNamed conn q [ ":kanjiSearch" := t1, ":kanaSearch" := t2 ] :: IO [(Int, Text, Text)]
              forM_ rs $ \(i, kanjiText, kanaText) -> T.putStrLn . T.intercalate " / " $ [ showText i, kanjiText, kanaText ]
            where
              q = Query $ "SELECT id, kanji, kana FROM " <> tblName <> " WHERE instr(kanji, :kanjiSearch) > 0 AND instr(kana, :kanaSearch)"
          f t = T.putStr t >> T.strip <$> T.getLine
          g s t | T.null t = s | otherwise = t
      in (,) <$> f "kanji: " <*> f "kana: " >>= \(g kanjiSearch *** g kanaSearch -> p) ->
          (>> return p) . unless (uncurry (&&) . both T.null $ p) . mapM_ (p &) $ [ queryHelper x | x <- [ "goi", "yonmoji" ] ]
    setCompoundSearch searchPair

----------

getUndo :: Stack Undo
getUndo = gets _undo

setUndo :: Undo -> Stack ()
setUndo u = modify $ \gs -> gs { _undo = u }

getSearch :: Stack Search
getSearch = gets _search

setSearch :: Search -> Stack ()
setSearch s = modify $ \gs -> gs { _search = s }

getCompoundSearch :: Stack (KanjiSearch, KanaSearch)
getCompoundSearch = gets _compoundSearch

setCompoundSearch :: (KanjiSearch, KanaSearch) -> Stack ()
setCompoundSearch s = modify $ \gs -> gs { _compoundSearch = s }

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

----------

both :: (a -> b) -> (a, a) -> (b, b)
both = join (***)

dup :: a -> (a, a)
dup = id &&& id

nl :: IO ()
nl = T.putStr . T.singleton $ '\n'

showText :: Show a => a -> Text
showText = T.pack . show

split :: Char -> Text -> (Text, Text)
split (T.singleton -> c) = second T.tail . T.breakOn c

{-
((+1) *** (*5)) (0,1) -- (1,5)
(succ &&& pred) 1 -- (2,0)
-}
