{-# LANGUAGE OverloadedStrings #-}

module Goi.Cmds.Undo ( noUndo
                     , undoCmd ) where

import Goi.Data
import Goi.Util.Db
import Goi.Util.State

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.IO as T (putStrLn)

----------

undoCmd :: Stack ()
undoCmd = do { liftIO . T.putStrLn $ "Undoing."; withConnection' =<< getUndo; setUndo noUndo }

noUndo :: Undo
noUndo = const . return $ ()
