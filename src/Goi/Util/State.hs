module Goi.Util.State ( getCompoundSearchHist
                      , getSearchHist
                      , getUndo
                      , setCompoundSearchHist
                      , setSearchHist
                      , setUndo ) where

import Goi.Data

import Control.Monad.State (gets, modify)

----------

getUndo :: Stack Undo
getUndo = gets _undo

setUndo :: Undo -> Stack ()
setUndo u = modify $ \gs -> gs { _undo = u }

----------

getSearchHist :: Stack SearchHist
getSearchHist = gets _search

setSearchHist :: SearchHist -> Stack ()
setSearchHist s = modify $ \gs -> gs { _search = s }

----------

getCompoundSearchHist :: Stack (KanjiSearchHist, KanaSearchHist)
getCompoundSearchHist = gets _compoundSearch

setCompoundSearchHist :: (KanjiSearchHist, KanaSearchHist) -> Stack ()
setCompoundSearchHist s = modify $ \gs -> gs { _compoundSearch = s }
