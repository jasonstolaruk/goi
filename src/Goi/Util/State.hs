module Goi.Util.State ( getCompoundSearch
                      , getSearch
                      , getUndo
                      , setCompoundSearch
                      , setSearch
                      , setUndo ) where

import Goi.Data

import Control.Monad.State (gets, modify)

----------

getUndo :: Stack Undo
getUndo = gets _undo

-- TODO: Record update syntax improvements?
setUndo :: Undo -> Stack ()
setUndo u = modify $ \gs -> gs { _undo = u }

----------

getSearch :: Stack Search
getSearch = gets _search

setSearch :: Search -> Stack ()
setSearch s = modify $ \gs -> gs { _search = s }

----------

getCompoundSearch :: Stack (KanjiSearch, KanaSearch)
getCompoundSearch = gets _compoundSearch

setCompoundSearch :: (KanjiSearch, KanaSearch) -> Stack ()
setCompoundSearch s = modify $ \gs -> gs { _compoundSearch = s }
