module Lsl.ValueDB where

import Control.Monad
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import Data.List
import Lsl.Type

data ValueDB = ValueDB (Map String (Either ValueDB LSLValue))
    deriving (Show)

emptyDB = ValueDB M.empty

insertDB [] value db = error "empty path"
insertDB [n] value (ValueDB map) = ValueDB $ M.insert n (Right value) map
insertDB (n:ns) value (ValueDB map) =
    case M.lookup n map of
        Nothing -> ValueDB $ M.insert n (Left $ insertDB ns value emptyDB) map
        Just (Right _) -> ValueDB $ M.insert n (Left $ insertDB ns value emptyDB) map
        Just (Left vdb) -> ValueDB $ M.insert n (Left $ insertDB ns value vdb) map
        
lookupDB [] db = error "empty path"
lookupDB [n] (ValueDB map) = M.lookup n map
lookupDB (n:ns) (ValueDB map) = 
    case M.lookup n map of
        Nothing -> fail "not found"
        Just (Right value) -> fail "not found"
        Just (Left vdb) -> lookupDB ns vdb