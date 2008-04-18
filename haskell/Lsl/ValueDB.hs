module Lsl.ValueDB(ValueDB,emptyDB,insertDB,insertAllDB,lookupDB,dbFromDomElement,dbAcceptor) where

import Control.Monad
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import Data.List
import Lsl.Type
import Lsl.DOMProcessing
import Lsl.Util
import Text.XML.HaXml hiding (when)
import Text.XML.HaXml.Posn

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

insertAllDB pairs db = foldl (flip (uncurry insertDB)) db pairs

dbFromDomElement name e = match (dbAcceptor name) e

dbAcceptor :: Monad m => String -> ElemAcceptor m ValueDB
dbAcceptor name = ElemAcceptor name acceptValueDB
acceptValueDB (Elem _ _ contents) = foldM (cmatch . entryAcceptor) emptyDB (elementsOnly contents)

entryAcceptor :: Monad m => ValueDB -> ElemAcceptor m ValueDB
entryAcceptor (ValueDB m) = ElemAcceptor "entry" $ 
    \ (Elem _ _ contents) -> do
        (key,c1) <- findSimple "key" (elementsOnly contents)
        (value,[]) <- findElement valueElement c1
        return $ ValueDB (M.insert key value m)

valueElement :: (Monad m, MonadPlus m) => ElemAcceptor m (Either ValueDB LSLValue)
valueElement = ElemAcceptor "value" matchValue
matchValue e = (matchDbValue e) `mplus` (matchLslValue e)
matchDbValue e = match dbValueElement e >>= return . Left
matchLslValue e = match lslValueElement e >>= return . Right

dbValueElement :: Monad m => ElemAcceptor m ValueDB
dbValueElement =
     let f e@(Elem _ attrs contents) = do
            valType <- lookupM "class" attrs
            case attrString valType of
                "valueDB" -> acceptValueDB e >>= return 
                _ -> fail "unrecognized element"
     in ElemAcceptor "value" f

acceptVector (Elem _ _ contents) = do
            (x,c1) <- findValue "x" (elementsOnly contents)
            (y,c2) <- findValue "y" (elementsOnly c1)
            (z,[]) <- findValue "z" (elementsOnly c2)
            return $ VVal x y z
            
acceptRotation (Elem _ _ contents) = do
            (x,c1) <- findValue "x" (elementsOnly contents)
            (y,c2) <- findValue "y" (elementsOnly c1)
            (z,c3) <- findValue "z" (elementsOnly c2)
            (s,[]) <- findValue "s" (elementsOnly c3)
            return $ RVal x y z s
        
acceptLslList e = do
        l <- acceptList lslValueElement e
        return $ LVal l
