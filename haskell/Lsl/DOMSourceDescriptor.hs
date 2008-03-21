module Lsl.DOMSourceDescriptor(sourceFiles,sourceFilesElement) where

import Control.Monad
import Lsl.DOMProcessing
import Text.XML.HaXml hiding (when)
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Pretty
import Lsl.Util

sourceFiles e = match sourceFilesElement e

sourceFilesElement :: Monad m => ElemAcceptor m ([(String,String)],[(String,String)])
sourceFilesElement = 
    let f (Elem _ _ contents) = do
            (m,contents1) <- findElement modulesElement [ e | e@(CElem _ _) <- contents ]
            (s,[]) <- findElement scriptsElement contents1
            return (m,s)
    in ElemAcceptor "source_files" f

modulesElement :: Monad m => ElemAcceptor m [(String,String)]
modulesElement = elementList "modules" itemElement
scriptsElement :: Monad m => ElemAcceptor m [(String,String)]
scriptsElement = elementList "scripts" itemElement

itemElement :: Monad m => ElemAcceptor m (String,String)
itemElement =
    let f (Elem _ _ contents) = do
            (id,contents1) <- findElement idElement [ e | e@(CElem _ _) <- contents ]
            (p,[]) <- findElement pathElement contents1
            return (id,p)
    in ElemAcceptor "item" f

idElement :: Monad m => ElemAcceptor m String
idElement = ElemAcceptor "identifier" simple
pathElement :: Monad m => ElemAcceptor m String
pathElement = ElemAcceptor "path" simple
