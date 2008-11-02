{-# OPTIONS_GHC -XFlexibleContexts #-}
module Language.Lsl.Internal.DOMSourceDescriptor(sourceFiles,sourceFilesElement) where

import Control.Monad.Error(MonadError(..))
import Language.Lsl.Internal.DOMProcessing(ElemAcceptor(..),elementList,findElement,match,simple)
import Text.XML.HaXml(Element(..),Content(..))

sourceFiles e = match sourceFilesElement e

sourceFilesElement :: MonadError String m => ElemAcceptor m ([(String,String)],[(String,String)])
sourceFilesElement = 
    let f (Elem _ _ contents) = do
            (m,contents1) <- findElement modulesElement [ e | e@(CElem _ _) <- contents ]
            (s,[]) <- findElement scriptsElement contents1
            return (m,s)
    in ElemAcceptor "source_files" f

modulesElement :: MonadError String m => ElemAcceptor m [(String,String)]
modulesElement = elementList "modules" itemElement
scriptsElement :: MonadError String m => ElemAcceptor m [(String,String)]
scriptsElement = elementList "scripts" itemElement

itemElement :: MonadError String m => ElemAcceptor m (String,String)
itemElement =
    let f (Elem _ _ contents) = do
            (id,contents1) <- findElement idElement [ e | e@(CElem _ _) <- contents ]
            (p,[]) <- findElement pathElement contents1
            return (id,p)
    in ElemAcceptor "item" f

idElement :: MonadError String m => ElemAcceptor m String
idElement = ElemAcceptor "identifier" simple
pathElement :: MonadError String m => ElemAcceptor m String
pathElement = ElemAcceptor "path" simple
