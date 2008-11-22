{-# OPTIONS_GHC -XFlexibleContexts #-}
module Language.Lsl.Internal.BreakpointsDeserialize(breakpointsElement, module Language.Lsl.Internal.Breakpoint) where

import Control.Monad.Error(MonadError)
import Language.Lsl.Internal.DOMProcessing(ElemAcceptor(..),elementList,findElement,simple)
import Language.Lsl.Internal.Breakpoint(Breakpoint(..),mkBreakpoint)
import Text.XML.HaXml(Content(..),Element(..))

breakpointsElement :: MonadError String m => ElemAcceptor m [Breakpoint]
breakpointsElement = elementList "breakpoints" breakpointElement

breakpointElement :: MonadError String m => ElemAcceptor m Breakpoint
breakpointElement =
    let f (Elem _ _ contents) = do
            (file,contents1) <- findElement (ElemAcceptor "file" simple) [ e | e@(CElem _ _) <- contents ]
            (lineStr,contents2) <- findElement (ElemAcceptor "line" simple) contents1
            return $ mkBreakpoint file (read lineStr) 0
    in ElemAcceptor "breakpoint" f