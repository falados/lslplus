module Lsl.BreakpointsDeserialize(breakpointsElement, module Lsl.Breakpoint) where

import Control.Monad
import Lsl.DOMProcessing
import Lsl.Breakpoint
import Lsl.Util
import Text.XML.HaXml hiding (when)
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Pretty

breakpointsElement :: Monad m => ElemAcceptor m [Breakpoint]
breakpointsElement = elementList "breakpoints" breakpointElement

breakpointElement :: Monad m => ElemAcceptor m Breakpoint
breakpointElement =
    let f (Elem _ _ contents) = do
            (file,contents1) <- findElement (ElemAcceptor "file" simple) [ e | e@(CElem _ _) <- contents ]
            (lineStr,contents2) <- findElement (ElemAcceptor "line" simple) contents1
            return $ mkBreakpoint file (read lineStr) 0
    in ElemAcceptor "breakpoint" f