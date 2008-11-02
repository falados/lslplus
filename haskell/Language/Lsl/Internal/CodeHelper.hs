-- a place to put shared, LSL code related utilities.  Some of these are already in Lsl.Type,
-- could be moved here.
module Language.Lsl.Internal.CodeHelper(renderCall) where

import Data.List(intersperse)
import Lsl.Type(lslShowVal)

renderCall n a = concat ([n, "("] ++ (intersperse "," $ map lslShowVal a) ++ [")"])
