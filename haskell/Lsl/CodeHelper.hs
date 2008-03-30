-- a place to put shared, LSL code related utilities.  Some of these are already in Lsl.Type,
-- could be moved here.
module Lsl.CodeHelper(renderCall) where

import Lsl.Type
import Lsl.Util

renderCall n a = concat ([n, "("] ++ (separateWith "," $ map lslShowVal a) ++ [")"])
