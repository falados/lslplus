module Lsl.Evaluation(
    ScriptInfo,
    EvalResult(..),
    Event(..)) where
    
import Lsl.Type
import Lsl.Util

type ScriptInfo = (String,Int,String,String) -- (object id, prim index, script name, prim key)

data EvalResult = EvalIncomplete | EvalComplete (Maybe String) | YieldTil Int
    deriving (Show)

data Event = Event { eventName :: String, eventValues :: [LSLValue] }
    deriving (Show)

