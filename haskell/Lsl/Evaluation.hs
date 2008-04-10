module Lsl.Evaluation(
    ScriptInfo(..),
    EvalResult(..),
    Event(..)) where
    
import Lsl.Type
import Lsl.Util
import Lsl.Breakpoint

--type ScriptInfo = (String,Int,String,String) -- (object id, prim index, script name, prim key)
data ScriptInfo = ScriptInfo { scriptInfoObjectKey :: String, 
                               scriptInfoPrimIndex :: Int,
                               scriptInfoScriptName :: String,
                               scriptInfoPrimKey :: String,
                               scriptInfoCurrentEvent :: Maybe Event }
data EvalResult = EvalIncomplete | EvalComplete (Maybe String) | YieldTil Int
                | BrokeAt Breakpoint
    deriving (Show)

data Event = Event { eventName :: String, eventValues :: [LSLValue], eventInfo :: [(String,LSLValue)] }
    deriving (Show)

