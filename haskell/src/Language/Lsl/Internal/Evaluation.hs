module Language.Lsl.Internal.Evaluation(
    ScriptInfo(..),
    EvalResult(..),
    Event(..)) where
    
import Data.Map(Map)
import Language.Lsl.Internal.Type(LSLValue)
import Language.Lsl.Internal.Breakpoint(Breakpoint)

--type ScriptInfo = (String,Int,String,String) -- (object id, prim index, script name, prim key)
data ScriptInfo = ScriptInfo { scriptInfoObjectKey :: String, 
                               scriptInfoPrimIndex :: Int,
                               scriptInfoScriptName :: String,
                               scriptInfoPrimKey :: String,
                               scriptInfoCurrentEvent :: Maybe Event }
    deriving (Show)
    
data EvalResult = EvalIncomplete | EvalComplete (Maybe String) | YieldTil Int
                | BrokeAt Breakpoint
    deriving (Show)

data Event = Event { eventName :: String, eventValues :: [LSLValue], eventInfo :: Map String LSLValue }
    deriving (Show)
