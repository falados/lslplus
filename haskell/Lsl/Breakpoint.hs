module Lsl.Breakpoint(
    Breakpoint,
    BreakpointManager,
    mkBreakpoint,  -- :: String -> Int -> Int -> Breakpoint
    checkBreakpoint, -- :: Breakpoint -> BreakpointManager -> (Bool,BreakpointManager)
    addFixedBreakpoint, -- :: Breakpoint -> BreakpointManager -> BreakpointManager
    removeFixedBreakpoint, -- :: Breakpoint -> BreakpointManager -> BreakpointManager
    pushBreakpointFrame, -- :: BreakpointManager -> BreakpointManager
    popBreakpointFrame, -- :: BreakpointManager -> BreakpointManager
    setStepOutBreakpoint, -- :: BreakpointManager -> BreakpointManager
    setStepBreakpoint, -- :: BreakpointManager -> BreakpointManager
    setStepOverBreakpoint, -- :: BreakpointManager -> BreakpointManager
    emptyBreakpointManager, -- :: BreakpointManager
    breakpointFile, -- :: Breakpoint -> String
    breakpointLine -- :: Breakpoint -> Int
    ) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

data Breakpoint = Breakpoint { breakpointFile :: String, breakpointLine :: Int, breakpointColumn :: Int }
    deriving (Show,Eq,Ord)
data DynamicBreakpoint = NoDynamicBreakpoint | NextStatement | NextStatementInFrame | NextStatementAboveFrame
    deriving (Show,Eq)
mkBreakpoint name line col = Breakpoint name line col

data BreakpointManager = BreakpointManager { fixedBreakpoints :: (Map String (Set (Int,Int))), dynamicBreakpoints :: [DynamicBreakpoint] }

emptyBreakpointManager = BreakpointManager M.empty []

replaceBreakpoints bps bpm =
    foldl (flip addFixedBreakpoint) (bpm { fixedBreakpoints = M.empty }) bps
    
checkBreakpoint bp bpm = 
    case dynamicBreakpoints bpm of
        (NextStatement:frames) -> (True,bpm { dynamicBreakpoints = NoDynamicBreakpoint : frames })
        _ -> hasFixedBreakpoint bp bpm
        
hasFixedBreakpoint bp bpm =
    case M.lookup (breakpointFile bp) map of
        Nothing -> (False,bpm)
        Just set -> ((breakpointLine bp, breakpointColumn bp) `S.member` set, bpm)
    where map = fixedBreakpoints bpm
    
addFixedBreakpoint (Breakpoint file line col) bpm =
    let map = fixedBreakpoints bpm in
        case M.lookup file map of
            Nothing -> bpm { fixedBreakpoints = M.insert file (S.singleton (line,col)) map }
            Just set -> bpm { fixedBreakpoints = M.insert file (S.insert (line,col) set) map }
            
removeFixedBreakpoint (Breakpoint file line col) bpm =
    let map = fixedBreakpoints bpm in
        case M.lookup file map of
            Nothing -> bpm
            Just set -> bpm { fixedBreakpoints = M.insert file (S.delete (line,col) set) map }
            
pushBreakpointFrame bpm =
    bpm { dynamicBreakpoints = case dynamicBreakpoints bpm of
        (NextStatement:frames) -> (NextStatement:NoDynamicBreakpoint:frames)
        frames -> (NoDynamicBreakpoint:frames)}

popBreakpointFrame bpm =
    bpm { dynamicBreakpoints = case dynamicBreakpoints bpm of
        (NoDynamicBreakpoint:NextStatementInFrame:frames) -> NextStatement:frames
        (NoDynamicBreakpoint:x:frames) -> x:frames
        (_:_:frames) -> NextStatement:frames
        _ -> []}

setStepOutBreakpoint = setDynamicBreakpoint NextStatementAboveFrame
setStepBreakpoint = setDynamicBreakpoint NextStatement
setStepOverBreakpoint = setDynamicBreakpoint NextStatementInFrame
        
setDynamicBreakpoint dbp bpm =
    bpm { dynamicBreakpoints = case dynamicBreakpoints bpm of
        (_:frames) -> dbp:frames
        [] -> [] }
