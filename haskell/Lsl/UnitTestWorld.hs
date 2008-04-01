module Lsl.UnitTestWorld(
    runUnitTests,
    simStep,
    SimpleWorld,
    TestEvent(..),
    ExecutionInfo(..),
    ExecCommand(..)) where

import Control.Exception
import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Data.List
import Data.Bits
import Debug.Trace
import Lsl.Breakpoint
import Lsl.CodeHelper
import Lsl.FuncSigs
import Lsl.InternalLLFuncs
import Lsl.Load
import Lsl.Parse
import Lsl.Render
import Lsl.Structure hiding (State)
import qualified Lsl.Structure as L
import Lsl.Type
import Lsl.Key
import Lsl.Evaluation
import Lsl.Exec
import Lsl.TestResult
import Lsl.UnitTest
import Lsl.UnitTestParser
import Lsl.Util
import System
import System.Random

trace1 v = trace ("->>" ++ (show v)) v

data SimpleWorld = SimpleWorld {
        maxTick :: Int,
        tick :: Int,
        msgLog :: [(Int,String)],
        wScripts :: [(String,Validity CompiledLSLScript)],
        wLibrary :: [(String,Validity LModule)],
        expectations :: FuncCallExpectations,
        breakpointManager :: BreakpointManager
    }

type SimpleWorldM = ErrorT String (State SimpleWorld)
simpleWorldM = ErrorT . State
getTick :: SimpleWorldM Int
getTick = get >>= return . tick
getMaxTick :: SimpleWorldM Int
getMaxTick = get >>= return . maxTick
getMsgLog :: SimpleWorldM [(Int,String)]
getMsgLog = get >>= return . msgLog
getWScripts :: SimpleWorldM [(String, Validity CompiledLSLScript)]
getWScripts = get >>= return . wScripts
getWLibrary :: SimpleWorldM [(String, Validity LModule)]
getWLibrary = get >>= return . wLibrary
getExpectations :: SimpleWorldM FuncCallExpectations
getExpectations = get >>= return . expectations
getBreakpointManager :: SimpleWorldM BreakpointManager
getBreakpointManager = get >>= return . breakpointManager

setTick t = do w <- get; put (w { tick = t })
setMsgLog l = do w <- get; put (w { msgLog = l })
setExpectations e = do w <- get; put (w { expectations = e })
setBreakpointManager bpm = do w <- get; put (w { breakpointManager = bpm })
modifyMsgLog f = do w <- get; put (w { msgLog = f (msgLog w) })

checkBp bp sm =
    do  bpm <- getBreakpointManager
        let (result,bpm',sm') = checkBreakpoint bp bpm sm
        setBreakpointManager bpm'
        return (result,sm')
        
logMsg s = do
    tick <- getTick
    modifyMsgLog ((tick,s):)

doPredef n i a = 
    do  logMsg $ "call: "  ++ renderCall n a
        case lookup n internalLLFuncs of
            Just f -> do
                result@(m, v) <- f i a
                logMsg ("return: " ++ lslShowVal v)
                return result
            Nothing -> do
                fce <- getExpectations
                let allowed = Nice == expectationMode fce
                case expectedReturns n a fce of
                    Nothing -> handleUnexpected allowed
                    Just (m, v) -> do
                        logMsg ("return: " ++ lslShowVal v)
                        setExpectations $ removeExpectation m fce
                        return (EvalIncomplete,v)
    where handleUnexpected allowed =
              if allowed then 
                  do (_,rt,_) <- ctx ("finding predef  " ++ n) $ 
                                  findM (\ (n',_,_) -> n' == n) funcSigs
                     return $ (EvalIncomplete, defaultValue rt)
              else fail ("unexpected call: " ++ renderCall n a)
              
-- simulate (ModuleFunc moduleName funcName) globs args =
--     do lib <- getWLibrary
--        case lookup moduleName lib of
--            Nothing -> return (Left $ "No such module: " ++ moduleName)
--            Just (Invalid s) -> return (Left $ "Invalid module: " ++ moduleName)
--            Just (Valid lmodule) ->
--                let script = mkScript lmodule in
--                    sim moduleName (validLSLScript lib script) [funcName] globs args
-- simulate (ScriptFunc scriptName funcName) globs args = simScript scriptName [funcName] globs args
-- simulate (ScriptHandler scriptName stateName handlerName) globs args = simScript scriptName [stateName,handlerName] globs args
-- simScript name path globs args =
--     do scripts <- getWScripts
--        case lookup name scripts of
--            Nothing -> return (Left $ "No such script: " ++ name)
--            Just vscript -> sim name vscript path globs args

-- sim name vscript path globs args =
--     case vscript of
--         Invalid s -> return (Left ("Invalid script: " ++ name))
--         Valid script -> 
--             do  maxTick <- getMaxTick
--                 result <- executeLslSimple script doPredef logMsg getTick setTick maxTick path globs args 
--                 return result

simulate entryPoint globs args =
    do scriptAndPath <- convertEntryPoint entryPoint
       case scriptAndPath of
           Left s -> return (Left s)
           Right (script,path) -> 
               do maxTick <- getMaxTick
                  executeLslSimple script doPredef logMsg getTick setTick checkBp maxTick path globs args
        
mkScript (LModule globdefs vars) =
    LSLScript (varsToGlobdefs ++ globdefs) [L.State (nullCtx "default") []]
    where varsToGlobdefs = map (\ v -> GV v Nothing) vars

getValidScript name =
    do  scripts <- getWScripts
        case lookup name scripts of
            Nothing -> return (Left $ "No such script: " ++ name)
            Just (Invalid s) -> return $ Left $ "Invalid script: " ++ name    
            Just (Valid script) -> return $ Right script
        
convertEntryPoint (ScriptFunc scriptName funcName) =
    do  script <- getValidScript scriptName
        return $ liftM2 (,) script (Right [funcName])
convertEntryPoint (ScriptHandler scriptName stateName handlerName) =
    do  script <- getValidScript scriptName
        return $ liftM2 (,) script (Right [stateName,handlerName])
convertEntryPoint (ModuleFunc moduleName funcName) =
    do  lib <- getWLibrary
        case lookup moduleName lib of
            Nothing -> return (Left $ "No such module: " ++ moduleName)
            Just (Invalid s) -> return (Left $ "Invalid module: " ++ moduleName)
            Just (Valid lmodule) -> 
                case validLSLScript lib (mkScript lmodule) of
                    Invalid _ -> return $ Left "Invalid entry point (internal error?)"
                    Valid script -> return $ Right (script,[funcName])

runUnitTests library scripts unitTests = do
    flip mapM unitTests $ \ unitTest -> do
        let name = unitTestName unitTest
        let result = (runState $ runErrorT $ simulate (entryPoint unitTest) (initialGlobs unitTest) (arguments unitTest))
                     (SimpleWorld { maxTick = 10000, tick = 0, msgLog = [], 
                                    wScripts = scripts, wLibrary = library, expectations = (expectedCalls unitTest),
                                    breakpointManager = emptyBreakpointManager})
        let resultDescriptor = case result of
                (Left s, w) -> ErrorResult name s (msgLog w)
                (Right res1, w) ->
                    case res1 of
                        Left s -> ErrorResult name s (msgLog w)
                        Right (EvalIncomplete,_,execState) -> Timeout name (msgLog w)
                        Right (EvalComplete newstate, Just val, execState) ->
                            checkResults (newstate, val, glob $ scriptImage execState, w) unitTest
        putStr $ resultToXML resultDescriptor
        putStr "\n"
    return ()

checkResults (ms1, val, globs, w) unitTest =
    let name = unitTestName unitTest
        ms0 = expectedNewState unitTest 
        expectedR = expectedReturn unitTest in
        if ((expectationMode $ expectations w) `elem` [Strict,Exhaust]) &&
           (not (null (callList $ expectations w))) then
             FailureResult name ("some expected function calls not made: " ++
                 concat (separateWith ", " $ map (fst.fst) $ callList $ expectations w))
                 (msgLog w)
        else case (ms0, ms1) of
          (Nothing, Just st) -> 
              FailureResult name ("expected no state change, but changed to " ++ st) (msgLog w)
          (Just st, Nothing) ->
              FailureResult name ("expected state change to " ++ st ++ ", but no change occurred") (msgLog w)
          (ms0, ms1) | ms0 /= ms1 -> let (Just s0, Just s1) = (ms0,ms1) in 
                                         FailureResult name ("expected state change to " ++ s0 ++
                                                             ", but acutally changed to " ++ s1) (msgLog w)
                             | otherwise ->
              if expectedR /= Nothing && expectedR /=  Just val then
                  let (Just val') = expectedR in
                      FailureResult name ("expected return value was " ++ (lslValString val') ++
                                          ", but actually was " ++ (lslValString val)) (msgLog w)
              else 
                  case find (`notElem` globs) (expectedGlobalVals unitTest) of
                      Just (name,val) ->
                          case lookup name globs of
                              Nothing ->
                                  FailureResult name ("expected global " ++ name ++ " to have final value of " ++
                                                (lslValString val) ++ ", but no such global was found")
                                                (msgLog w)
                              Just val' ->
                                  FailureResult name ("expected global " ++ name ++ " to have final value of " ++
                                                (lslValString val) ++ ", but actually had value of " ++
                                                (lslValString val')) (msgLog w)
                      Nothing -> SuccessResult name (msgLog w)

--------------------------------------------------
-- 'Interactive' testing

data ExecutionInfo = ExecutionInfo String Int [(String,SourceContext,Maybe Int,MemRegion)]

data TestEvent = TestComplete TestResult | TestSuspended  ExecutionInfo | AllComplete

data ExecCommand = ExecContinue [Breakpoint] | ExecStep [Breakpoint] | ExecStepOver [Breakpoint] | 
                   ExecStepOut [Breakpoint]

breakpointsFromCommand (ExecContinue bps) = bps
breakpointsFromCommand (ExecStep bps) = bps
breakpointsFromCommand (ExecStepOver bps) = bps
breakpointsFromCommand (ExecStepOut bps) = bps

simSome exec world = runState (runErrorT (
    do maxTick <- getMaxTick
       (runStateT $ runErrorT $ evalSimple maxTick) exec)) world
  
-- no more tests, not currently executing     
simStep _ _ ([], Nothing) _ = (AllComplete,([],Nothing))     
--  not currently executing, more tests
simStep scripts lib (unitTest:tests, Nothing) command =
    let world = (SimpleWorld { maxTick = 10000, tick = 0, msgLog = [], 
                               wScripts = scripts, wLibrary = lib, expectations = (expectedCalls unitTest),
                               breakpointManager = emptyBreakpointManager})
        ep = entryPoint unitTest
        globs = initialGlobs unitTest
        args = arguments unitTest
        name = unitTestName unitTest
        init = runState (runErrorT (
            do converted <- convertEntryPoint ep
               case converted of
                   Left s -> fail s
                   Right (script,path) ->
                       do  result <- runEval (setupSimple path globs args) exec
                           case result of
                               (Left s, _) -> fail s
                               (Right (),exec') -> return exec'
                       where exec = initStateSimple script doPredef logMsg getTick setTick checkBp)) world
    in case init of
        (Left s,world') -> (TestComplete $ ErrorResult name s [],(tests, Nothing))
        (Right exec,world') -> simStep scripts lib (unitTest:tests, Just (world',exec)) command
-- currently executing
simStep _ _ (unitTest:tests, Just (world,exec)) command =
    let name = unitTestName unitTest 
        breakpoints = breakpointsFromCommand command
        world' = world { breakpointManager = replaceBreakpoints breakpoints (breakpointManager world) }
        updateStepManager f ex = let img = scriptImage ex
                                     stepMgr = stepManager img in ex { scriptImage = img { stepManager = f stepMgr } }
        execNew = case command of
            ExecStep _ -> updateStepManager setStepBreakpoint exec
            ExecStepOver _ -> updateStepManager setStepOverBreakpoint exec
            ExecStepOut _ -> updateStepManager setStepOutBreakpoint exec
            _ -> exec
    in
    case simSome execNew world' of
        (Left s,world'') -> (TestComplete $ ErrorResult name s (msgLog world''),(tests,Nothing))
        (Right res,world'') -> 
            case res of
                (Left s,_) -> (TestComplete $ ErrorResult name s (msgLog world''),(tests,Nothing))
                (Right (EvalComplete newState,Just val), exec') ->  (TestComplete checkedResult, (tests,Nothing))
                    where checkedResult = checkResults (newState, val, glob $ scriptImage exec', world'') unitTest
                (Right (EvalIncomplete,_),_) -> (TestComplete $ Timeout name (msgLog world''),(tests,Nothing))
                (Right (BrokeAt bp,_),exec') -> 
                    (TestSuspended (ExecutionInfo file line frames),(unitTest:tests,Just (world'',exec')))
                    where file = breakpointFile bp
                          line = breakpointLine bp
                          frames = frameInfo (scriptImage exec')
            