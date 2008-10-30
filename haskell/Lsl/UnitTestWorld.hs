module Lsl.UnitTestWorld(
    simStep,
    simFunc,
    hasFunc,
    SimpleWorld,
    TestEvent(..),
    ExecutionInfo(..),
    ExecCommand(..)) where

import Control.Monad(liftM2)
import Control.Monad.State(MonadState(..),State(..),StateT(..),evalState)
import Control.Monad.Error(ErrorT(..))
import Data.List(find,intersperse)
import Data.Maybe(isJust)
import Lsl.Breakpoint(Breakpoint,BreakpointManager,checkBreakpoint,emptyBreakpointManager,
                      replaceBreakpoints,setStepBreakpoint,setStepOverBreakpoint,setStepOutBreakpoint,
                      breakpointFile,breakpointLine)
import Lsl.CodeHelper(renderCall)
import Lsl.FuncSigs(funcSigs)
import Lsl.InternalLLFuncs(internalLLFuncs)
import Lsl.Syntax hiding (State)
import qualified Lsl.Syntax as L
import Lsl.Type(LSLValue,lslValString,lslShowVal,defaultValue)
import Lsl.Evaluation(EvalResult(..))
import Lsl.Exec(ExecutionInfo(..),ScriptImage(..),evalSimple,runEval,scriptImage,setupSimple,initStateSimple,frameInfo)
import Lsl.TestResult(TestResult(..))
import Lsl.UnitTest(EntryPoint(..),LSLUnitTest(..),ExpectationMode(..),FuncCallExpectations(..),expectedReturns,removeExpectation)
import Lsl.Util(findM,ctx)

--trace1 v = trace ("->>" ++ (show v)) v

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
              
mkScript (LModule globdefs vars) =
    LSLScript (varsToGlobdefs ++ globdefs) [L.State (nullCtx "default") []]
    where varsToGlobdefs = map (\ v -> GV v Nothing) vars

getValidScript name =
    do  scripts <- getWScripts
        case lookup name scripts of
            Nothing -> return (Left $ "No such script: " ++ name)
            Just (Left s) -> return $ Left $ "Invalid script: " ++ name    
            Just (Right script) -> return $ Right script
        
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
            Just (Left s) -> return (Left $ "Invalid module: " ++ moduleName)
            Just (Right lmodule) -> 
                case validLSLScript lib (mkScript lmodule) of
                    Left _ -> return $ Left "Invalid entry point (internal error?)"
                    Right script -> return $ Right (script,[funcName])

checkResults (ms1, val, globs, w) unitTest =
    let name = unitTestName unitTest
        ms0 = expectedNewState unitTest 
        expectedR = expectedReturn unitTest in
        if ((expectationMode $ expectations w) `elem` [Strict,Exhaust]) &&
           (not (null (callList $ expectations w))) then
             FailureResult name ("some expected function calls not made: " ++
                 concat (intersperse ", " $ map (fst.fst) $ callList $ expectations w))
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
                      Just (globname,val) ->
                          case lookup globname globs of
                              Nothing ->
                                  FailureResult name ("expected global " ++ globname ++ " to have final value of " ++
                                                (lslValString val) ++ ", but no such global was found")
                                                (msgLog w)
                              Just val' ->
                                  FailureResult name ("expected global " ++ globname ++ " to have final value of " ++
                                                (lslValString val) ++ ", but actually had value of " ++
                                                (lslValString val')) (msgLog w)
                      Nothing -> SuccessResult name (msgLog w)

--------------------------------------------------
-- 'Interactive' testing

data TestEvent = TestComplete TestResult | TestSuspended  ExecutionInfo | AllComplete

data ExecCommand = ExecContinue [Breakpoint] | ExecStep [Breakpoint] | ExecStepOver [Breakpoint] | 
                   ExecStepOut [Breakpoint]

breakpointsFromCommand (ExecContinue bps) = bps
breakpointsFromCommand (ExecStep bps) = bps
breakpointsFromCommand (ExecStepOver bps) = bps
breakpointsFromCommand (ExecStepOut bps) = bps

hasFunc :: [(String,Validity LModule)] -> (String,String) -> Either String Bool
hasFunc lib (moduleName,functionName) =
        case converted of
           Left s -> Left ("no such module: " ++ moduleName)
           Right (Left s) -> Left ("no such module: " ++ moduleName)
           Right (Right ((_,funcs,_),path)) -> Right $ isJust (findFunc functionName funcs)
    where converted = evalState (runErrorT (convertEntryPoint ep)) world
          ep = ModuleFunc moduleName functionName
          world = SimpleWorld { maxTick = 10000, tick = 0, msgLog = [], wScripts = [], wLibrary = lib, 
                                expectations = FuncCallExpectations Nice [], breakpointManager = emptyBreakpointManager }
                                
simFunc :: [(String,Validity LModule)] -> (String,String) -> [(String,LSLValue)] -> [LSLValue] -> Either String (LSLValue,[(String,LSLValue)])
simFunc lib (moduleName,functionName) globs args =
   let world = SimpleWorld { maxTick = 10000, tick = 0, msgLog = [], wScripts = [], wLibrary = lib, 
                             expectations = FuncCallExpectations Nice [], breakpointManager = emptyBreakpointManager }
       ep = ModuleFunc moduleName functionName
       init = runState (runErrorT (
           do converted <- convertEntryPoint ep
              case converted of
                  Left s -> fail s
                  Right (script,path) ->
                      do result <- runEval (setupSimple path globs args) exec
                         case result of
                             (Left s, _) -> fail s
                             (Right (), exec') -> return exec'
                      where exec = initStateSimple script doPredef logMsg getTick setTick checkBp)) world
    in case init of
        (Left s, world') -> Left s
        (Right exec,world') ->
            case (runState $ runErrorT $ (runStateT $ runErrorT $ evalSimple 1000) exec) world of
                (Left s,_) -> Left s
                (Right r, _) ->
                    case r of
                        (Left s,_) -> Left s
                        (Right (EvalComplete newState, Just val), exec') -> Right (val,glob $ scriptImage exec')
                        (Right (EvalComplete newState, _),_) -> Left "execution error"
                        (Right (EvalIncomplete,_),_) -> Left "execution error: timeout"
                        (Right _,_) -> Left "execution error"
                            
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
            
