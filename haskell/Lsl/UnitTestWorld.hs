module Lsl.UnitTestWorld(
    runUnitTests,
    runUnitTests',
    renderTestResults) where

import Control.Exception
import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Data.List
import Data.Bits
import Debug.Trace
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
        randGen :: StdGen,
        wScripts :: [(String,Validity CompiledLSLScript)],
        wLibrary :: [(String,Validity LModule)],
        expectations :: FuncCallExpectations
    } deriving (Show)

type SimpleWorldM = ErrorT String (State SimpleWorld)
simpleWorldM = ErrorT . State
getTick :: SimpleWorldM Int
getTick = get >>= return . tick
getMaxTick :: SimpleWorldM Int
getMaxTick = get >>= return . maxTick
getMsgLog :: SimpleWorldM [(Int,String)]
getMsgLog = get >>= return . msgLog
getRandGen :: SimpleWorldM StdGen
getRandGen = get >>= return . randGen
getWScripts :: SimpleWorldM [(String, Validity CompiledLSLScript)]
getWScripts = get >>= return . wScripts
getWLibrary :: SimpleWorldM [(String, Validity LModule)]
getWLibrary = get >>= return . wLibrary
getExpectations :: SimpleWorldM FuncCallExpectations
getExpectations = get >>= return . expectations

setTick t = do w <- get; put (w { tick = t })
setMsgLog l = do w <- get; put (w { msgLog = l })
setExpectations e = do w <- get; put (w { expectations = e })
modifyMsgLog f = do w <- get; put (w { msgLog = f (msgLog w) })

logMsg s = do
    tick <- getTick
    modifyMsgLog ((tick,s):)

renderCall n a = concat ([n, "("] ++ (separateWith "," $ map lslValString a) ++ [")"])

doPredef n i a = 
    do  logMsg $ "call: "  ++ renderCall n a
        case lookup n internalLLFuncs of
            Just f -> f i a
            Nothing -> do
                fce <- getExpectations
                let allowed = Nice == expectationMode fce
                case expectedReturns n a fce of
                    Nothing -> handleUnexpected allowed
                    Just (m, v) -> do
                        when (v /= VoidVal) $ logMsg ("return: " ++ lslValString v)
                        setExpectations $ removeExpectation m fce
                        return (EvalIncomplete,v)
    where handleUnexpected allowed =
              if allowed then 
                  do (_,rt,_) <- ctx ("finding predef  " ++ n) $ 
                                  findM (\ (n',_,_) -> n' == n) funcSigs
                     return $ (EvalIncomplete, defaultValue rt)
              else fail ("unexpected call: " ++ renderCall n a)
              
simulate (ModuleFunc moduleName funcName) globs args =
    do lib <- getWLibrary
       case lookup moduleName lib of
           Nothing -> return (Left $ "No such module: " ++ moduleName)
           Just (Invalid s) -> return (Left $ "Invalid module: " ++ moduleName)
           Just (Valid lmodule) ->
               let script = mkScript lmodule in
                   sim' moduleName (validLSLScript lib script) [funcName] globs args
simulate (ScriptFunc scriptName funcName) globs args = simScript scriptName [funcName] globs args
simulate (ScriptHandler scriptName stateName handlerName) globs args = simScript scriptName [stateName,handlerName] globs args

simScript name path globs args =
    do scripts <- getWScripts
       case lookup name scripts of
           Nothing -> return (Left $ "No such script: " ++ name)
           Just vscript -> sim' name vscript path globs args

sim' name vscript path globs args =
    case vscript of
        Invalid s -> return (Left ("Invalid script: " ++ name))
        Valid script -> 
            do  maxTick <- getMaxTick
                result <- executeLslSimple (initLSLScript script) doPredef getTick setTick maxTick path globs args 
                return result
             
mkScript (LModule globdefs vars) =
    LSLScript (varsToGlobdefs ++ globdefs) [L.State (nullCtx "default") []]
    where varsToGlobdefs = map (\ v -> GV v Nothing) vars
    

nl = showChar '\n'
padInt i =
  let s = show i
      padding = 7 - length s in showString (replicate padding ' ') . showString s
renderIndent n = showString (replicate n ' ')
renderSequence r = (foldl (.) (showString "")) . (map r)
renderLogMsg n (i,s) = renderIndent n . padInt i . showString ": " . showString s . nl
renderMsgLog n l = renderSequence (renderLogMsg n) l
renderTestResult (ErrorResult n s l) =
    showString "Error " . shows s . nl . renderMsgLog 4 l
renderTestResult (Timeout n l) =
     showString "Timeout" . nl . renderMsgLog 4 l
renderTestResult (FailureResult n s l) =
     showString "Failure " . shows s . nl . renderMsgLog 4 l
renderTestResult (SuccessResult n l) =
    showString "Success" . nl . renderMsgLog 4 l
    
renderTestResults = renderSequence renderTestResult

runTests unitTests =
    do results <- runErrorT $
           do augLib <- loadLibrary
              let library = libFromAugLib augLib
              scripts <- loadScripts library
              runUnitTests library scripts unitTests
       case results of
           Left s -> fail s -- error s
           Right results' -> putStr $ renderTestResults results' ""

runUnitTests library scripts unitTests = do
    flip mapM unitTests $ \ unitTest -> do
        let name = unitTestName unitTest
        let result = (runState $ runErrorT $ simulate (entryPoint unitTest) (initialGlobs unitTest) (arguments unitTest))
                     (SimpleWorld { maxTick = 10000, tick = 0, msgLog = [], randGen = mkStdGen 0,
                                    wScripts = scripts, wLibrary = library, expectations = (expectedCalls unitTest)})
        return $ case result of
            (Left s, w) -> ErrorResult name s (msgLog w)
            (Right res1, w) ->
                case res1 of
                    Left s -> ErrorResult name s (msgLog w)
                    Right (EvalIncomplete,_,_) -> Timeout name (msgLog w)
                    Right (EvalComplete newstate, Just val, globs) ->
                        checkResults (newstate, val,globs,w) unitTest

runUnitTests' library scripts unitTests = do
    flip mapM unitTests $ \ unitTest -> do
        let name = unitTestName unitTest
        let result = (runState $ runErrorT $ simulate (entryPoint unitTest) (initialGlobs unitTest) (arguments unitTest))
                     (SimpleWorld { maxTick = 10000, tick = 0, msgLog = [], randGen = mkStdGen 0,
                                    wScripts = scripts, wLibrary = library, expectations = (expectedCalls unitTest)})
        let resultDescriptor = case result of
                (Left s, w) -> ErrorResult name s (msgLog w)
                (Right res1, w) ->
                    case res1 of
                        Left s -> ErrorResult name s (msgLog w)
                        Right (EvalIncomplete,_,_) -> Timeout name (msgLog w)
                        Right (EvalComplete newstate, Just val, globs) ->
                            checkResults (newstate, val, globs, w) unitTest
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
         

                           
tryElse defaultVal action = liftIO $ tryJust (const $ Just defaultVal) action >>= return . (either id id)

main =
    do args <- getArgs
       tests <- mapM parseUnitTestFile args
       runTests $ concat tests