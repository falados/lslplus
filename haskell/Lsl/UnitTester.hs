{-# OPTIONS_GHC -XFlexibleContexts #-}
module Lsl.UnitTester(main2) where

import Control.Monad(MonadPlus(..))
import Control.Monad.Error(MonadError(..))
import Lsl.BreakpointsDeserialize(breakpointsElement)
import Lsl.Compiler(compile)
import Lsl.DOMProcessing(ElemAcceptor(..),findElement,findOptionalElement,match)
import Lsl.DOMSourceDescriptor(sourceFilesElement)
import Lsl.DOMUnitTestDescriptor(testsElement)
import Lsl.ExecInfo(emitExecutionInfo)
import Language.Lsl.Syntax(libFromAugLib)
import Lsl.TestResult(emitTestResult)
import Lsl.UnitTest(LSLUnitTest(..))
import Lsl.UnitTestWorld(ExecCommand(..),TestEvent(..),simStep)
import Lsl.Util(unescape,processLinesS)
import Lsl.XmlCreate(emit)
import Text.XML.HaXml(Element(..),Content(..),Document(..),xmlParse)
-- import Debug.Trace

-- trace1 x = trace (show x) x

testExecutionElement :: MonadError String m => ElemAcceptor m (([(String,String)],[(String,String)]),[LSLUnitTest])
testExecutionElement =
    let f (Elem _ _ contents) =
            do (sources,contents1) <- findElement sourceFilesElement [ e | e@(CElem _ _) <- contents]
               (tests,[]) <- findElement testsElement contents1
               return (sources,tests)
    in ElemAcceptor "test-descriptor" f
               
testExecutionDescriptionFromXML input = let doc = xmlParse "" input in parseTestExecutionDescription doc

parseTestExecutionDescription (Document _ _ root _) = match testExecutionElement root

main2 :: IO ()
main2 = 
    do  input <- getLine
        --hPutStrLn stderr input
        --(src,unitTests) <- 
        case testExecutionDescriptionFromXML  (unescape input) of
            Left s -> error s
            Right (src,unitTests) -> do
                (augLib,scripts) <- compile src
                let runStep state s =
                        let command = execCommandFromXML s
                            (e,state') = simStep scripts (libFromAugLib augLib) state command
                        in (state',testEventToXML e)
                processLinesS (unitTests,Nothing) "quit" runStep
    
execCommandFromXML xml = 
     let (Document _ _ root _) = xmlParse "" xml in 
         case execCommand root of
             Left s -> error s
             Right command -> command

testEventToXML e = emitTestEvent e ""

execCommand e = (execContinue e) `mplus` (execStep e) `mplus` (execStepOver e) `mplus` (execStepOut e)

execContinue e = match (ElemAcceptor "exec-continue" commandContent) e >>= return . ExecContinue
execStep e = match (ElemAcceptor "exec-step" commandContent) e >>= return . ExecStep
execStepOver e = execCmd "exec-step-over" ExecStepOver e
execStepOut e = execCmd "exec-step-out" ExecStepOut e

execCmd s f e = match (ElemAcceptor s commandContent) e >>= return . f
commandContent (Elem _ _ contents) = do
    (breakpoints,[]) <- findOptionalElement  breakpointsElement [ e | e@(CElem _ _) <- contents]
    return $ maybe [] id breakpoints
    
emitTestEvent (TestComplete testResult) = emit "test-complete" [] [emitTestResult testResult]
emitTestEvent (AllComplete) = emit "all-complete" [] []
emitTestEvent (TestSuspended info) = emit "test-suspended" [] [emitExecutionInfo info]
