{-# OPTIONS_GHC -XFlexibleContexts #-}
module Lsl.UnitTester where

import Control.Exception
import Control.Monad.Error
import IO
import Lsl.BreakpointsDeserialize
import Lsl.Compiler
import Lsl.DOMProcessing
import Lsl.DOMSourceDescriptor
import Lsl.DOMUnitTestDescriptor
import Lsl.Exec
import Lsl.ExecInfo
import Lsl.Structure
import Lsl.TestResult
import Lsl.Type
import Lsl.UnitTest
import Lsl.UnitTestWorld
import Lsl.Util
import Lsl.XmlCreate
import Text.XML.HaXml hiding (when)
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Pretty
import Debug.Trace

trace1 x = trace (show x) x

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
