module Lsl.UnitTester where

import Control.Exception
import Control.Monad
import IO
import Lsl.BreakpointsDeserialize
import Lsl.Compiler
import Lsl.DOMProcessing
import Lsl.DOMSourceDescriptor
import Lsl.DOMUnitTestDescriptor
import Lsl.Exec
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

testExecutionElement :: Monad m => ElemAcceptor m (([(String,String)],[(String,String)]),[LSLUnitTest])
testExecutionElement =
    let f (Elem _ _ contents) =
            do (sources,contents1) <- findElement sourceFilesElement [ e | e@(CElem _ _) <- contents]
               (tests,[]) <- findElement testsElement contents1
               return (sources,tests)
    in ElemAcceptor "test-descriptor" f
               
readTestExecutionDescription :: Handle -> IO (([(String,String)],[(String,String)]),[LSLUnitTest])
readTestExecutionDescription handle = do
    input <- hGetContents handle
    let doc = xmlParse "" input
    parseTestExecutionDescription doc

testExecutionDescriptionFromXML input = let doc = xmlParse "" input in parseTestExecutionDescription doc

parseTestExecutionDescription (Document _ _ root _) = match testExecutionElement root

main1 :: IO ()
main1 = do
    (src,unitTests) <- readTestExecutionDescription stdin
    (augLib,scripts) <- compile src
    runUnitTests (libFromAugLib augLib) scripts unitTests

main2 :: IO ()
main2 = 
    do  input <- getLine
        --hPutStrLn stderr input
        (src,unitTests) <- testExecutionDescriptionFromXML  (unescape input)
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

emitExecutionInfo (ExecutionInfo name line frames) =
    emit "script-state" [] [emitSimple "sourceElement" [] name,
                            emitSimple "currentLine" [] (show line),
                            emitFrames frames]

emitFrames frames =
    emit "frames" [] (map emitFrame frames)

emitFrame (name,ctx,line,bindings) =
    emit "frame" [] [emitSimple "name" []  name,
                     emitCtx ctx,
                     emitLine line, 
                     emit "bindings" [] (map emitBinding bindings)]
    where emitCtx UnknownSourceContext = id
          emitCtx ctx = emitSimple "file" [] ( textName ctx )
          emitLine Nothing = id
          emitLine (Just i) = emitSimple "line" [] (show i)
          
emitBinding (name,val) =
    emit "binding" [] [emitSimple "name" [] name,
                       emitVal val]
              
emitVal' t s = emitSimple "val" [("class",t)] s
emitVal (IVal i) = emitVal' "integer-value" (show i)
emitVal (FVal f) = emitVal' "float-value" (show f)
emitVal (SVal s) = emitVal' "string-value" s
emitVal (KVal s) = emitVal' "key-value" s
emitVal (VVal x y z) = 
    emit "val" [("class","vector-value")] 
        [emitSimple "x" [] (show x),
         emitSimple "y" [] (show y),
         emitSimple "z" [] (show z)]
emitVal (RVal x y z s) = 
    emit "val" [("class","rotation-value")] 
        [emitSimple "x" [] (show x),
         emitSimple "y" [] (show y),
         emitSimple "z" [] (show z),
         emitSimple "s" [] (show s)]
emitVal (LVal l) =
    emit "val" [("class","list-value")] [emit "elements" [] (map emitVal l)]