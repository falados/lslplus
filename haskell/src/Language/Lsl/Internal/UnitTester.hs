{-# LANGUAGE FlexibleContexts,NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fwarn-unused-binds -fwarn-unused-imports #-}
module Language.Lsl.Internal.UnitTester(main2) where

import Control.Applicative
import Control.Monad.Error(MonadError(..))
import Language.Lsl.Internal.BreakpointsDeserialize(bps)
import Language.Lsl.Internal.Compiler(compile)
import Language.Lsl.Internal.DOMProcessing(choice,req,tag,tagit,getTag,def,runSimpleContext)
import Language.Lsl.Internal.DOMSourceDescriptor(sources)
import Language.Lsl.Internal.DOMUnitTestDescriptor(tests)
import Language.Lsl.Internal.ExecInfo(emitExecutionInfo)
import Language.Lsl.Syntax(libFromAugLib)
import Language.Lsl.Internal.TestResult(emitTestResult)
import Language.Lsl.UnitTestEnv(ExecCommand(..),TestEvent(..),simStep)
import Language.Lsl.Internal.Util(unescape,processLinesS)
import Language.Lsl.Internal.XmlCreate(emit)
import Text.XML.HaXml(Document(..),xmlParse)

execElement = tag "test-descriptor" >> (,) <$> req "source_files" sources <*> req "tests" tests

testExecutionDescriptionFromXML' input = runSimpleContext execElement root
    where (Document _ _ root _) = xmlParse "input" input
    
main2 :: IO ()
main2 = 
    do  input <- getLine
        case testExecutionDescriptionFromXML'  (unescape input) of
            Left s -> error s
            Right (src,unitTests) -> do
                (augLib,scripts) <- compile src
                let runStep state s =
                        let command = execCommandFromXML' s
                            (e,state') = simStep scripts (libFromAugLib augLib) state command
                        in (state',testEventToXML e)
                processLinesS (unitTests,Nothing) "quit" runStep

testEventToXML e = emitTestEvent e ""

execCommandFromXML' xml = 
     let (Document _ _ root _) = xmlParse "" xml in 
         case runSimpleContext command root of
             Left s -> error s
             Right command -> command

command = choice cmds >>= maybe
    (getTag >>= \ t -> throwError ("unrecognized command: " ++ t))
    return
    
cmds = [cmd "exec-continue" ExecContinue, cmd "exec-step" ExecStep,
    cmd "exec-step-over" ExecStepOver, cmd "exec-step-out" ExecStepOut]
cmd s f = tagit s $ f <$> def "breakpoints" [] bps
    
emitTestEvent (TestComplete testResult) = emit "test-complete" [] [emitTestResult testResult]
emitTestEvent (AllComplete) = emit "all-complete" [] []
emitTestEvent (TestSuspended info) = emit "test-suspended" [] [emitExecutionInfo info]
