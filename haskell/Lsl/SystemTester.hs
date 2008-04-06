module Lsl.SystemTester where

import Control.Exception
import Control.Monad
import IO
import Lsl.BreakpointsDeserialize
import Lsl.Compiler
import Lsl.DOMProcessing
import Lsl.DOMSourceDescriptor
import Lsl.Exec
import Lsl.Log
import Lsl.Structure
import Lsl.TestResult
import Lsl.Type
import Lsl.Util
import Lsl.World1
import Lsl.XmlCreate
import Text.XML.HaXml hiding (when)
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Pretty
import Debug.Trace

initializationFromXML xml = let doc = xmlParse "" xml in parseInitialization doc

parseInitialization (Document _ _ root _) = match initSimElement root

initSimElement :: Monad m => ElemAcceptor m (([(String,String)],[(String,String)]),WorldDef)
initSimElement =
    let f (Elem _ _ contents) =
            do (sources,contents1) <- findElement sourceFilesElement [ e | e@(CElem _ _) <- contents]
               (worldDef,[]) <- findElement worldDefElement contents1
               return (sources,worldDef)
    in ElemAcceptor "sim-descriptor" f

worldDefElement :: Monad m => ElemAcceptor m WorldDef
worldDefElement = 
    let f (Elem _ _ contents) =
            do (script,[]) <- findElement (ElemAcceptor "script" simple) [ e | e@(CElem _ _) <- contents]
               return $ WorldDef script
    in ElemAcceptor "worldDef" f
    
commandFromXML xml = 
     let (Document _ _ root _) = xmlParse "" xml in 
         case simCommand root of
             Left s -> error s
             Right command -> command

simCommand e = match (ElemAcceptor "sim-continue" simple) e >> return (SimContinue [] [])

outputToXML (SimInfo _ log) = (emit "sim-info" [] [emitLog log]) ""

emitLog log =
    emit "messages" [] $ map emitMessage (log)

emitMessage logMessage =
    emit "message" [] [ emitSimple "time" [] (show $ logMessageTime logMessage),
                        emitSimple "level" [] (logLevelToName $ logMessageLevel logMessage),
                        emitSimple "source" [] (logMessageSource logMessage),
                        emitSimple "text" [] (logMessageText logMessage)]

testSystem :: IO ()
testSystem = 
    do  input <- getLine
        --hPutStrLn stderr input
        (src,worldDef) <- initializationFromXML  (unescape input)
        (augLib,scripts) <- compile src
        let runStep state s =
                let command = commandFromXML s
                    (e,state') = simStep state command
                in (state',outputToXML e)
        processLinesS (Left (worldDef,scripts,libFromAugLib augLib)) "quit" runStep
