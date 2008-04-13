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
import Lsl.WorldDef
import Lsl.XmlCreate
import Text.XML.HaXml hiding (when)
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Pretty
import Debug.Trace

initializationFromXML xml = let doc = xmlParse "" xml in parseInitialization doc

parseInitialization (Document _ _ root _) = match initSimElement root

initSimElement :: Monad m => ElemAcceptor m (([(String,String)],[(String,String)]),FullWorldDef)
initSimElement =
    let f (Elem _ _ contents) =
            do (sources,contents1) <- findElement sourceFilesElement [ e | e@(CElem _ _) <- contents]
               (worldDef,[]) <- findElement worldElement contents1
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

--simCommand e = match (ElemAcceptor "sim-continue" simple) e >> return (SimContinue [] [])
simCommand e = execCmd "sim-continue" SimContinue e

execCmd s f e = do
    (bp,events) <- match (ElemAcceptor s commandContent) e
    return (f bp events)
  
commandContent (Elem _ _ contents) = do
    (breakpoints,contents1) <- findOptionalElement  breakpointsElement [ e | e@(CElem _ _) <- contents]
    (events,[]) <- findOptionalElement  (elementList "events" simEventElement) contents1
    return $ (maybe [] id breakpoints, maybe [] id events)

simEventElement :: Monad m => ElemAcceptor m SimEvent
simEventElement =
    let f (Elem _ _ contents) = 
             do (name,contents1) <- findElement (ElemAcceptor "name" simple) [ e | e@(CElem _ _) <- contents]
                (delay,contents2) <- findElement (ElemAcceptor "delay" simple) contents1
                (args,[]) <- findOptionalElement (elementList "args" simArgElement) contents2
                case reads delay of
                    [] -> fail "invalid delay: expected integer"
                    ((i,_):_) -> return $ SimEvent name (maybe [] id args) i
    in ElemAcceptor "event" f

simArgElement :: Monad m => ElemAcceptor m SimEventArg
simArgElement = 
    let f (Elem _ _ contents) =
            do (name,contents1) <- findElement (ElemAcceptor "name" simple) [ e | e@(CElem _ _) <- contents]
               (value,[]) <- findElement (ElemAcceptor "value" simple) contents1
               return $ SimEventArg name value
    in ElemAcceptor "arg" f
    
outputToXML (SimInfo _ log state) = (emit "sim-info" [] [emitLog log,emitState state]) ""
outputToXML (SimEnded _ log state) = (emit "sim-ended" [] [emitLog log,emitState state]) ""

emitLog log =
    emit "messages" [] $ map emitMessage (log)

emitMessage logMessage =
    emit "message" [] [ emitSimple "time" [] (show $ logMessageTime logMessage),
                        emitSimple "level" [] (logLevelToName $ logMessageLevel logMessage),
                        emitSimple "source" [] (logMessageSource logMessage),
                        emitSimple "text" [] (logMessageText logMessage)]

emitState state =
    emit "state" [] [ emitPrims (simStateInfoPrims state),
                      emitAvatars (simStateInfoAvatars state) ]
emitPrims prims = emitList "prims" emitPrim prims
emitAvatars avatars = emitList "avatars" emitAvatar avatars

emitPrim (key,name) = emit "prim" [] [emitSimple "key" [] key, emitSimple "name" [] name]
emitAvatar (key,name) = emit "avatar" [] [emitSimple "key" [] key, emitSimple "name" [] name]

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
