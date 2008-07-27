-- The Lsl "compiler" parses LSL+ 'modules' and 'scripts', and can then:
--   issue a report on all the errors it has found
--   generate LSL scripts for those LSL+ scripts that successfully 'compiled'

module Lsl.Compiler(compile,main0) where

import Control.Monad
import IO
import Lsl.DOMSourceDescriptor
import Lsl.Load
import Lsl.Render
import Lsl.Structure
import Lsl.Type
import System.Directory
import System.FilePath
import System.Time
import Text.XML.HaXml hiding (when,xmlEscape)
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Pretty
import Lsl.Optimize(optimizeScript) 
import Lsl.Util
import Lsl.XmlCreate hiding (emit)
import qualified Lsl.XmlCreate as E

emit s = E.emit s []

readCompileEmit :: Handle -> IO ()
readCompileEmit h =
    do sourceInfo@(_,scriptInfo) <- readSourceList h
       results@(_,compiledScripts) <- compile sourceInfo
       renderScriptsToFiles compiledScripts scriptInfo
       putStr $ formatCompilationSummary results

main0 = readCompileEmit stdin
      
compile :: ([(String,String)],[(String,String)]) -> IO (AugmentedLibrary,[(String,Validity CompiledLSLScript)])
compile (moduleInfo,scriptInfo) =
    do augLib <- loadModules moduleInfo
       scripts <- loadScripts (libFromAugLib augLib) scriptInfo
       return (augLib,scripts)
       
formatCompilationSummary :: (AugmentedLibrary,[(String,Validity CompiledLSLScript)]) -> String
formatCompilationSummary (augLib, scripts) = 
   let emitModules = emit "modules" (map formatModuleCompilationSummary augLib)
       emitScripts = emit "scripts" (map formatScriptCompilationSummary scripts)
   in emit "summary" [emitModules , emitScripts] ""
   
formatScriptCompilationSummary (name,result) =
    emit "item" 
        ([emit "name" [showString name]] ++ 
        case result of
            Invalid err -> [formatErr err]
            Valid (globals,funcs,states) ->
                [emit "status" [emit "ok" [showString "true"]],
                emit "entryPoints" (map emitFunc funcs ++ concatMap stateEntryPointEmitters states),
                emit "globals" (map emitGlobal globals)])
    where funcNames = map (\ (Func dec _) -> ctxItem $ funcName dec)
          handlerPaths = concatMap (\ (State ctxName handlers) -> map (\ (Handler ctxName1 _ _) -> ctxItem ctxName ++ "." ++ ctxItem ctxName1) handlers)
 
formatModuleCompilationSummary (name,result) =
    emit "item"
        ([emit "name" [showString name]] ++
        case result of
            Invalid err -> [formatErr err]
            Valid (LModule globdefs freevars,(globals,_)) ->
                [emit "status" [emit "ok" [showString "true"]],
                emit "entryPoints" (map emitFunc (funcs globdefs)),
                emit "globals" (map emitGlobal globals ++ map emitFreeVar freevars)])
    where funcs globdefs = [ f | GF f <- globdefs]

emitFunc (Func fd _) =
    emit "entryPoint" [
        emit "name" [showString (ctxItem $ funcName fd)],
        emit "returnType" [showString (lslTypeString $ funcType fd)],
        emit "params" (emitParams $ map ctxItem (funcParms fd))]

emitGlobal (GDecl (Var n t) _) = emit "global" (emitNameTypePair n t)
      
emitFreeVar ctxvar =
   let (Var n t) = ctxItem ctxvar in emit "global" (emitNameTypePair n t)
       
emitNameTypePair n t = [emit "name" [showString n], emit "type" [showString $ lslTypeString t]]
   
stateEntryPointEmitters (State ctxName handlers) = map (emitHandler $ ctxItem ctxName) handlers

emitHandler state (Handler ctxName ctxVars _) =
    emit "entryPoint" [
        emit "name" [showString (state ++ "." ++ ctxItem ctxName)],
        emit "returnType" [showString "void"],
        emit "params" (emitParams $ map ctxItem ctxVars)]
        
emitParams = map emitParam

emitParam var = emit "param" [emit "name" [showString $ varName var], emit "type" [showString $ lslTypeString $ varType var]]

formatErr (ctx,msg) = 
    emit "status" [emit "ok" [showString "false"] , formatCtx ctx , emit "msg" [showString (xmlEscape msg)]]

formatCtx UnknownSourceContext = id
formatCtx (TextLocation { textLine0 = l0, textColumn0 = c0, textLine1 = l1, textColumn1 = c1, textName = n }) =
    emit "errLoc" (map (\ (x,y) -> emit x [showString y]) 
                   [("lineStart",show l0),
                   ("columnStart",show c0),
                   ("lineEnd",show l1),
                   ("columnEnd",show c1)])

readSourceList :: Handle -> IO ([(String,String)],[(String,String)])
readSourceList handle = do
    input <- hGetContents handle
    let doc = xmlParse "" input
    return $ processCompileList doc
    
processCompileList :: Document Posn -> ([(String,String)],[(String,String)])
processCompileList (Document _ _ root _) = 
    case sourceFiles root of
        Left s -> error s
        Right v -> v

renderScriptsToFiles :: [(String,Validity CompiledLSLScript)] -> [(String,String)] -> IO ()
renderScriptsToFiles compiledScripts pathTable = 
    let scriptsToRender = 
         [(path,script) | (Just path,Valid script) <- map (\ (name,vs) -> (lookup name pathTable,vs)) compiledScripts]
        scriptsToRemove =
         [path | (Just path,Invalid _) <- map (\ (name,vs) -> (lookup name pathTable,vs)) compiledScripts]
    in do
        clockTime <- getClockTime
        calTime <- toCalendarTime clockTime
        let stamp = calendarTimeToString calTime
        mapM_ (\ (path,script) -> renderScriptToFile stamp path script) scriptsToRender
        mapM_ (removeOutputScript) scriptsToRemove

renderScriptToFile stamp path script =
   let newPath = replaceExtension path ".lsl"
       text = renderCompiledScript stamp (optimizeScript script) "" in writeFile newPath text
       
removeOutputScript path = 
    do exists <- doesFileExist outpath
       when exists $ removeFile outpath
    where outpath = replaceExtension path ".lsl"
           