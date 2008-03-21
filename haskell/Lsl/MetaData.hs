module Lsl.MetaData where

-- constructs meta-data about the LSL+ language, and emits it as XML.

import IO
import Lsl.Constants
import Lsl.FuncSigs
import Lsl.InternalLLFuncs
import Lsl.Structure
import Lsl.Type
import qualified Lsl.XmlCreate as X

emit t = X.emit t []
emitSimple t s = emit t [showString s]

-- this is the main function...
printMeta = putStr buildMeta

buildMeta :: String
buildMeta =
    emit "lslmeta" [handlers,functions,constants] ""

emitHandler (name,params) =
    let dummyNames = zipWith (++) (repeat "arg") (map show [1..])
        params' = zip dummyNames params
    in emit "handler" [
           emitSimple "name" name,
           emitParams params',
           emitSimple "description" ("no description for " ++ name)]

emitParams params = emit "params" (map emitParam params)
emitParam (name,t) = emit "param" [emitSimple "name"  name, emitSimple "type" (lslTypeString t)]

handlers =
    emit "handlers" (map emitHandler goodHandlers)
    
functions =
    emit "functions" (map emitFunction funcMeta)
    
emitFunction (name,rtype,params,description) =
    emit "function" [
        emitSimple "name" name,
        emitSimple "returns" (if (rtype == LLVoid) then "" else lslTypeString rtype),
        emitParams params,
        emitSimple "description" description,
        emitSimple "stateless" (if (name `elem` internalLLFuncNames) then "true" else "false")]
        
constants =
    emit "constants" (map emitConstant allConstants)
    
emitConstant (Constant name value) =
    emit "constant" [
        emitSimple "name" name,
        emitSimple "type" (lslTypeString $ typeOfLSLValue value),
        emitSimple "value" $ case value of
            SVal s -> (show s)
            KVal k -> (show k)
            VVal x y z -> ("&lt;" ++ show x ++ "," ++ show y ++ "," ++ show z ++ "&gt;")
            RVal x y z s -> ("&lt;" ++ show x ++ "," ++ show y ++ "," ++ show z ++ "," ++ show s ++ "&gt;")
            _ -> lslValString value,
        emitSimple "description" "no description"]