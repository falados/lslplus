module Lsl.DOMUnitTestDescriptor(testsElement) where

import Control.Monad
import Data.List
import Lsl.DOMProcessing
import Lsl.ExpressionHandler
import Lsl.Type
import Lsl.UnitTest
import Lsl.Util
import Text.XML.HaXml hiding (when,find)
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Pretty

import Debug.Trace
trace1 v = trace (show v) v

testsElement :: Monad m => ElemAcceptor m [LSLUnitTest]
testsElement = elementList "tests" testElement

testElement :: Monad m => ElemAcceptor m LSLUnitTest
testElement =
    let f (Elem _ _ contents) =
            do  (nm,contents1) <- findElement nameElement (elementsOnly contents)
                (ep,contents2) <- findElement entryPointElement contents1
                (args,contents3) <- findElement argumentsElement contents2
                (callExpectations, contents4) <- findElement expectationsElement contents3
                (er,contents5) <- findElement expectedReturnElement contents4
                (gb,contents6) <- findElement initialBindingsElement contents5
                (fb,[]) <- findElement finalBindingsElement contents6
                return $ LSLUnitTest {
                        unitTestName = nm,
                        entryPoint = ep,
                        initialGlobs = gb,
                        arguments = args,
                        expectedCalls = callExpectations,
                        expectedReturn = er,
                        expectedGlobalVals = fb,
                        expectedNewState = Nothing
                    }
    in ElemAcceptor "lsl-test" f

nameElement :: Monad m => ElemAcceptor m String
nameElement = ElemAcceptor "name" simple

entryPointElement :: Monad m => ElemAcceptor m EntryPoint
entryPointElement = 
    let f (Elem _ _ contents) =
          do (fn,contents1) <- findElement fileNameElement (elementsOnly contents)
             (p,[]) <- findElement pathElement contents1
             if "lslm" `isSuffixOf` fn then return (ModuleFunc fn p)
                 else case parts p of
                   [a,b] -> return $ ScriptHandler fn a b
                   [a] -> return $ ScriptFunc fn a
                   _ -> fail "invalid function/handler path"
    in ElemAcceptor "entryPoint" f  

fileNameElement :: Monad m => ElemAcceptor m String
fileNameElement = ElemAcceptor "fileName" simple
pathElement :: Monad m => ElemAcceptor m String
pathElement = ElemAcceptor "path" simple

argumentsElement :: Monad m => ElemAcceptor m [LSLValue]
argumentsElement = 
    let f (Elem _ _ contents) = 
          mapM (matchChoice [lslString, lslInteger, lslFloat, lslVector, lslRotation, lslKey, lslList]) (elementsOnly contents)
    in ElemAcceptor "arguments" f

lslString :: Monad m => ElemAcceptor m LSLValue    
lslString = ElemAcceptor "lsl-string" (\ e -> do
             s <- simple e
             case evaluateExpression LLString s of
                 Just k -> return k
                 Nothing -> fail "invalid content for lsl-string")
lslKey :: Monad m => ElemAcceptor m LSLValue    
lslKey = ElemAcceptor "lsl-key" (\ e -> do
             s <- simple e
             case evaluateExpression LLKey s of
                 Just k -> return k
                 Nothing -> fail "invalid content for lsl-key")
lslInteger :: Monad m => ElemAcceptor m LSLValue    
lslInteger = ElemAcceptor "lsl-integer" (\ e -> do
               s <- simple e
               case evaluateExpression LLInteger s of
                   Just f -> return f
                   Nothing -> fail "invalid content for lsl-integer")
lslFloat :: Monad m => ElemAcceptor m LSLValue
lslFloat = ElemAcceptor "lsl-float" (\ e -> do
               s <- simple e
               case evaluateExpression LLFloat s of
                   Just f -> return f
                   Nothing -> fail "invalid content for lsl-float")

lslVector :: Monad m => ElemAcceptor m LSLValue    
lslVector = ElemAcceptor "lsl-vector" (\ e -> do
               s <- simple e
               case evaluateExpression LLVector s of
                   Just f -> return f
                   Nothing -> fail "invalid content for lsl-vector")
lslRotation :: Monad m => ElemAcceptor m LSLValue    
lslRotation = ElemAcceptor "lsl-rotation" (\ e -> do
               s <- simple e
               case evaluateExpression LLRot s of
                   Just f -> return f
                   Nothing -> fail "invalid content for lsl-rotation")

lslList :: Monad m => ElemAcceptor m LSLValue    
lslList =
    let f (Elem _ _ contents) =
          do list <- mapM (matchChoice [lslString,lslInteger,lslKey,lslVector,lslRotation,lslFloat]) (elementsOnly contents)
             return (LVal list)
    in ElemAcceptor "lsl-list" f

lslVoid :: Monad m => ElemAcceptor m LSLValue
lslVoid = 
    let f (Elem _ _ []) = return VoidVal
        f (Elem name _ _) = fail ("unexpected content in " ++ name ++ " tag.")
    in ElemAcceptor "lsl-void" f
    
float (Elem name _ [CString _ s _]) =
       case reads s of
           [(v::Float,[])] -> return v
           _ -> fail ("invalid float in " ++ name ++ " tag")
float (Elem name _ _) = fail ("invalid content in " ++ name ++ " tag.")


xcomponent :: Monad m => ElemAcceptor m Float
xcomponent = ElemAcceptor "x" float
ycomponent :: Monad m => ElemAcceptor m Float
ycomponent = ElemAcceptor "y" float
zcomponent :: Monad m => ElemAcceptor m Float
zcomponent = ElemAcceptor "z" float
scomponent :: Monad m => ElemAcceptor m Float
scomponent = ElemAcceptor "s" float

expectedReturnElement :: Monad m => ElemAcceptor m (Maybe LSLValue)
expectedReturnElement = maybeSomeVal "expectedReturn"

expectationsElement :: Monad m => ElemAcceptor m FuncCallExpectations
expectationsElement =
    let f (Elem _ _ contents) = do
          (modeString,contents1) <- findElement modeElement (elementsOnly contents)
          mode <- case modeString of
                      "nice" -> return Nice
                      "exhaust" -> return Exhaust
                      "strict" -> return Strict
                      "normal" -> return Normal
                      s -> fail ("illegal mode " ++ s)
          (calls,[]) <- findElement callsElement contents1
          return $ FuncCallExpectations {
                  expectationMode = mode,
                  callList = filter (not . null . fst . fst) calls
              }
    in ElemAcceptor "expectations" f

modeElement :: Monad m => ElemAcceptor m String
modeElement = ElemAcceptor "mode" simple

callsElement :: Monad m => ElemAcceptor m [((String, [Maybe LSLValue]),LSLValue)]
callsElement = elementList "calls" callElement
    
callElement :: Monad m => ElemAcceptor m ((String, [Maybe LSLValue]),LSLValue)
callElement =
    let f (Elem _ _ contents) = do
          (name,contents1) <- findElement (ElemAcceptor "name" simple) (elementsOnly contents)
          (args,contents2) <- findElement callArgsElement contents1
          (retval, []) <- findElement returnsElement contents2
          return ((name,args),retval)
    in ElemAcceptor "call" f

callArgsElement :: Monad m => ElemAcceptor m [Maybe LSLValue]
callArgsElement = elementList "args" (maybeSomeVal "maybe-value")

returnsElement :: Monad m => ElemAcceptor m LSLValue
returnsElement = someVal "returns"

initialBindingsElement :: Monad m => ElemAcceptor m [(String,LSLValue)]
initialBindingsElement = elementList "initialBindings" globalBindingElement

finalBindingsElement :: Monad m => ElemAcceptor m [(String,LSLValue)]
finalBindingsElement = elementList "finalBindings" globalBindingElement

globalBindingElement :: Monad m => ElemAcceptor m (String,LSLValue)
globalBindingElement =
    let f (Elem _ _ contents) = do
        (name,contents1) <- findElement (ElemAcceptor "name" simple) (elementsOnly contents)
        (value,[]) <- findElement (someVal "value") contents1
        return (name,value)
    in ElemAcceptor "globalBinding" f
    
maybeSomeVal tag =
    let f e@(Elem _ _ contents) = do
            (val,[]) <- findOptionalElement (someVal "val") (elementsOnly contents)
            return val
    in ElemAcceptor tag f
       
someVal tag =
   let f e@(Elem _ [("class",v)] contents) =
           let valString = attValueString v in
            case find (\ acceptor -> valString == acceptorTag acceptor) [lslString,lslKey,lslInteger,lslList,lslFloat,
                                                                         lslVector,lslRotation,lslVoid] of
                Nothing -> fail ("unknown lsl value class: " ++ valString)
                Just (ElemAcceptor _ f1) -> f1 e
       f e@(Elem name _ _) = fail ("'class' attribute required in " ++ name ++ " element")
    in ElemAcceptor tag f
               
parts :: String -> [String]
parts "" =  []
parts s  =  let (l, s') = break (== '.') s
			   in  l : case s' of
					[]     	-> []
					(_:s'') -> parts s''
