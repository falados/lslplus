{-# OPTIONS_GHC -XFlexibleContexts #-}
module Language.Lsl.Internal.DOMUnitTestDescriptor(testsElement) where

import Control.Monad.Error(MonadError(..))
import Data.List(find,isSuffixOf)
import Language.Lsl.Internal.DOMProcessing(ElemAcceptor(..),Element(..),
                         attValueString,elementList,elementsOnly,findElement,findOptionalElement,matchChoice,simple)
import Lsl.ExpressionHandler(evaluateExpression)
import Language.Lsl.Internal.Type(LSLType(..),LSLValue(..))
import Lsl.UnitTest(LSLUnitTest(..),EntryPoint(..),FuncCallExpectations(..),ExpectationMode(..))

--trace1 v = trace (show v) v

testsElement :: MonadError String m => ElemAcceptor m [LSLUnitTest]
testsElement = elementList "tests" testElement

testElement :: MonadError String m => ElemAcceptor m LSLUnitTest
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

nameElement :: MonadError String m => ElemAcceptor m String
nameElement = ElemAcceptor "name" simple

entryPointElement :: MonadError String m => ElemAcceptor m EntryPoint
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

fileNameElement :: MonadError String m => ElemAcceptor m String
fileNameElement = ElemAcceptor "fileName" simple
pathElement :: MonadError String m => ElemAcceptor m String
pathElement = ElemAcceptor "path" simple

argumentsElement :: MonadError String m => ElemAcceptor m [LSLValue]
argumentsElement = 
    let f (Elem _ _ contents) = 
          mapM (matchChoice [lslString, lslInteger, lslFloat, lslVector, lslRotation, lslKey, lslList1]) (elementsOnly contents)
    in ElemAcceptor "arguments" f

lslString :: MonadError String m => ElemAcceptor m LSLValue    
lslString = ElemAcceptor "lsl-string" (\ e -> do
             s <- simple e
             case evaluateExpression LLString s of
                 Just k -> return k
                 Nothing -> fail "invalid content for lsl-string")
lslKey :: MonadError String m => ElemAcceptor m LSLValue    
lslKey = ElemAcceptor "lsl-key" (\ e -> do
             s <- simple e
             case evaluateExpression LLKey s of
                 Just k -> return k
                 Nothing -> fail "invalid content for lsl-key")
lslInteger :: MonadError String m => ElemAcceptor m LSLValue    
lslInteger = ElemAcceptor "lsl-integer" (\ e -> do
               s <- simple e
               case evaluateExpression LLInteger s of
                   Just f -> return f
                   Nothing -> fail "invalid content for lsl-integer")
lslFloat :: MonadError String m => ElemAcceptor m LSLValue
lslFloat = ElemAcceptor "lsl-float" (\ e -> do
               s <- simple e
               case evaluateExpression LLFloat s of
                   Just f -> return f
                   Nothing -> fail "invalid content for lsl-float")

lslVector :: MonadError String m => ElemAcceptor m LSLValue    
lslVector = ElemAcceptor "lsl-vector" (\ e -> do
               s <- simple e
               case evaluateExpression LLVector s of
                   Just f -> return f
                   Nothing -> fail "invalid content for lsl-vector")
lslRotation :: MonadError String m => ElemAcceptor m LSLValue    
lslRotation = ElemAcceptor "lsl-rotation" (\ e -> do
               s <- simple e
               case evaluateExpression LLRot s of
                   Just f -> return f
                   Nothing -> fail "invalid content for lsl-rotation")

lslList :: MonadError String m => ElemAcceptor m LSLValue    
lslList =
    let f (Elem _ _ contents) =
          do list <- mapM (matchChoice [lslString,lslInteger,lslKey,lslVector,lslRotation,lslFloat]) (elementsOnly contents)
             return (LVal list)
    in ElemAcceptor "lsl-list" f
    
lslList1 :: MonadError String m => ElemAcceptor m LSLValue    
lslList1 =ElemAcceptor "lsl-list1" (\ e -> do
               s <- simple e
               case evaluateExpression LLList s of
                   Just f -> return f
                   Nothing -> fail "invalid content for lsl-list1")


lslVoid :: MonadError String m => ElemAcceptor m LSLValue
lslVoid = 
    let f (Elem _ _ []) = return VoidVal
        f (Elem name _ _) = fail ("unexpected content in " ++ name ++ " tag.")
    in ElemAcceptor "lsl-void" f
        

expectedReturnElement :: MonadError String m => ElemAcceptor m (Maybe LSLValue)
expectedReturnElement = maybeSomeVal "expectedReturn"

expectationsElement :: MonadError String m => ElemAcceptor m FuncCallExpectations
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

modeElement :: MonadError String m => ElemAcceptor m String
modeElement = ElemAcceptor "mode" simple

callsElement :: MonadError String m => ElemAcceptor m [((String, [Maybe LSLValue]),LSLValue)]
callsElement = elementList "calls" callElement
    
callElement :: MonadError String m => ElemAcceptor m ((String, [Maybe LSLValue]),LSLValue)
callElement =
    let f (Elem _ _ contents) = do
          (name,contents1) <- findElement (ElemAcceptor "name" simple) (elementsOnly contents)
          (args,contents2) <- findElement callArgsElement contents1
          (retval, []) <- findElement returnsElement contents2
          return ((name,args),retval)
    in ElemAcceptor "call" f

callArgsElement :: MonadError String m => ElemAcceptor m [Maybe LSLValue]
callArgsElement = elementList "args" (maybeSomeVal "maybe-value")

returnsElement :: MonadError String m => ElemAcceptor m LSLValue
returnsElement = someVal "returns"

initialBindingsElement :: MonadError String m => ElemAcceptor m [(String,LSLValue)]
initialBindingsElement = elementList "initialBindings" globalBindingElement

finalBindingsElement :: MonadError String m => ElemAcceptor m [(String,LSLValue)]
finalBindingsElement = elementList "finalBindings" globalBindingElement

globalBindingElement :: MonadError String m => ElemAcceptor m (String,LSLValue)
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
            case find (\ acceptor -> valString == acceptorTag acceptor) [lslString,lslKey,lslInteger,lslList1,lslFloat,
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
