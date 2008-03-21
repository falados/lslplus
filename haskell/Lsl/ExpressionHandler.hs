module Lsl.ExpressionHandler(validateExpression,evaluateExpression) where

import Control.Monad
import Data.Bits
import IO

import Lsl.Constants
import Lsl.DOMProcessing
import Lsl.Parse
import Lsl.Structure
import Lsl.Type
import Lsl.Util
import Lsl.XmlCreate hiding (emit)
import qualified Lsl.XmlCreate as E

import Text.XML.HaXml hiding (when,xmlEscape)
import Text.XML.HaXml.Posn

emit s = E.emit s []

validPrimitiveCtxExpr (Ctx _ expr) = validPrimitiveExpr expr

validPrimitiveExpr (Get (Ctx _ name,All)) = findConstType name
validPrimitiveExpr (Neg expr) = 
    do t <- validPrimitiveCtxExpr expr
       when (t `notElem` [LLFloat,LLInteger]) $ fail "operator only valid for integer and float expressions"
       return t
validPrimitiveExpr (Inv expr) =
    do t <- validPrimitiveCtxExpr expr
       when (t /= LLInteger) $ fail "operator only valid for integer expressions"
       return LLInteger
validPrimitiveExpr (Not expr) =
    do t <- validPrimitiveCtxExpr expr
       when (t /= LLInteger) $ fail "operator only valid for integer expressions"
       return LLInteger
validPrimitiveExpr (IntLit i) = return LLInteger
validPrimitiveExpr (FloatLit f) = return LLFloat
validPrimitiveExpr (StringLit s) = return LLString
validPrimitiveExpr (KeyLit k) = return LLKey
validPrimitiveExpr (Add e0 e1) =
    do (t0,t1) <- validPrimEach e0 e1
       case (t0,t1) of
           (LLInteger,LLFloat) -> return LLFloat
           (LLFloat,LLInteger) -> return LLFloat
           (LLFloat,LLFloat) -> return LLFloat
           (LLInteger,LLInteger) -> return LLInteger
           (LLString,LLString) -> return LLString
           _ -> fail "incompatible operands"
validPrimitiveExpr (Sub e0 e1) = validArithExpr e0 e1
validPrimitiveExpr (Mul e0 e1) = validArithExpr e0 e1
validPrimitiveExpr (Div e0 e1) = validArithExpr e0 e1
validPrimitiveExpr (Mod e0 e1) = validIntegerExpr e0 e1
validPrimitiveExpr (BAnd e0 e1) = validIntegerExpr e0 e1
validPrimitiveExpr (BOr e0 e1) = validIntegerExpr e0 e1
validPrimitiveExpr (Xor e0 e1) = validIntegerExpr e0 e1
validPrimitiveExpr (ShiftL e0 e1) = validIntegerExpr e0 e1
validPrimitiveExpr (ShiftR e0 e1) = validIntegerExpr e0 e1
validPrimitiveExpr (And e0 e1) = validIntegerExpr e0 e1
validPrimitiveExpr (Or e0 e1) = validIntegerExpr e0 e1
validPrimitiveExpr (Equal e0 e1) = validEqExpr e0 e1
validPrimitiveExpr (NotEqual e0 e1) = validEqExpr e0 e1
validPrimitiveExpr (Lt e0 e1) = validRelExpr e0 e1
validPrimitiveExpr (Le e0 e1) = validRelExpr e0 e1
validPrimitiveExpr (Gt e0 e1) = validRelExpr e0 e1
validPrimitiveExpr (Ge e0 e1) = validRelExpr e0 e1
validPrimitiveExpr expr = fail "expression not valid in this context"

validPrimEach e0 e1 =
    do t0 <- validPrimitiveCtxExpr e0
       t1 <- validPrimitiveCtxExpr e1
       return (t0,t1)
       
validIntegerExpr e0 e1 =
    do (t0,t1) <- validPrimEach e0 e1
       case (t0,t1) of
           (LLInteger,LLInteger) -> return LLInteger
           _ -> fail "incompatible operands"
           
validArithExpr e0 e1 =
    do (t0,t1) <- validPrimEach e0 e1
       case (t0,t1) of
           (LLInteger,LLFloat) -> return LLFloat
           (LLFloat,LLInteger) -> return LLFloat
           (LLFloat,LLFloat) -> return LLFloat
           (LLInteger,LLInteger) -> return LLInteger
           _ -> fail "incompatible operands"
           
validRelExpr e0 e1 =
    do (t0,t1) <- validPrimEach e0 e1
       case (t0,t1) of
           (LLInteger,LLFloat) -> return LLInteger
           (LLFloat,LLInteger) -> return LLInteger
           (LLFloat,LLFloat) -> return LLInteger
           (LLInteger,LLInteger) -> return LLInteger
           _ -> fail "incompatible operands"

validEqExpr e0 e1 =           
    do (t0,t1) <- validPrimEach e0 e1
       case (t0,t1) of
           (LLInteger,LLFloat) -> return LLInteger
           (LLInteger,LLInteger) -> return LLInteger
           (LLFloat,LLFloat) -> return LLInteger
           (LLFloat,LLInteger) -> return LLInteger
           (LLString,LLString) -> return LLInteger
           (LLKey,LLKey) -> return LLInteger
           _ -> fail "incompatible operands"

checkExpr t text =
   case exprParser text of
       Left _ -> fail "syntax error"
       Right expr ->
           case validPrimitiveCtxExpr expr of
               Right t' -> 
                   case (t,t') of
                       (LLString,LLKey) -> return expr
                       (LLKey,LLString) -> return expr
                       (LLFloat,LLInteger) -> return expr
                       (t,t') | t == t' -> return expr
                              | otherwise -> fail (lslTypeString t ++ " expected")
               Left s -> fail s
               
evaluateExpression t text =
    case checkExpr t text of
        Left s -> fail s
        Right expr -> 
            do v <- evalCtxExpr expr
               case (t,v) of
                   (LLKey,SVal s) -> return $ KVal s
                   (LLString,KVal k) -> return $ SVal k
                   (LLFloat,IVal i) -> return $ FVal (fromInt i)
                   _ -> return v
            
unexpectedValue :: Monad m => m a
unexpectedValue = fail "unexpected value"

evalCtxExpr (Ctx _ expr) = evalExpr expr

evalExpr (Get (Ctx _ name,All)) = findConstVal name
evalExpr (Neg expr) = 
    do v <- evalCtxExpr expr
       case v of
           (IVal i) -> return (IVal (-i))
           (FVal f) -> return (FVal (-f))
           _ -> unexpectedValue
evalExpr (Inv expr) =
    do t <- evalCtxExpr expr
       case t of
           (IVal i) -> return (IVal $ complement i)
           _ -> unexpectedValue
evalExpr (Not expr) =
    do t <- evalCtxExpr expr
       case t of
           (IVal i) -> return $ IVal (if i == 0 then 1 else 0)
evalExpr (IntLit i) = return (IVal i)
evalExpr (FloatLit f) = return (FVal f)
evalExpr (StringLit s) = return (SVal s)
evalExpr (KeyLit k) = return (KVal k)
evalExpr (Add e0 e1) =
    do (v0,v1) <- evalEach e0 e1
       case (v0,v1) of
           (IVal i,FVal f) -> return $ FVal (f + fromInt i)
           (FVal f,IVal i) -> return $ FVal (f + fromInt i)
           (FVal f0,FVal f1) -> return $ FVal (f0 + f1)
           (IVal i0,IVal i1) -> return $ IVal (i0 + i1)
           (SVal s0,SVal s1) -> return $ SVal (s0 ++ s1)
           _ -> fail "incompatible operands"
evalExpr (Sub e0 e1) = evalArithExpr (-) (-) e0 e1
evalExpr (Mul e0 e1) = evalArithExpr (*) (*) e0 e1
evalExpr (Div e0 e1) = evalArithExpr (div) (/) e0 e1
evalExpr (Mod e0 e1) = evalIntExpr (mod) e0 e1
evalExpr (BAnd e0 e1) = evalIntExpr (.&.) e0 e1
evalExpr (BOr e0 e1) = evalIntExpr (.|.) e0 e1
evalExpr (Xor e0 e1) = evalIntExpr xor e0 e1
evalExpr (ShiftL e0 e1) = evalIntExpr shiftL e0 e1
evalExpr (ShiftR e0 e1) = evalIntExpr shiftR e0 e1
evalExpr (And e0 e1) = evalIntExpr (\ x y -> if x /= 0 && y /= 0 then 1 else 0) e0 e1
evalExpr (Or e0 e1) = evalIntExpr (\ x y -> if x /= 0 || y /= 0 then 1 else 0) e0 e1
evalExpr (Equal e0 e1) = evalEqExpr (==) e0 e1
evalExpr (NotEqual e0 e1) = evalEqExpr (/=) e0 e1
evalExpr (Lt e0 e1) = evalRelExpr (<) (<) e0 e1
evalExpr (Le e0 e1) = evalRelExpr (<=) (<=) e0 e1
evalExpr (Gt e0 e1) = evalRelExpr (>) (>) e0 e1
evalExpr (Ge e0 e1) = evalRelExpr (>=) (>=) e0 e1
evalExpr expr = fail "expression not valid in this context"

evalArithExpr fi ff e0 e1 =
    do (v0,v1) <- evalEach e0 e1
       case (v0,v1) of
           (IVal i0, IVal i1) -> return $ IVal (fi i0 i1)
           (FVal f0, IVal i1) -> return $ FVal (ff f0 $ fromInt i1)
           (FVal f0, FVal f1) -> return $ FVal (ff f0 f1)
           (IVal i0, FVal f1) -> return $ FVal (ff (fromInt i0) f1)

evalEqExpr f e0 e1 =
    do (v0,v1) <- evalEach e0 e1
       let b = case (v0,v1) of
               (FVal f0, IVal i1) -> f (FVal f0) (FVal $ fromInt i1)
               (IVal i0, FVal f1) -> f (FVal $ fromInt i0) (FVal f1)
               (SVal s, KVal k) -> f (SVal s) (SVal k)
               (KVal k, SVal s) -> f (SVal s) (SVal k)
               (x,y) -> f x y
       return $ IVal $ if b then 1 else 0
           
evalRelExpr fi ff e0 e1 =
    do (v0,v1) <- evalEach e0 e1
       let b = case (v0,v1) of
               (FVal f, IVal i) -> ff f (fromInt i)
               (IVal i, FVal f) -> ff (fromInt i) f
               (FVal f0, FVal f1) -> ff f0 f1
               (IVal i0, IVal i1) -> fi i0 i1
       return $ IVal $ if b then 1 else 0

evalIntExpr f e0 e1 =
    do (IVal i0, IVal i1) <- evalEach e0 e1
       return $ IVal $ f i0 i1 
       
evalEach e0 e1 =
    do v0 <- evalCtxExpr e0
       v1 <- evalCtxExpr e1
       return (v0,v1)
       
extractExpressionFromXML s =
    let doc = xmlParse "" s in processDOMExpr doc
       
processDOMExpr (Document _ _ root _) = 
    case match expressionElementAcceptor root of
        Left s -> error s
        Right v -> v

expressionElementAcceptor :: Monad m => ElemAcceptor m (String,String)
expressionElementAcceptor =
    let f (Elem _ _ contents) = do
            (t,contents1) <- findElement (ElemAcceptor "type" simple) (elementsOnly contents)
            (text,[]) <- findElement (ElemAcceptor "text" simple) contents1
            return (t,text)
    in ElemAcceptor "expression" f

expressionValidatorEmitter (Left s) =
    (emit "result" [ emit "ok" [showString "false"], emit "msg" [showString $ xmlEscape s]]) ""
expressionValidatorEmitter (Right _) =
    (emit "result" [ emit "ok" [showString "true"]]) ""

validateExpression hIn hOut =
    do input <- hGetContents hIn
       let (tstring,text) = extractExpressionFromXML input
       let t = case parseType tstring of
                   Right v -> v
                   Left s -> error (show s)
       putStr $ expressionValidatorEmitter (checkExpr t text)
           
       