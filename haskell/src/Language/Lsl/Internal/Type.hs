{-# OPTIONS_GHC -XFlexibleContexts -XDeriveDataTypeable #-}
module Language.Lsl.Internal.Type(
    LSLType(..),
    LSLValue(..),
    Component(..),
    typeOfLSLValue,
    typeOfLSLComponent,
    parseFloat,
    parseInt,
    parseVector,
    parseRotation,
    lslValueComponent,
    replaceLslValueComponent,
    lslValString,
    lslTypeString,
    lslShowVal,
    toSVal,
    isIVal,
    isFVal,
    isSVal,
    isKVal,
    isLVal,
    isVVal,
    isRVal,
    lslValueElement,
    defaultValue,
    vecMulScalar,
    rotMulVec,
    rotMul,
    invRot,
    vcross,
    toFloat,
    vVal2Vec,
    vec2VVal,
    rVal2Rot,
    rot2RVal,
    liftV1,
    liftV2) where
    
import Control.Monad.Error(MonadError)
import Data.Data(Data,Typeable)
import Data.List(intersperse)
import Language.Lsl.Internal.NumberParsing(readInt,readHexFloat)
import Language.Lsl.Internal.Key(nullKey)
import Language.Lsl.Internal.Util(lookupM,readM,cross,quaternionMultiply,quaternionToMatrix,fromInt)
import Language.Lsl.Internal.DOMProcessing(Element(..),ElemAcceptor(..),findValue,elementsOnly,simple,attrString,acceptList)

import Text.Printf(printf,PrintfArg(..))

data LSLType = LLList | LLInteger | LLVector | LLFloat | LLString | LLRot | LLKey | LLVoid
    deriving (Eq, Show, Typeable, Data)

-- A value.  Values correspond to the built in types (LSLType) that LSL
-- supports.  A value is an item that can be pushed onto the value stack.
data LSLValue a = IVal Int | FVal a | SVal String | VVal a a a 
               | RVal a a a a | LVal [LSLValue a] | KVal String
               | VoidVal deriving (Show,Eq,Ord)

data Component = X | Y | Z | S | All deriving (Eq,Show, Typeable, Data)

defaultValue LLList = LVal []
defaultValue LLInteger = IVal 0
defaultValue LLVector = VVal 0.0 0.0 0.0
defaultValue LLRot = RVal 0.0 0.0 0.0 1.0
defaultValue LLString = SVal ""
defaultValue LLKey = KVal nullKey
defaultValue LLFloat = FVal 0.0
defaultValue LLVoid = VoidVal

lslValueComponent X (VVal x y z) = FVal x
lslValueComponent X (RVal x y z s) = FVal x
lslValueComponent Y (VVal x y z) = FVal y
lslValueComponent Y (RVal x y z s) = FVal y 
lslValueComponent Z (VVal x y z) = FVal z
lslValueComponent Z (RVal x y z s) = FVal z
lslValueComponent S (RVal x y z s) = FVal s
lslValueComponent All val = val 
lslValueComponent c v = error ("illegal component " ++ (show c) ++ " of " ++ (show v))
replaceLslValueComponent X (VVal x y z) (FVal f) = VVal f y z
replaceLslValueComponent X (RVal x y z s) (FVal f) = RVal f y z s
replaceLslValueComponent Y (VVal x y z) (FVal f) = VVal x f z
replaceLslValueComponent Y (RVal x y z s) (FVal f) = RVal x f z s
replaceLslValueComponent Z (VVal x y z) (FVal f) = VVal x y f
replaceLslValueComponent Z (RVal x y z s) (FVal f) = RVal x y f s
replaceLslValueComponent S (RVal x y z s) (FVal f) = RVal x y z f
replaceLslValueComponent All v v' = v'
replaceLslValueComponent c v v' = error ("can't replace component " ++ (show c) ++ " of value " ++ (show v) ++ " with value " ++ (show v'))

typeOfLSLValue :: (RealFloat a) => LSLValue a -> LSLType
typeOfLSLValue v =
    case v of
        (IVal _) -> LLInteger
        (FVal _) -> LLFloat
        (SVal _) -> LLString
        (VVal _ _ _) -> LLVector
        (RVal _ _ _ _) -> LLRot
        (LVal _) -> LLList
        (KVal _) -> LLKey
        VoidVal -> LLVoid

typeOfLSLComponent v All = typeOfLSLValue v
typeOfLSLComponent (VVal _ _ _) _ = LLFloat
typeOfLSLComponent (RVal _ _ _ _) _ = LLFloat
typeOfLSLComponent v c = error ("value " ++ (show v) ++ " doesn't have a subcomponents")

-- convert a value to a string 'internally' (TODO: where SHOULD this be used? It's used in internal funcs, but
-- probably will not work completely correctly when tabs, newlines, or double quotes are involved)
lslValString (IVal i) = (show i)
lslValString (FVal f) = (printf "%.6f" ((realToFrac f) :: Double))
lslValString (SVal s) = s
lslValString (KVal k) = k
lslValString (VVal x y z) = concat ["<",comp2Str x,",",comp2Str y,",",comp2Str z,">"]
lslValString (RVal x y z s) = concat ["<",comp2Str x,",",comp2Str y,",",comp2Str z,",",comp2Str s,">"]
lslValString (LVal l) = concat ("[":(intersperse "," (map lslValString l) ++ ["]"]))
lslValString (VoidVal) = ""

-- convert a value to a string for display
lslShowVal (SVal s) = ('\"':escape s) ++ "\""
    where escape ('\n':cs) = '\\':'n':(escape cs)
          escape ('\\':cs) = '\\':'\\':(escape cs)
          escape ('\"':cs) = '\\':'\"':(escape cs)
          escape ('\t':cs) = '\\':'t':(escape cs) -- this one shouldn't happen (tabs should get converted to spaces)
          escape (c:cs) = c:(escape cs)
          escape [] = []
lslShowVal (KVal k) = lslShowVal (SVal k)
lslShowVal (LVal l) = concat ("[":(intersperse "," (map lslShowVal l))) ++ "]"
lslShowVal VoidVal = "n/a"
lslShowVal v = lslValString v

-- TODO: this info is duplicated elsewhere (e.g. parser) (fix)
lslTypeString LLInteger = "integer"
lslTypeString LLFloat = "float"
lslTypeString LLKey = "key"
lslTypeString LLList = "list"
lslTypeString LLVector = "vector"
lslTypeString LLRot = "rotation"
lslTypeString LLString = "string"
lslTypeString LLVoid = "void"

isIVal (IVal _) = True
isIVal _ = False
isSVal (SVal _) = True
isSVal _ = False
isLVal (LVal _) = True
isLVal _ = False
isFVal (FVal _) = True
isFVal _ = False
isVVal (VVal _ _ _) = True
isVVal _ = False
isRVal (RVal _ _ _ _) = True
isRVal _ = False
isKVal (KVal _) = True
isKVal _ = False

parseInt s = 
   case readInt s of
       [] -> 0
       (i,_):_ -> i
parseFloat s =
   case readFloat s of
       [] -> 0.0
       (f,_):_ -> f
       
parseVector s =
    case [(VVal x y z,t) | ("<",t0) <- lex s,
	                       (x,t1) <- readFloat t0,
	                       (",",t2) <- lex t1,
	 					   (y,t3) <- readFloat t2,
						   (",",t4) <- lex t3,
		                   (z,t5) <- readFloat t4,
		                   (">",t) <- lex t5] of
        [] -> VVal 0.0 0.0 0.0
        (v,_):_ -> v
parseRotation s =
    case [(RVal x y z w,t) | ("<",t0) <- lex s,
	                       (x,t1) <- readFloat t0,
	                       (",",t2) <- lex t1,
	 					   (y,t3) <- readFloat t2,
						   (",",t4) <- lex t3,
		                   (z,t5) <- readFloat t4,
						   (",",t6) <- lex t5,
		                   (w,t7) <- readFloat t6,
		                   (">",t) <- lex t7] of
        [] -> RVal 0.0 0.0 0.0 0.0
        (v,_):_ -> v

readFloat s =
    case readHexFloat s of
        [] -> reads s
        v -> v
        
toSVal :: RealFloat a => LSLValue a -> LSLValue a
toSVal (SVal s) = SVal s
toSVal (FVal f) = SVal (printf "%.6f" (realToFrac f :: Double))
toSVal (IVal i) = SVal (show i)
toSVal (KVal k) = SVal k
toSVal (VVal x y z) = SVal $ concat ["<",comp2Str x,",",comp2Str y,",",comp2Str z,">"]
toSVal (RVal x y z s) = SVal $ concat ["<",comp2Str x,",",comp2Str y,",",comp2Str z,",",comp2Str s,">"]
toSVal VoidVal = SVal "" -- perhaps should be error
toSVal (LVal l) = 
    SVal $ concatMap toS l
    where toS v = let (SVal s) = toSVal v in s

comp2Str :: RealFloat a => a -> String
comp2Str f = printf "%.5f" (realToFrac f :: Double)

lslValueElement :: (RealFloat a, Read a, MonadError String m) => ElemAcceptor m (LSLValue a)
lslValueElement =
    let f e@(Elem _ attrs contents) = do
            valType <- lookupM "class" attrs
            case attrString valType of
                "string" -> simple e >>= return . SVal
                "key" -> simple e >>= return . KVal
                "int" -> simple e >>= readM >>= return . IVal
                "float" -> simple e >>= readM >>= return . FVal
                "vector" -> acceptVector e >>= return
                "rotation" -> acceptRotation e >>= return
                "list" -> acceptLslList e >>= return
                _ -> fail "unrecognized value element"
    in ElemAcceptor "value" f
 
acceptVector (Elem _ _ contents) = do
            (x,c1) <- findValue "x" (elementsOnly contents)
            (y,c2) <- findValue "y" (elementsOnly c1)
            (z,[]) <- findValue "z" (elementsOnly c2)
            return $ VVal x y z
            
acceptRotation (Elem _ _ contents) = do
            (x,c1) <- findValue "x" (elementsOnly contents)
            (y,c2) <- findValue "y" (elementsOnly c1)
            (z,c3) <- findValue "z" (elementsOnly c2)
            (s,[]) <- findValue "s" (elementsOnly c3)
            return $ RVal x y z s
        
acceptLslList e = do
        l <- acceptList lslValueElement e
        return $ LVal l

-- vector and rotation operations
vecMulScalar (VVal x y z) f = (VVal (x*f) (y*f) (z*f))
rotMul (RVal x1 y1 z1 s1) (RVal x2 y2 z2 s2) = 
    let (x,y,z,s) = (x1,y1,z1,s1) `quaternionMultiply` (x2,y2,z2,s2)
    in RVal x y z s
rot2Mat (RVal x y z s) = quaternionToMatrix (x,y,z,s)
matMul ((a1,b1,c1),(a2,b2,c2),(a3,b3,c3)) (VVal a b c) =
    VVal (a1*a + a2 * b + a3 * c) (b1 * a + b2 * b + b3 * c) (c1 * a + c2 * b + c3 * c)
rotMulVec rot vec = matMul (rot2Mat rot) vec
invRot (RVal x y z s) = (RVal (-x) (-y) (-z) s)

vcross (VVal x1 y1 z1) (VVal x2 y2 z2) =
    let (x,y,z) = (x1,y1,z1) `cross` (x2,y2,z2) in VVal x y z
    
toFloat (FVal f) = f
toFloat (IVal i) = fromInt i

vVal2Vec (VVal x y z) = (x,y,z)
vec2VVal (x,y,z) = VVal x y z
liftV1 f = vec2VVal . f . vVal2Vec
liftV2 f = \ x y -> vec2VVal $ f (vVal2Vec x) (vVal2Vec y)

rot2RVal (x,y,z,s) = RVal x y z s
rVal2Rot (RVal x y z s) = (x,y,z,s)
