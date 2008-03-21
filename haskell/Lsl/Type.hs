module Lsl.Type(
    LSLType(..),
    LSLValue(..),
    Component(..),
    typeOfLSLValue,
    typeOfLSLComponent,
    parseFloat,
    parseInt,
    lslValueComponent,
    replaceLslValueComponent,
    lslValString,
    lslTypeString,
    toSVal,
    defaultValue) where
    
import Lsl.NumberParsing
import Lsl.Key
import Lsl.Util

import Text.Printf

data LSLType = LLList | LLInteger | LLVector | LLFloat | LLString | LLRot | LLKey | LLVoid
    deriving (Eq, Show)

-- A value.  Values correspond to the built in types (LSLType) that LSL
-- supports.  A value is an item that can be pushed onto the value stack.
data LSLValue = IVal Int | FVal Float | SVal String | VVal Float Float Float 
              | RVal Float Float Float Float | LVal [LSLValue] | KVal String
              | VoidVal deriving (Show,Eq,Ord)

data Component = X | Y | Z | S | All deriving (Eq,Show)

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

lslValString (IVal i) = (show i)
lslValString (FVal f) = (show f)
lslValString (SVal s) = s
lslValString (KVal k) = k
lslValString (VVal x y z) = "<" ++ (show x) ++ "," ++ (show y) ++ "," ++ (show z) ++ ">"
lslValString (RVal x y z s) = "<" ++ (show x) ++ "," ++ (show y) ++ "," ++ (show z) ++ "," ++ (show s) ++ ">"
lslValString (LVal l) = concat (("[":(weave (map lslValString l) $ replicate (length l - 1) ",")) ++ ["]"])
lslValString (VoidVal) = ""

-- TODO: this info is duplicated elsewhere (e.g. parser) (fix)
lslTypeString LLInteger = "integer"
lslTypeString LLFloat = "float"
lslTypeString LLKey = "key"
lslTypeString LLList = "list"
lslTypeString LLVector = "vector"
lslTypeString LLRot = "rotation"
lslTypeString LLString = "string"
lslTypeString LLVoid = "void"

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
        
toSVal :: LSLValue -> LSLValue
toSVal (SVal s) = SVal s
toSVal (FVal f) = SVal (printf "%.6f" f)
toSVal (IVal i) = SVal (show i)
toSVal (KVal k) = SVal k
toSVal (VVal x y z) = SVal $ concat ["<",comp2Str x,",",comp2Str y,",",comp2Str z,">"]
toSVal (RVal x y z s) = SVal $ concat ["<",comp2Str x,",",comp2Str y,",",comp2Str z,",",comp2Str s,">"]
toSVal VoidVal = SVal "" -- perhaps should be error
toSVal (LVal l) = 
    SVal $ concatMap toS l
    where toS v = let (SVal s) = toSVal v in s

comp2Str :: Float -> String
comp2Str f = printf "%.5f" f