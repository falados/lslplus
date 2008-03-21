-- Classes to help make expressing LSL in Haskell simpler
module Lsl.Builder(
        VDecl(..),
        Init(..),
        Assignment(..),
        Call(..),
        AccessValue(..),
        Incable(..),
        slit,
        ilit,
        flit,
        klit,
        castString,
        castFloat,
        castKey,
        castInt,
        castVector,
        castRot,
        castList,
        (!*),
        (!/),
        (!%),
        (!-) ,
        (!+) ,
        (!<<) ,
        (!>>) ,
        (!==) ,
        (!!=) ,
        (!<) ,
        (!<=) ,
        (!>) ,
        (!>=) ,
        (!&) ,
        (!^) ,
        (!|) ,
        (!&&) ,
        (!||) ,
        neg,
        inv,
        lslNot,
        handler,
        mimport,
        state
    ) where

import Lsl.Structure

type CtxExpr = Ctx Expr
type CtxStmt = Ctx Statement

class VDecl a where
   llInteger :: String -> a
   llFloat :: String -> a
   llString :: String -> a
   llKey :: String -> a
   llList :: String -> a
   llVector :: String -> a
   llRot :: String -> a
   
gdecl t n = GV (nullCtx $ Var n t) Nothing

instance VDecl GlobDef where
    llInteger = gdecl LLInteger
    llFloat = gdecl LLFloat
    llString = gdecl LLString
    llKey = gdecl LLKey
    llList = gdecl LLList
    llVector = gdecl LLVector
    llRot = gdecl LLRot
    
decl t n = nullCtx $ Decl (Var n t) Nothing

instance VDecl CtxStmt where
    llInteger = decl LLInteger
    llFloat = decl LLFloat
    llString = decl LLString
    llKey = decl LLKey
    llList = decl LLList
    llVector = decl LLVector
    llRot = decl LLRot
    
class Assignment a b where
    (<=>) :: a -> CtxExpr -> b
    (<+=>) :: a -> CtxExpr -> b
    (<-=>) :: a -> CtxExpr -> b
    (<*=>) :: a -> CtxExpr -> b
    (</=>) :: a -> CtxExpr -> b
    (<%=>) :: a -> CtxExpr -> b

instance Assignment String CtxExpr where
    x <=> e = nullCtx $ Set (nullCtx x,All) e
    x <+=> e = nullCtx $ IncBy (nullCtx x,All) e
    x <-=> e = nullCtx $ DecBy (nullCtx x,All) e
    x <*=> e = nullCtx $ MulBy (nullCtx x,All) e
    x </=> e = nullCtx $ DivBy (nullCtx x,All) e
    x <%=> e = nullCtx $ ModBy (nullCtx x,All) e
    
instance Assignment String CtxStmt where
    x <=> e = nullCtx $ Do $ nullCtx $ Set (nullCtx x,All) e
    x <+=> e = nullCtx $ Do $ nullCtx $ IncBy (nullCtx x,All) e
    x <-=> e = nullCtx $ Do $ nullCtx $ DecBy (nullCtx x,All) e
    x <*=> e = nullCtx $ Do $ nullCtx $ MulBy (nullCtx x,All) e
    x </=> e = nullCtx $ Do $ nullCtx $ DivBy (nullCtx x,All) e
    x <%=> e = nullCtx $ Do $ nullCtx $ ModBy (nullCtx x,All) e
    
instance Assignment (String,Component) CtxExpr where
    x <=> e = nullCtx $ Set (nullCtx $ fst x, snd x) e
    x <+=> e = nullCtx $ IncBy (nullCtx $ fst x, snd x) e
    x <-=> e = nullCtx $ DecBy (nullCtx $ fst x, snd x) e
    x <*=> e = nullCtx $ MulBy (nullCtx $ fst x, snd x) e
    x </=> e = nullCtx $ DivBy (nullCtx $ fst x, snd x) e
    x <%=> e = nullCtx $ ModBy (nullCtx $ fst x, snd x) e
    
instance Assignment (String,Component) CtxStmt where
    x <=> e = nullCtx $ Do $ nullCtx (Set (nullCtx $ fst x, snd x) e)
    x <+=> e = nullCtx $ Do $ nullCtx (IncBy (nullCtx $ fst x, snd x) e)
    x <-=> e = nullCtx $ Do $ nullCtx (DecBy (nullCtx $ fst x, snd x) e)
    x <*=> e = nullCtx $ Do $ nullCtx (MulBy (nullCtx $ fst x, snd x) e)
    x </=> e = nullCtx $ Do $ nullCtx (DivBy (nullCtx $ fst x, snd x) e)
    x <%=> e = nullCtx $ Do $ nullCtx (ModBy (nullCtx $ fst x, snd x) e)

class Init a where
    (<<-) :: a -> CtxExpr -> a
    
instance Init GlobDef where
    (GV v _) <<- e = (GV v $ Just e)
    
instance Init CtxStmt where
    (Ctx ctx (Decl v _)) <<- e = Ctx ctx (Decl v $ Just e)
   
class Call a where
    call :: String -> [CtxExpr] -> a
    
instance Call CtxExpr where
    call s = nullCtx . Call (nullCtx s)
    
instance Call CtxStmt where
    call n es = nullCtx $ Do $ nullCtx $ Call (nullCtx n) es

class AccessValue a where
    get :: a -> CtxExpr

instance AccessValue String where
    get s = nullCtx $ Get (nullCtx s,All)

instance AccessValue (String,Component) where
    get (s,c) = nullCtx $ Get (nullCtx s,c)

class Incable a where
    preinc :: a -> CtxExpr
    postinc :: a -> CtxExpr
    predec :: a -> CtxExpr
    postdec :: a -> CtxExpr

instance Incable String where
    preinc s = nullCtx $ PreInc (nullCtx s,All)
    postinc s = nullCtx $ PostInc (nullCtx s,All)
    predec s = nullCtx $ PreDec (nullCtx s,All)
    postdec s = nullCtx $ PostDec (nullCtx s,All)
    
instance Incable (String,Component) where
    preinc (s,c) = nullCtx $ PreInc (nullCtx s,c)
    postinc (s,c) = nullCtx $ PostInc (nullCtx s,c)
    predec (s,c) = nullCtx $ PreDec (nullCtx s,c)
    postdec (s,c) = nullCtx $ PostDec (nullCtx s,c)

slit s = nullCtx $ StringLit s
ilit i = nullCtx $ IntLit i
flit i = nullCtx $ FloatLit i
klit s = nullCtx $ KeyLit s

castString = nullCtx . Cast LLString
castInt = nullCtx . Cast LLInteger
castFloat = nullCtx . Cast LLFloat
castVector = nullCtx . Cast LLVector
castRot = nullCtx . Cast LLRot
castKey = nullCtx . Cast LLKey
castList = nullCtx . Cast LLList

(!*) = liftCtx2 Mul
(!/) = liftCtx2 Div
(!%) = liftCtx2 Mod
(!-) = liftCtx2 Sub
(!+) = liftCtx2 Add
(!<<) = liftCtx2 ShiftL
(!>>) = liftCtx2 ShiftR
(!==) = liftCtx2 Equal
(!!=) = liftCtx2 NotEqual
(!<) = liftCtx2 Lt
(!<=) = liftCtx2 Le
(!>) = liftCtx2 Gt
(!>=) = liftCtx2 Ge
(!&) = liftCtx2 BAnd
(!^) = liftCtx2 Xor
(!|) = liftCtx2 BOr
(!&&) = liftCtx2 And
(!||) = liftCtx2 Or

neg = nullCtx . Neg
inv = nullCtx . Inv
lslNot = nullCtx . Not

handler name = Handler (nullCtx name)
state name = State (nullCtx name)
mimport name = GI (nullCtx name)

infixl 8  !*, !/, !%
infixl 7  !-,!+
infixl 6  !<<, !>>
infix  5  !==, !!=, !<, !<=, !>=, !>
infix  4  !&,!^
infix  3  !|
infixr 2  !&&
infixr 1  !||
-- infixr 0  :=:, :+=:, :-=:, :*=:, :/=:, :%=:

liftCtx2 f x y = nullCtx (f x y)
