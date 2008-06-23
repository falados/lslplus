module Lsl.Structure (
    -- Types
    Expr(..),
    Var(..),
    FuncDec (..),
    Func(..),
    LModule(..),
    Component(..),
    Statement(..),
    LSLType(..),
    Handler(..),
    State(..),
    GlobDef(..),
    LSLScript(..),
    Validity(..),
    Global(..),
    SourceContext(..),
    Ctx(..),
    CompiledLSLScript,
    Library,
    AugmentedLibrary,
    -- Values
    fromMCtx,
    ctxItems,
    nullCtx,
    ctxVr2Vr,
    findFunc,
    validLSLScript,
    validLibrary,
    findState,
    predefFuncs,
    findFuncDec,
    goodHandlers,
    libFromAugLib,
    isTextLocation) where

import Lsl.Type
import Lsl.Constants
import Lsl.EventSigs
import Lsl.FuncSigs
import Data.List
import Data.Bits
import Lsl.Util
import Control.Monad
import Control.Monad.Error
import Debug.Trace

trace1 s v = trace (s ++ show v) v

type CtxVar = Ctx Var

data Var = Var { varName :: String, varType :: LSLType } deriving (Show)

type CtxName = Ctx String

data FuncDec = FuncDec { funcName :: CtxName, funcType :: LSLType, funcParms :: [CtxVar] }
    deriving (Show)

type CtxStmt = Ctx Statement
data Func = Func FuncDec [CtxStmt] deriving (Show)

data LModule = LModule [GlobDef] [CtxVar]
    deriving (Show)

type CtxExpr = Ctx Expr
data Expr = IntLit Int
          | FloatLit Float
          | StringLit String
          | ListExpr [CtxExpr]
          | VecExpr CtxExpr CtxExpr CtxExpr
          | RotExpr CtxExpr CtxExpr CtxExpr CtxExpr
          | KeyLit String
          | Call CtxName [CtxExpr]
          | Add CtxExpr CtxExpr
          | Sub CtxExpr CtxExpr
          | Mul CtxExpr CtxExpr
          | Div CtxExpr CtxExpr
          | Mod CtxExpr CtxExpr
          | Get (CtxName,Component)
          | Set (CtxName,Component) CtxExpr
          | BAnd CtxExpr CtxExpr
          | BOr CtxExpr CtxExpr
          | Xor CtxExpr CtxExpr
          | ShiftL CtxExpr CtxExpr
          | ShiftR CtxExpr CtxExpr
          | And CtxExpr CtxExpr
          | Or CtxExpr CtxExpr
          | Equal CtxExpr CtxExpr
          | NotEqual CtxExpr CtxExpr
          | Lt CtxExpr CtxExpr
          | Le CtxExpr CtxExpr
          | Gt CtxExpr CtxExpr
          | Ge CtxExpr CtxExpr
          | IncBy (CtxName,Component) CtxExpr
          | DecBy (CtxName,Component) CtxExpr
          | MulBy (CtxName,Component) CtxExpr
          | DivBy (CtxName,Component) CtxExpr
          | ModBy (CtxName,Component) CtxExpr
          | PostInc (CtxName,Component)
          | PostDec (CtxName,Component)
          | PreInc (CtxName,Component)
          | PreDec (CtxName,Component)
          | Not CtxExpr
          | Neg CtxExpr
          | Inv CtxExpr
          | Cast LSLType CtxExpr
            deriving (Show)

                          
data Statement = Compound [CtxStmt]
               | While CtxExpr Statement
               | DoWhile Statement CtxExpr
               | For ([CtxExpr]) (Maybe CtxExpr) ([CtxExpr]) Statement
               | If CtxExpr Statement Statement
               | Decl Var (Maybe CtxExpr)
               | NullStmt
               | Return (Maybe CtxExpr)
               | StateChange String
               | Do CtxExpr
               | Label String
               | Jump String
    deriving (Show)

isLabel (Label _) = True
isLabel _ = False
               
data Global = GDecl Var (Maybe Expr)
    deriving (Show)

data GlobDef = GV CtxVar (Maybe CtxExpr) | GF Func | GI CtxName [(String,String)] String
    deriving (Show)

data Handler = Handler CtxName [CtxVar] [CtxStmt]
    deriving (Show)
    
goodHandlers :: [(String,[LSLType])]
goodHandlers = simpleLslEventDescriptors
--     [ 
--     ("at_rot_target", [LLInteger,LLRot,LLRot]),
--     ("at_target", [LLInteger,LLVector,LLVector]),
--     ("attach",[LLKey]),
--     ("changed", [LLInteger]),
--     ("collision",[LLInteger]),
--     ("collision_end",[LLInteger]),
--     ("collision_start",[LLInteger]),
--     ("control",[LLKey,LLInteger,LLInteger]),
--     ("changed", [LLInteger]),
--     ("dataserver",[LLKey,LLString]),
--     ("email",[LLString,LLString,LLString,LLString,LLInteger]),
--     ("http_response",[LLKey,LLInteger,LLList,LLString]),
--     ("land_collision",[LLVector]),
--     ("land_collision_end",[LLVector]),
--     ("land_collision_start",[LLVector]),
--     ("link_message", [LLInteger, LLInteger, LLString, LLKey]),
--     ("listen", [LLInteger, LLString, LLKey, LLString]),
--     ("money",[LLKey,LLInteger]),
--     ("moving_end",[]),
--     ("moving_start",[]),
--     ("no_sensor",[]),
--     ("not_at_rot_target",[]),
--     ("not_at_target",[]),
--     ("object_rez",[LLKey]),
--     ("on_rez", [LLInteger]),
--     ("remote_data", [LLInteger,LLKey,LLKey,LLString,LLInteger,LLString]),
--     ("run_time_permissions", [LLInteger]),
--     ("sensor",[LLInteger]),
--     ("state_entry", []),
--     ("state_exit", []),
--     ("timer", []),
--     ("touch", [LLInteger]),
--     ("touch_start",[LLInteger]),
--     ("touch_end",[LLInteger])
--     ]
    
data State = State CtxName [Handler]
    deriving (Show)

data LSLScript = LSLScript [GlobDef] [State] deriving (Show)

type ModuleInfo = ([Global],[Func])
type Library = [(String,Validity LModule)]
type AugmentedLibrary = [(String,Validity (LModule,ModuleInfo))]

lslFunc (name,t,ts) =
    FuncDec (nullCtx name) t (zipWith (\ x y -> nullCtx $ Var [y] x) ts ['a'..])
predefFuncs = map lslFunc funcSigs

findVar name = find (\(Var n _ ) -> n == name)
findType name =
    let f Nothing = Nothing
        f (Just (Var _ t )) = Just t in f . (findVar name)
findFuncDec name = ctx ("finding function " ++ name) . findM (\ fd -> ctxItem (funcName fd) == name)
findState name = ctx ("finding state " ++ name) . findM (\ (State n _) -> ctxItem n == name)
findFunc name = ctx ("finding function " ++ name) . findM (\ (Func fd _) -> ctxItem (funcName fd) == name)

lookupModule name lib =
    case lookup name lib of
        Nothing -> fail ("unknown module")
        Just (Invalid (_,s)) -> fail ("invalid library (" ++ s ++ ")")
        Just (Valid m) -> return m
        
data Validity a = Invalid (SourceContext,String) | Valid a deriving (Show)

instance Monad Validity where
    return = Valid
    fail = Invalid . ((,)UnknownSourceContext)
    Valid x >>= k = k x
    Invalid x >>= _ = Invalid x
instance MonadPlus Validity where
    mzero = Invalid undefined
    (Invalid _) `mplus` y = y
    x `mplus` y = x
instance Functor Validity where
    fmap f (Invalid s) = (Invalid s)
    fmap f (Valid x) = Valid (f x)
        
vfail = Invalid

incontext (ctx,s) (Invalid (ctx',s')) =
    case ctx' of
        UnknownSourceContext -> Invalid (ctx,msg)
        _ -> Invalid (ctx',msg) 
    where msg = if null s then s' else s ++ ": " ++ s'
incontext _ v = v

incontext' (ctx,s) (Invalid (_,s')) = Invalid (ctx,s ++ ": " ++ s')
incontext' _ v = v

--------------------
matchTypes LLFloat LLInteger = True
matchTypes dest src = dest == src || (all (`elem` [LLKey,LLString]) [dest,src])

typeGlob library prefix (vars,funcs) (GV (Ctx ctx (Var name t)) _) = return ((Var (prefix ++ name) t):vars,funcs)
typeGlob library prefix (vars,funcs) (GF (Func (FuncDec name t params) _)) = 
    return (vars,(FuncDec (fmap (prefix++) name) t params):funcs)
typeGlob library prefix v@(vars,funcs) (GI moduleName _ prefix') =
    do (LModule globs _) <- lookupModule (ctxItem moduleName) library
       foldM (typeGlob library (prefix++prefix')) (vars,funcs) globs
typeGlobs library gs = foldM (typeGlob library "") ([],[]) gs

noDupVars :: [String] -> [CtxVar] -> Validity [String]
noDupVars used [] = return used
noDupVars used ((Ctx ctx (Var n t)):vs) = do
    when (n `elem` used) $ vfail (ctx, n ++ " already defined")
    noDupVars (n:used) vs

checkName :: Maybe SourceContext -> String -> [String] -> Validity ()
checkName (Just ctx) name names = 
    when (name `elem` names) $ vfail (ctx, name ++ " is multiply defined")

noDupGlobs :: Maybe SourceContext -> String -> [String] -> Library -> [GlobDef] -> Validity [String]
noDupGlobs forceCtx prefix usedNames library [] = return usedNames
noDupGlobs forceCtx prefix usedNames library ((GV (Ctx ctx (Var name t)) _):gs) = do
        checkName (forceCtx `mplus` Just ctx) globName usedNames
        noDupGlobs forceCtx prefix (globName:usedNames) library gs
    where (globName::String) = prefix ++ name
noDupGlobs forceCtx prefix usedNames library ((GF (Func (FuncDec (Ctx ctx name) t params) _)):gs) = do
        checkName (forceCtx `mplus` Just ctx) globName usedNames
        noDupGlobs forceCtx prefix (globName:usedNames) library gs
    where globName = prefix ++ name
noDupGlobs forceCtx prefix usedNames library ((GI (Ctx ctx moduleName) _ prefix'):gs) = do
        (LModule globs _) <- incontext (ctx,"") $ lookupModule moduleName library
        usedNames' <- noDupGlobs (forceCtx `mplus` Just ctx) (prefix ++ prefix') usedNames library globs
        noDupGlobs forceCtx prefix usedNames' library gs
        
type CompiledLSLScript = ([Global],[Func],[State])

validLSLScript :: Library -> LSLScript -> Validity CompiledLSLScript
validLSLScript library (LSLScript globs states) = 
    do  --noDupGlobs Nothing "" [] library globs
        (typedVars,typedFuncs) <- typeGlobs library globs
        let vars = reverse typedVars
        let funcDecs = typedFuncs ++ predefFuncs
        (globvars,funcs,_,_) <- foldM (validGlob library vars funcDecs) ([],[],[],[]) globs
        validStates [] vars funcDecs states
        return (reverse globvars,funcs,states)

validGlob _ vars funcDecs (globvars,funcs,imports,namesUsed) (GV v mexpr) =
    do when (isConstant $ varName v') $ vfail (srcCtx v, varName v' ++ " is a predefined constant")
       -- find the vars that are defined prior to this global variable -- only one of these
       -- vars may be used to initialize the global variable.
       when (varName v' `elem` namesUsed) $ vfail (srcCtx v, varName v' ++ " is already defined")
       let (vars',_) = break (\ var -> varName var == varName v') vars
       case mexpr of
           Nothing -> return (GDecl v' Nothing:globvars,funcs,imports, (varName v'):namesUsed)
           Just expr -> do
               t <- validCtxSimple vars' expr
               let vt = varType v'
               when (not (matchTypes vt t)) $ vfail (srcCtx expr, "expression not of the correct type")
               return ((GDecl v' $ Just (ctxItem expr)):globvars,funcs,imports, (varName v'):namesUsed)
    where v' = ctxItem v
validGlob _ vars funcDecs (globvars,funcs,imports,namesUsed) (GF f@(Func (FuncDec name t params) statements)) =
    do  noDupVars [] params
        when (ctxItem name `elem` namesUsed) $ vfail (srcCtx name, ctxItem name ++ " is already defined")
        returns <- validStatements funcDecs vars t [] [[],params'] statements
        when (not returns && t /= LLVoid) $
            vfail (srcCtx name, "function " ++ (ctxItem name) ++ ": not all code paths return a value")
        return (globvars,f:funcs,imports,(ctxItem name):namesUsed)
    where params' = ctxItems params
validGlob library vars funcDecs vstate@(globvars,funcs,imports,namesUsed) (GI (Ctx ctx name) bindings prefix) =
    let context = incontext' (ctx,"module " ++ name) in
    do  let imp = (name,sort bindings,prefix)
        if imp `elem` imports 
            then return (globvars,funcs,imports,namesUsed) 
            else context $ do
                (LModule globs freevars) <- context $ lookupModule name library
                context $ validBindings vars freevars bindings
                (vars',funcDecs') <- context $ typeGlobs library globs
                let renames = bindings ++ (map (\ x -> (x,prefix ++ x)) ((map varName vars') ++ (funcNames funcDecs')))
                (gvs,fs,imports',namesUsed') <- foldM (rewriteGlob prefix library renames ((map ctxItem freevars) ++ vars')) vstate globs
                return (gvs,fs,imp:imports',namesUsed')

rewriteGlob _ _ renames vars (globvars,funcs,imports,namesUsed) (GF (Func (FuncDec name t params) statements)) =
    do  name' <- incontext (srcCtx name,  "renaming function " ++ ctxItem name ++ ", " ++ show renames) $ lookupM (ctxItem name) renames
        when (name' `elem` namesUsed) $ fail (name' ++ " imported from module is already defined")
        let rewrittenFunc = (Func (FuncDec (Ctx (srcCtx name) name') t params) $ rewriteStatements 0 renames statements)
        return (globvars,rewrittenFunc:funcs,imports,name':namesUsed)
rewriteGlob _ _ renames vars (globvars,funcs,imports,namesUsed) (GV (Ctx ctx (Var name t)) mexpr) =
    do  name' <- incontext (ctx,"renaming variable " ++ name) $ lookupM name renames
        when (name' `elem` namesUsed) $ fail (name' ++ " imported from module is already defined")
        let rewrittenGlobVar = GDecl (Var name' t) $
                case mexpr of
                    Nothing -> Nothing
                    Just expr -> Just $ (ctxItem (rewriteCtxExpr renames expr))
        return (rewrittenGlobVar:globvars,funcs,imports,name':namesUsed)
rewriteGlob prefix0 library renames vars vstate@(globvars,funcs,imports,namesUsed) (GI (Ctx ctx mName) bindings prefix) =
    do  (LModule globs freevars) <- incontext (ctx, "rewriting module " ++ mName) $ lookupModule mName library
        incontext (ctx,"") $ validBindings vars freevars bindings
        bindings' <- mapM rewriteBinding bindings
        let imp = (mName,sort bindings',prefix0 ++ prefix)
        if (imp `elem` imports)
            then return (globvars,funcs,imports,namesUsed)
            else do
                (vars',funcDecs') <- typeGlobs library globs
                let renames = bindings' ++ map (\ x -> (x,prefix0 ++ prefix ++ x)) (map varName vars' ++ map (ctxItem . funcName) funcDecs')
                (gvs,fs,imports',namesUsed') <- foldM (rewriteGlob (prefix0 ++ prefix) library renames vars') vstate globs
                return (gvs,fs,imp:imports',namesUsed')
    where rewriteBinding (fv,rn) = lookupM rn renames >>= return . ((,) fv)

validBindings vars freevars bindings = 
    if length freevars /= length bindings then
        fail ("wrong number of bindings in import: " ++ (show $ length freevars) ++ " required")
    else let f [] = return ()
             f ((x,y):xys) = 
                case (findType x (ctxItems freevars), findType y vars) of
                    (Nothing,_) -> fail ("free variable " ++ x ++ " not found")
                    (_,Nothing) -> fail ("global variable " ++ y ++ " not found")
                    (Just t0,Just t1) | not (matchTypes t0 t1) -> fail ("types of " ++ x ++ " and " ++ y ++ " don't match")
                            | otherwise -> f xys
         in f bindings

validState used vars funcs (State (Ctx ctx name) handlers) =
    do when (name `elem` used) $ vfail (ctx, name ++ " already used")
       incontext (ctx,"") $ validHandlers [] funcs vars handlers
       return name
       
validStates used vars funcs [] = return ()
validStates used vars funcs (s:ss) =
    do  name <- validState used vars funcs s
        validStates (name:used) vars funcs ss
        
validCast t0 t1 =
    let validCasts = [(LLInteger,LLFloat), (LLFloat,LLInteger),
                      (LLInteger,LLString),(LLString,LLInteger),
                      (LLFloat,LLString),(LLString,LLFloat),
                      (LLString,LLVector),(LLVector,LLString),
                      (LLString,LLKey),(LLKey,LLString),
                      (LLRot,LLString),(LLString,LLRot),
                      (LLList,LLString),(LLString,LLList)] in
    do when (t0 /= t1 && (t0,t1) `notElem` validCasts) $ fail ("can't cast from " ++ (lslTypeString t0) ++ " to " ++ (lslTypeString t1))

validCtxSimple :: [Var] -> Ctx Expr -> Validity LSLType
validCtxSimple vars (Ctx ctx expr) = incontext (ctx,"") $ validSimple vars expr
       
validSimple :: [Var] -> Expr -> Validity LSLType
validSimple vars (IntLit i) = return LLInteger
validSimple vars (FloatLit f) = return LLFloat
validSimple vars (StringLit s) = return LLString
validSimple vars (KeyLit k) = return LLKey
validSimple vars (Get (Ctx ctx name,All)) = 
    (do (Var _ t) <- incontext (ctx, "variable " ++ name) $ findM (\ v -> varName v == name) vars
        return t)
    `mplus` (findConstType name)
validSimple vars (Get (Ctx ctx name,_)) = vfail (ctx,"can't access vector/rotation component in global variable initialization")
validSimple vars (ListExpr []) = return LLList
validSimple vars (ListExpr (e:es)) = 
    do t <- validCtxSimple vars e
       when (t == LLList) $ vfail (srcCtx e,"lists cannot contain other lists")
       validSimple vars (ListExpr es)
validSimple vars (VecExpr e1 e2 e3) = validSimpleStructure vars LLVector [e1,e2,e3]
validSimple vars (RotExpr e1 e2 e3 e4) = validSimpleStructure vars LLRot [e1,e2,e3,e4]
validSimple vars (Neg e) = 
    do t <- validCtxSimple vars e
       when (t `notElem` [LLFloat, LLInteger]) $ vfail (srcCtx e,"operator only applicable to integers and floats in this context")
       return t
validSimple vars e = fail ("expression is not valid in a static context.")

validSimpleStructure vars t [] = return t
validSimpleStructure vars t (e:es) =
    do  t' <- validCtxSimple vars e
        when (t' `notElem` [LLFloat,LLInteger]) $ vfail (srcCtx e, "literal of type " ++
             (lslTypeString t') ++ " not a valid element of " ++ (lslTypeString t))
        validSimpleStructure vars t es

validExpression :: Expr -> [FuncDec] -> [Var] -> [[Var]] -> Validity LSLType
validExpression (Cast t expr) funcs vars locals  = 
   do t' <- validCtxExpr expr funcs vars locals
      incontext (srcCtx expr, "") $ validCast t' t
      return t
validExpression (Get ((Ctx ctx name),component)) funcs vars locals =
   case (findType name (concat locals ++ vars) `mplus` findConstType name,component) of
       (Nothing,_) -> vfail (ctx, "undefined variable or constant: " ++ name)
       (Just LLRot,All) -> return LLRot
       (Just LLRot,_) -> return LLFloat
       (Just LLVector,All) -> return LLVector
       (Just LLVector,S) -> vfail (ctx,"s is not a valid component of a vector")
       (Just LLVector,_) -> return LLFloat
       (Just t,All) -> return t
       (Just t,_) -> vfail (ctx,"only vectors and rotations have components")
validExpression (Call name exprs) funcs vars locals = validCall funcs vars locals name exprs
validExpression (Not expr) funcs vars locals =
    do t <- validCtxExpr expr funcs vars locals
       when (t /= LLInteger) $ vfail (srcCtx expr, "expression is not an integer expression, which is required for applying the Not operator")
       return t
validExpression (Neg expr) funcs vars locals =
    do t <- validCtxExpr expr funcs vars locals
       when (t == LLList) $ vfail (srcCtx expr, "operator not applicable to list type")
       return t
validExpression (Inv expr) funcs vars locals =
    do t <- validCtxExpr expr funcs vars locals
       when (t /= LLInteger) $ vfail (srcCtx expr, "expression is not an integer expression, which is required for applying the inverse operator")
       return t
validExpression plus@(Add expr1 expr2) funcs vars locals =
    do  (t1,t2) <- validEach (expr1,expr2) funcs vars locals
        case (t1,t2) of
            (LLInteger,LLInteger) -> return LLInteger
            (LLInteger,LLFloat) -> return LLFloat
            (LLFloat,LLInteger) -> return LLFloat
            (LLFloat,LLFloat) -> return LLFloat
            (LLVector,LLVector) -> return LLVector
            (LLRot,LLRot) -> return LLRot
            (LLString,LLString) -> return LLString
            (LLList,LLList) -> return LLList
            (t,LLList) -> return LLList
            (LLList,t) -> return LLList
            (t0,t1) -> incompatibleOperands plus t0 t1
validExpression minus@(Sub expr1 expr2) funcs vars locals =
    do  (t1,t2) <- validEach (expr1,expr2) funcs vars locals
        case (t1,t2) of
            (LLInteger,LLInteger) -> return LLInteger
            (LLInteger,LLFloat) -> return LLFloat
            (LLFloat,LLInteger) -> return LLFloat
            (LLFloat,LLFloat) -> return LLFloat
            (LLVector,LLVector) -> return LLVector
            (LLRot,LLRot) -> return LLRot
            (t0,t1) -> incompatibleOperands minus t0 t1
validExpression expr@(Mul expr1 expr2) funcs vars locals=
    do  (t1,t2) <- validEach (expr1,expr2) funcs vars locals
        case (t1,t2) of
            (LLInteger,LLInteger) -> return LLInteger
            (LLInteger,LLFloat) -> return LLFloat
            (LLFloat,LLInteger) -> return LLFloat
            (LLFloat,LLFloat) -> return LLFloat
            (LLVector,LLInteger) -> return LLVector
            (LLVector,LLFloat) -> return LLVector
            (LLFloat,LLVector) -> return LLVector
            (LLInteger,LLVector) -> return LLVector
            (LLVector,LLVector) -> return LLFloat
            (LLVector,LLRot) -> return LLVector
            (LLRot,LLRot) -> return LLRot
            (t0,t1) -> incompatibleOperands expr t0 t1
validExpression expr@(Div expr1 expr2) funcs vars locals =
    do  (t1,t2) <- validEach (expr1,expr2) funcs vars locals
        case (t1,t2) of
            (LLInteger,LLInteger) -> return LLInteger
            (LLInteger,LLFloat) -> return LLFloat
            (LLFloat,LLInteger) -> return LLFloat
            (LLFloat,LLFloat) -> return LLFloat
            (LLVector,LLInteger) -> return LLVector
            (LLVector,LLFloat) -> return LLVector
            (LLVector,LLRot) -> return LLVector
            (LLRot,LLRot) -> return LLRot
            (t0,t1) -> incompatibleOperands expr t0 t1
validExpression expr@(Mod expr1 expr2) funcs vars locals =
    do  (t1,t2) <- validEach (expr1,expr2) funcs vars locals
        case (t1,t2) of
            (LLInteger,LLInteger) -> return LLInteger
            (LLVector,LLVector) -> return LLVector
            _ -> incompatibleOperands expr t1 t2
validExpression e@(Equal expr1 expr2) funcs vars locals =
    do (t1,t2) <- validEach (expr1,expr2) funcs vars locals
       case (t1,t2) of
           (LLInteger,LLFloat) -> return LLInteger
           (LLFloat,LLInteger) -> return LLInteger
           (LLString,LLKey) -> return LLInteger
           (LLKey,LLString) -> return LLInteger
           (t1,t2) | (t1 == t2) -> return LLInteger
                   | otherwise  -> incompatibleOperands e t1 t2
validExpression e@(NotEqual expr1 expr2) funcs vars locals =
    do (t1,t2) <- validEach (expr1,expr2) funcs vars locals
       case (t1,t2) of
           (LLInteger,LLFloat) -> return LLInteger
           (LLFloat,LLInteger) -> return LLInteger
           (LLString,LLKey) -> return LLInteger
           (LLKey,LLString) -> return LLInteger
           (t1,t2) | (t1 == t2) -> return LLInteger
                   | otherwise  -> incompatibleOperands e t1 t2
validExpression e@(BAnd expr1 expr2) funcs vars locals = validBothInteger (expr1,expr2) funcs vars locals
validExpression e@(BOr expr1 expr2) funcs vars locals = validBothInteger (expr1,expr2) funcs vars locals
validExpression e@(Xor expr1 expr2) funcs vars locals = validBothInteger (expr1,expr2) funcs vars locals
validExpression e@(ShiftL expr1 expr2) funcs vars locals = validBothInteger (expr1,expr2) funcs vars locals
validExpression e@(ShiftR expr1 expr2) funcs vars locals = validBothInteger (expr1,expr2) funcs vars locals
validExpression e@(Gt expr1 expr2) funcs vars locals = validRelExpr (expr1,expr2) funcs vars locals
validExpression e@(Ge expr1 expr2) funcs vars locals = validRelExpr (expr1,expr2) funcs vars locals
validExpression e@(Le expr1 expr2) funcs vars locals = validRelExpr (expr1,expr2) funcs vars locals
validExpression e@(Lt expr1 expr2) funcs vars locals = validRelExpr (expr1, expr2) funcs vars locals
validExpression e@(And expr1 expr2) funcs vars locals = validBothInteger (expr1, expr2) funcs vars locals
validExpression e@(Or expr1 expr2) funcs vars locals = validBothInteger (expr1, expr2) funcs vars locals
validExpression e@(IncBy (name,All) expr) funcs vars locals =
    do  failIfNoModify name
        (t1,t2) <- validNameExpr (name,expr) funcs vars locals
        case (t1,t2) of
            (LLInteger,LLInteger) -> return LLInteger
            (LLFloat,LLInteger) -> return LLFloat
            (LLFloat,LLFloat) -> return LLFloat
            (LLVector,LLVector) -> return LLVector
            (LLRot,LLRot) -> return LLRot
            (LLString,LLString) -> return LLString
            (LLList,LLList) -> return LLList
            (LLList,t) -> return LLList
            (t0,t1) -> incompatibleOperands e t0 t1
validExpression e@(IncBy (name,_) expr) funcs vars locals =
    do  failIfNoModify name
        (t1,t2) <- validNameExpr (name,expr) funcs vars locals
        case (t1,t2) of
            (t1,t2) | t1 `elem` [LLVector,LLRot] && t2 `elem` [LLFloat,LLInteger] -> return LLFloat
                          | otherwise -> incompatibleOperands e t1 t2
validExpression e@(DecBy (name,All) expr) funcs vars locals =
    do  failIfNoModify name
        (t1,t2) <- validNameExpr (name,expr) funcs vars locals
        case (t1,t2) of
            (LLInteger,LLInteger) -> return LLInteger
            (LLFloat,LLInteger) -> return LLFloat
            (LLFloat,LLFloat) -> return LLFloat
            (LLVector,LLVector) -> return LLVector
            (LLRot,LLRot) -> return LLRot
            (t0,t1) -> incompatibleOperands e t0 t1
validExpression e@(DecBy (name,_) expr) funcs vars locals =
    do  failIfNoModify name 
        (t1,t2) <- validNameExpr (name,expr) funcs vars locals
        case (t1,t2) of
            (t1,t2) | t1 `elem` [LLVector,LLRot] && t2 `elem` [LLFloat,LLInteger] -> return LLFloat
                    | otherwise                                                   -> incompatibleOperands e t1 t2
validExpression e@(MulBy (name,All) expr) funcs vars locals =
    do  failIfNoModify name
        (t1,t2) <- validNameExpr (name,expr) funcs vars locals
        case (t1,t2) of
            (LLInteger,LLInteger) -> return LLInteger
            (LLFloat,LLInteger) -> return LLFloat
            (LLFloat,LLFloat) -> return LLFloat
            (LLVector,LLInteger) -> return LLVector
            (LLVector,LLFloat) -> return LLVector
            (LLVector,LLVector) -> return LLVector -- note: LSL compiles this, but it results in runtime error!
            (LLVector,LLRot) -> return LLVector
            (LLRot,LLRot) -> return LLRot
            (t0,t1) -> incompatibleOperands e t0 t1
validExpression e@(MulBy (name,_) expr) funcs vars locals =
    do  failIfNoModify name
        (t1,t2) <- validNameExpr (name,expr) funcs vars locals
        case (t1,t2) of
            (t1,t2) | t1 `elem` [LLVector,LLRot] && t2 `elem` [LLFloat,LLInteger] -> return LLFloat
                    | otherwise -> incompatibleOperands e t1 t2
validExpression e@(DivBy (name,All) expr) funcs vars locals =
    do  failIfNoModify name
        (t1,t2) <- validNameExpr (name,expr) funcs vars locals
        case (t1,t2) of
            (LLInteger,LLInteger) -> return LLInteger
            (LLFloat,LLInteger) -> return LLFloat
            (LLFloat,LLFloat) -> return LLFloat
            (LLVector,LLInteger) -> return LLVector
            (LLVector,LLFloat) -> return LLVector
            (LLVector,LLRot) -> return LLVector
            (LLRot,LLRot) -> return LLRot
            (t0,t1) -> incompatibleOperands e t0 t1
validExpression e@(DivBy (name,_) expr) funcs vars locals =
    do  failIfNoModify name
        (t1,t2) <- validNameExpr (name,expr) funcs vars locals
        case (t1,t2) of
            (t1,t2) | t1 `elem` [LLVector,LLRot] && t2 `elem` [LLFloat,LLInteger] -> return LLFloat
                    | otherwise                                                   -> incompatibleOperands e t1 t2
validExpression e@(ModBy (name,All) expr) funcs vars locals =
    do  failIfNoModify name
        (t1,t2) <- validNameExpr (name,expr) funcs vars locals
        case (t1,t2) of
            (LLInteger,LLInteger) -> return LLInteger
            (LLVector,LLVector) -> return LLVector
            (t0,t1) -> incompatibleOperands e t0 t1
validExpression e@(ModBy (name,_) expr) funcs vars locals =
    do  failIfNoModify name 
        (t1,t2) <- validNameExpr (name,expr) funcs vars locals
        case (t1,t2) of
            (t1,t2) -> incompatibleOperands e t1 t2
validExpression e@(PostInc var) funcs vars locals = validIncDecOp var vars locals "++"
validExpression e@(PostDec var) funcs vars locals = validIncDecOp var vars locals "--"
validExpression e@(PreInc var) funcs vars locals = validIncDecOp var vars locals "++"
validExpression e@(PreDec var) funcs vars locals = validIncDecOp var vars locals "++"
validExpression expr0@(Set (name,All) expr) funcs vars locals =
    do  failIfNoModify name
        (t1,t2) <- validNameExpr (name,expr) funcs vars locals
        case (t1,t2) of
            (LLFloat,LLInteger) -> return LLFloat
            (LLKey,LLString) -> return LLKey
            (LLString,LLKey) -> return LLString
            (t1,t2) | t1 == t2 -> return t1
                    | otherwise -> incompatibleOperands expr0 t1 t2
validExpression expr0@(Set (name,S) expr) funcs vars locals =
    do  failIfNoModify name
        (t1,t2) <- validNameExpr (name,expr) funcs vars locals
        case (t1,t2) of
            (LLRot,LLFloat) -> return LLFloat
            (LLRot,LLInteger) -> return LLFloat
            (t0,t1) -> incompatibleOperands expr0 t0 t1
validExpression expr0@(Set (name,_) expr) funcs vars locals =
    do  failIfNoModify name
        (t1,t2) <- validNameExpr (name,expr) funcs vars locals
        case (t1,t2) of
            (LLVector,LLFloat) -> return LLFloat
            (LLVector,LLInteger) -> return LLFloat
            (LLRot,LLFloat) -> return LLFloat
            (LLRot,LLInteger) -> return LLFloat
            (t0,t1) -> incompatibleOperands expr0 t0 t1
validExpression (IntLit i) _ _ _ = return LLInteger
validExpression (FloatLit _) _ _ _ = return LLFloat
validExpression (StringLit _) _ _ _ = return LLString
validExpression (KeyLit _) _ _ _ = return LLKey
validExpression (ListExpr es) fs vs ls = do
    mapM (\ e -> validListExprElement e fs vs ls) es
    return LLList
validExpression (VecExpr xExpr yExpr zExpr) funcs vars locals = 
    do  xt <- validCtxExpr xExpr funcs vars locals
        yt <- validCtxExpr yExpr funcs vars locals
        zt <- validCtxExpr zExpr funcs vars locals
        when (not (all (`elem` [LLInteger,LLFloat]) [xt,yt,zt])) $ fail "invalid components for vector"
        return LLVector
validExpression (RotExpr xExpr yExpr zExpr sExpr) funcs vars locals = 
    do  xt <- validCtxExpr xExpr funcs vars locals
        yt <- validCtxExpr yExpr funcs vars locals
        zt <- validCtxExpr zExpr funcs vars locals
        st <- validCtxExpr sExpr funcs vars locals
        when (not (all (`elem` [LLInteger,LLFloat]) [xt,yt,zt,st])) $ fail "invalid components for rotation"
        return LLRot
--validExpression x funcs vars locals = error ("what to do with " ++ (show x))

validListExprElement (Ctx ctx e) funcs vars locals = do
    t <- validExpression e funcs vars locals
    when (t `elem` [LLVoid,LLList]) $ vfail (ctx,"invalid type for list element")
    return ()

validMExpression Nothing funcs vars locals = return LLVoid
validMExpression (Just expr) funcs vars locals = validCtxExpr expr funcs vars locals

validExpressions es funcs vars locals = mapM_ (\ e -> validCtxExpr e funcs vars locals) es

validRelExpr (expr1,expr2) funcs vars locals =
    do (t1,t2) <- validEach (expr1,expr2) funcs vars locals
       case (t1,t2) of
           (LLInteger,LLInteger) -> return LLInteger
           (LLInteger,LLFloat) -> return LLInteger
           (LLFloat, LLInteger) -> return LLInteger
           (LLFloat, LLFloat) -> return LLInteger
           (t0,t1) -> fail ("operands are of incompatible types")
validBothInteger (expr1, expr2) funcs vars locals =
    do (t1,t2) <- validEach (expr1,expr2) funcs vars locals
       when (t1 /= LLInteger || t2 /= LLInteger) $ fail ("operands are of incompatible types") 
       return LLInteger
validEach (expr1, expr2) funcs vars locals =
    do t1 <- validCtxExpr expr1 funcs vars locals
       t2 <- validCtxExpr expr2 funcs vars locals
       return (t1,t2)

validNameExpr (Ctx ctx name, expr) funcs vars locals = 
    case (findType name (concat locals ++ vars), 
          validCtxExpr expr funcs vars locals) of
        (Just t1, Valid t2) -> return (t1,t2)
        (Nothing, _) -> vfail (ctx, "variable " ++ name ++ " not defined")
        (_,Invalid s) -> vfail s
validCall funcs vars locals (Ctx ctx fname) exprs =
    do  (FuncDec _ t params) <- findFuncDec fname funcs
        let vArg _ [] [] = return ()
            vArg _ (p:ps) [] = vfail (ctx, "mismatch of arguments vs. formal paramters in call to function " ++ fname)
            vArg _ [] (a:as) = vfail (ctx, "mismatch of arguments vs. formal paramters in call to function " ++ fname)
            vArg n (Var name t:ts) (arg:args) = 
              do t' <- validCtxExpr arg funcs vars locals
                 when (not (matchTypes t t')) $ vfail (ctx, "argument " ++ (show n) ++ " in call to function (" ++ fname ++ ") is of wrong type:" ++ (lslTypeString t') ++ ", should be " ++ (lslTypeString t))
                 vArg (n+1) ts args
        vArg 1 (ctxItems params) exprs
        return t

validCtxExpr (Ctx ctx e) fs vs ls = incontext (ctx,"") $ validExpression e fs vs ls

validIncDecOp (n@(Ctx ctx name),c) vars locals op =
    do  failIfNoModify n
        case (findType name (concat locals ++ vars),c) of
            (Nothing,_) ->  vfail (ctx, "variable " ++ name ++ " not found")
            (Just LLInteger,All) -> return LLInteger
            (Just LLFloat,All) -> return LLFloat
            (Just LLRot,S) -> return LLFloat
            (Just LLVector,S) -> vfail (ctx, "s is not a valid component of " ++ name)
            (Just t,All) -> vfail (ctx, name ++ " is not a valid operand for " ++ op)
            (Just LLVector,_) -> return LLFloat
            (Just LLRot,_) -> return LLFloat
            _ -> vfail (ctx, name ++ " is not a valid operand for " ++ op)

failIfNoModify (Ctx ctx name) = 
    when (isConstant name) $ vfail (ctx,"cannot modify " ++ name ++ " because it is a constant")

incompatibleOperands expr t0 t1 = 
    fail ("types of the operands aren't compatible (" ++ 
             (lslTypeString t0) ++ " vs. " ++ (lslTypeString t1) ++ ")")

defined :: String -> [Var] -> Bool
defined n = any (\ (Var n' _) -> n == n')

validStatement funcs vars rtype labels locals@(scope:scopes) returns (Decl var@(Var name t) expr) = 
    do when (defined name $ concat locals) $ fail ("variable " ++ name ++ " already defined") -- can't hide another local, even in a surrounding scope
       when (isConstant name) $ fail ("variable " ++ name ++ " is a predefined constant")
       case expr of
           Nothing -> return ((var:scope):scopes,returns)
           Just expr' -> do t' <- validCtxExpr expr' funcs vars locals
                            when (not $ matchTypes t t') $ vfail (srcCtx expr', "type of expression in declaration of " ++ name ++ " does not match " ++ lslTypeString t)
                            return ((var:scope):scopes,returns)
validStatement funcs vars rtype labels locals returns (While expr statement) =
    do t <- validCtxExpr expr funcs vars locals
       --when (t /= LLInteger) $ vfail (srcCtx expr, "expression is not a valid loop condition")
       validStatement funcs vars rtype labels locals False statement
       return (locals,returns)
validStatement funcs vars rtype labels locals returns (DoWhile statement expr) =
    do t <- validCtxExpr expr funcs vars locals
       --when (t /= LLInteger) $ vfail (srcCtx expr, "expression is not a valid loop condition")
       validStatement funcs vars rtype labels locals False statement
       return (locals,returns)
validStatement funcs vars rtype labels locals returns (For mexpr1 mexpr2 mexpr3 statement) =
    do  validExpressions mexpr1 funcs vars locals
        validExpressions mexpr3 funcs vars locals
        t <- validMExpression mexpr2 funcs vars locals
        --when (t /= LLInteger) $ fail ("expression is not a valid loop condition")
        validStatement funcs vars rtype labels locals False statement
        return (locals,returns)
validStatement funcs vars rtype labels locals returns (If expr thenStmt elseStmt) =
    do t <- validCtxExpr expr funcs vars locals
       --when (t /= LLInteger) $ vfail (srcCtx expr, "expression is not a valid 'if' condition")
       (_,ret1) <- validStatement funcs vars rtype labels locals False thenStmt
       (_,ret2) <- validStatement funcs vars rtype labels locals False elseStmt
       return (locals,returns || ret1 && ret2)
validStatement _ _ _ _ locals returns NullStmt = return (locals,returns)
validStatement funcs vars rtype labels locals _ (Return Nothing) = 
    do  when (rtype /= LLVoid) (fail "function must return a value")
        return (locals,True)
validStatement funcs vars rtype labels locals _ (Return (Just expr)) = 
    do  t <- validCtxExpr expr funcs vars locals
        when (t /= rtype && not (all (`elem` [LLString,LLKey]) [t,rtype])) (fail "inappropriate return type for function/handler")
        return (locals,True)
validStatement funcs vars rtype labels locals returns (StateChange name) = return (locals,returns)
validStatement funcs vars rtype labels locals returns (Do expr) = validCtxExpr expr funcs vars locals>>return (locals,returns)
validStatement funcs vars rtype labels locals returns (Compound stmts) = 
    do  returns' <- validStatements funcs vars rtype labels ([]:locals) stmts
        return (locals,returns || returns')
validStatement funcs vars rtype labels locals _ (Label _) = return (locals,False)
validStatement funcs vars rtype labels locals returns (Jump s) = 
    do when (s `notElem` concat labels) $ fail ("no such label to jump to: " ++ s)
       return (locals,returns)

validStatement' funcs vars rtype labels locals returns line (Ctx ctx stmt) = 
  incontext (ctx, "") $ validStatement funcs vars rtype labels locals returns stmt

validStatements :: [FuncDec] -> [Var] -> LSLType -> [[String]] -> [[Var]] -> [CtxStmt] -> Validity Bool
validStatements funcs vars rtype labels locals stmts =
    do let newLabels = map (\ (Label s) -> s) $ filter isLabel (ctxItems stmts)
       (_,r') <- foldM (\ (l,r) (n, s) -> 
           validStatement' funcs vars rtype (newLabels:labels) l r n s) (locals,False) $ zip [1..] stmts
       return r'

validHandler used funcs vars (Handler (Ctx ctx name) args stmts) = 
    do  when (name `elem` used) $ vfail (ctx,name ++ " already used in state")
        types <- incontext (ctx,"handler: ") $ lookupM name goodHandlers
        when (types /= map varType args') $ vfail (ctx,"invalid argument types for handler " ++ name)
        when (length args /= (length $ nub $ map varName args')) $ vfail (ctx,"not all argument names are unique for handler " ++ name)
        validStatements funcs vars LLVoid [] [[],args'] stmts
        return name
    where args' = ctxItems args
        
validHandlers _ _ _ [] = return ()
validHandlers used funcs vars (h:hs) =
    do  name <- validHandler used funcs vars h
        validHandlers (name:used) funcs vars hs

-- Validating a library of modules

-- ********
validModule library m@(LModule globs freevars) = 
    do --used <- noDupGlobs Nothing "" [] library globs
       (typedVars, typedFuncs) <- typeGlobs library globs
       let used = (map varName typedVars) ++ (map (ctxItem . funcName) typedFuncs)
       noDupVars used freevars
       let vars = freevars' ++ reverse typedVars
       let funcDecs = typedFuncs ++ predefFuncs
       (vs,fs,_,_) <- foldM (validGlob library vars funcDecs) ([],[],[],[]) globs
       return (vs,fs)
    where freevars' = ctxItems freevars

-- this function isn't partiuclarly efficient!
moduleDependencies lib chain m =
    let f (GI s _ _) = Just (ctxItem s)
        f _          = Nothing
    in  do  (LModule globs _) <- lookupM m lib
            case filtMap f globs of
                [] -> return []
                list -> if any (`elem` list) (m:chain) then fail ("circular dependency")
                        else
                            do deps <- fmap concat (
                                   let chain' = (m:chain) in
                                       mapM (moduleDependencies lib chain') list)
                               return $ nub (list ++ deps)
                                
-- sort modules by dependency: for each module in the list, after sorting that module
-- will depend only on modules preceding it in the list.  This of course implies that
-- there can be no circular dependencies in the modules.
sortModules :: [(String,(LModule,[String]))] -> [(String,LModule)]
sortModules modules =
    let cmp (name,(_,deplist)) (name',(_,deplist')) = compare (length deplist, name) (length deplist', name')
        sort1 [] = []
        sort1 list =
           let sorted = sortBy cmp list
               (nodeps,deps) = span ((==0).length.snd.snd) sorted 
               exclude = map fst nodeps 
               newlist = if length nodeps == 0 then error "circular depencencies in library"
                         else map (\ (nm,(m,l)) -> (nm, (m,filter (`notElem` exclude) l))) deps
           in nodeps ++ sort1 newlist 
   in map (\ (s,(m,_)) -> (s,m)) $ sort1 modules
       
validLibrary modules =
    let checkDep (n,m) = case moduleDependencies modules [] n of
            Valid deps -> (n,Valid (m,deps))
            Invalid s -> (n,Invalid s)
        categorize (good,bad) (n,Invalid s) = (good,(n,s):bad)
        categorize (good,bad) (n,Valid (m,deps)) = ((n,(m,deps)):good,bad)
        (good,bad) = foldl categorize ([],[]) $ map checkDep modules
        sorted = sortModules good
        validate augLib (name,m) =
            case validModule (libFromAugLib augLib) m of
                Invalid s -> (name, Invalid s):augLib
                Valid gs -> (name,Valid (m,gs)):augLib
    in (foldl validate [] sorted) ++ (map (\ (n,s) -> (n,Invalid s)) bad)

libFromAugLib :: AugmentedLibrary -> Library
libFromAugLib augLib = 
   let f (name,Invalid s) = (name,Invalid s)
       f (name,Valid (lm,_)) = (name, Valid lm)
   in map f augLib
   
tstLib = [
    ("alpha", LModule [GI (nullCtx "beta") [] []] []),
    ("beta", LModule  [GI (nullCtx "gamma") [] []] []),
    ("gamma", LModule [GI (nullCtx "alpha") [] []] []),
    ("omega", LModule [GI (nullCtx "lambda") [] []] []),
    ("lambda", LModule [GI (nullCtx "kappa") [] [], GI (nullCtx "sigma") [] []] []),
    ("kappa", LModule [] []),
    ("sigma", LModule [] [])]
    
rewriteName renames (Ctx ctx name) =
    case lookup name renames of
        Nothing -> Ctx ctx name
        Just name' -> Ctx ctx name'
rewriteStatements _ _ [] = []
rewriteStatements n bindings (Ctx c s:ss) =
    let (n',bindings',s') = rewriteStatement n bindings s in
       (Ctx c s'):(rewriteStatements n' bindings' ss)

rewriteStatement n bindings (Compound stmts) = (n, bindings, Compound $ rewriteStatements n bindings stmts)
rewriteStatement n bindings (While expr stmt) = 
    let (_,_,stmt') = rewriteStatement n bindings stmt in
    (n, bindings, While (rewriteCtxExpr bindings expr) stmt')
rewriteStatement n bindings (DoWhile stmt expr) =
    let (_,_,stmt') = rewriteStatement n bindings stmt in
    (n, bindings, DoWhile stmt' (rewriteCtxExpr bindings expr))
rewriteStatement n bindings (For mexpr1 mexpr2 mexpr3 stmt) =
    let (_,_,stmt') = rewriteStatement n bindings stmt
        rewriteMExpr = rewriteMExpression bindings 
        rewriteEs = rewriteCtxExprs bindings in
    (n, bindings, For (rewriteEs mexpr1) (rewriteMExpr mexpr2) (rewriteEs mexpr3) stmt')
rewriteStatement n bindings (If expr stmt1 stmt2) = 
    let (_,_,stmt1') = rewriteStatement n bindings stmt1
        (_,_,stmt2') = rewriteStatement n bindings stmt2 in
        (n, bindings, If (rewriteCtxExpr bindings expr) stmt1' stmt2')
rewriteStatement n bindings (Decl (Var name t) val) =
   let (n',bindings', newname) =
           if any (\(name',_) -> name == name') bindings then let newname = "local" ++ (show n) in (n + 1, (name,newname):bindings, newname)
           else (n,bindings,name)
   in (n',bindings',Decl (Var newname t) (rewriteMExpression bindings val))
rewriteStatement n bindings (Return Nothing) = (n, bindings, Return Nothing)
rewriteStatement n bindings (Return (Just expr)) = (n, bindings, Return $ Just $ rewriteCtxExpr bindings expr)
rewriteStatement n bindings (Do expr) = (n, bindings, Do $ rewriteCtxExpr bindings expr)
rewriteStatement n bindings s = (n, bindings, s)

--rewriteExpressions bindings es = map (rewriteExpression bindings) es

rewriteCtxExpr bindings (Ctx ctx expr) = Ctx ctx $ rewriteExpression bindings expr
rewriteCtxExprs bindings ctxExprs = map (rewriteCtxExpr bindings) ctxExprs

rewriteExpression _ (IntLit i) = IntLit i
rewriteExpression _ (FloatLit f) = FloatLit f
rewriteExpression _ (StringLit s) = StringLit s
rewriteExpression _ (KeyLit k) = KeyLit k
rewriteExpression bindings (ListExpr l) = ListExpr $ rewriteCtxExprs bindings l
rewriteExpression bindings (VecExpr e1 e2 e3) = VecExpr (rewriteCtxExpr bindings e1) (rewriteCtxExpr bindings e2) (rewriteCtxExpr bindings e3)
rewriteExpression bindings (RotExpr e1 e2 e3 e4) = RotExpr (rewriteCtxExpr bindings e1) (rewriteCtxExpr bindings e2) (rewriteCtxExpr bindings e3) (rewriteCtxExpr bindings e4)
rewriteExpression bindings (Add expr1 expr2) = Add (rewriteCtxExpr bindings expr1) (rewriteCtxExpr bindings expr2)
rewriteExpression bindings (Sub expr1 expr2) = Sub (rewriteCtxExpr bindings expr1) (rewriteCtxExpr bindings expr2)
rewriteExpression bindings (Mul expr1 expr2) = Mul (rewriteCtxExpr bindings expr1) (rewriteCtxExpr bindings expr2)
rewriteExpression bindings (Div expr1 expr2) = Div (rewriteCtxExpr bindings expr1) (rewriteCtxExpr bindings expr2)
rewriteExpression bindings (Mod expr1 expr2) = Mod (rewriteCtxExpr bindings expr1) (rewriteCtxExpr bindings expr2)
rewriteExpression bindings (BAnd expr1 expr2) = BAnd (rewriteCtxExpr bindings expr1) (rewriteCtxExpr bindings expr2)
rewriteExpression bindings (Xor expr1 expr2) = Xor (rewriteCtxExpr bindings expr1) (rewriteCtxExpr bindings expr2)
rewriteExpression bindings (BOr expr1 expr2) = BOr (rewriteCtxExpr bindings expr1) (rewriteCtxExpr bindings expr2)
rewriteExpression bindings (Lt expr1 expr2) = Lt (rewriteCtxExpr bindings expr1) (rewriteCtxExpr bindings expr2)
rewriteExpression bindings (Gt expr1 expr2) = Gt (rewriteCtxExpr bindings expr1) (rewriteCtxExpr bindings expr2)
rewriteExpression bindings (Le expr1 expr2) = Le (rewriteCtxExpr bindings expr1) (rewriteCtxExpr bindings expr2)
rewriteExpression bindings (Ge expr1 expr2) = Ge (rewriteCtxExpr bindings expr1) (rewriteCtxExpr bindings expr2)
rewriteExpression bindings (And expr1 expr2) = And (rewriteCtxExpr bindings expr1) (rewriteCtxExpr bindings expr2)
rewriteExpression bindings (Or expr1 expr2) = Or (rewriteCtxExpr bindings expr1) (rewriteCtxExpr bindings expr2)
rewriteExpression bindings (ShiftL expr1 expr2) = ShiftL (rewriteCtxExpr bindings expr1) (rewriteCtxExpr bindings expr2)
rewriteExpression bindings (ShiftR expr1 expr2) = ShiftR (rewriteCtxExpr bindings expr1) (rewriteCtxExpr bindings expr2)
rewriteExpression bindings (Equal expr1 expr2) = Equal (rewriteCtxExpr bindings expr1) (rewriteCtxExpr bindings expr2)
rewriteExpression bindings (NotEqual expr1 expr2) = NotEqual (rewriteCtxExpr bindings expr1) (rewriteCtxExpr bindings expr2)
rewriteExpression bindings (Inv expr) = Inv $ rewriteCtxExpr bindings expr
rewriteExpression bindings (Not expr) = Not $ rewriteCtxExpr bindings expr
rewriteExpression bindings (Neg expr) = Neg $ rewriteCtxExpr bindings expr
rewriteExpression bindings (Call name exprs) = Call (rewriteName bindings name) $ rewriteCtxExprs bindings exprs
rewriteExpression bindings (Cast t expr) = Cast t $ rewriteCtxExpr bindings expr
rewriteExpression bindings (Get (name,component)) = Get (rewriteName bindings name,component)
--rewriteExpression bindings c@(Const _) = c
rewriteExpression bindings (Set (name,component) expr) =  Set (rewriteName bindings name,component) (rewriteCtxExpr bindings expr)
rewriteExpression bindings (IncBy (name,component) expr) = IncBy (rewriteName bindings name,component) (rewriteCtxExpr bindings expr)
rewriteExpression bindings (DecBy (name,component) expr) = DecBy (rewriteName bindings name,component) (rewriteCtxExpr bindings expr)
rewriteExpression bindings (MulBy (name,component) expr) = MulBy (rewriteName bindings name,component) (rewriteCtxExpr bindings expr)
rewriteExpression bindings (DivBy (name,component) expr) = DivBy (rewriteName bindings name,component) (rewriteCtxExpr bindings expr)
rewriteExpression bindings (ModBy (name,component) expr) = ModBy (rewriteName bindings name,component) (rewriteCtxExpr bindings expr)
rewriteExpression bindings e@(PostInc _) = e
rewriteExpression bindings e@(PostDec _) = e
rewriteExpression bindings e@(PreInc _) = e
rewriteExpression bindings e@(PreDec _) = e
rewriteMExpression bindings = fmap (rewriteCtxExpr bindings)

data SourceContext = TextLocation { textLine0 :: Int, textColumn0 :: Int, textLine1 :: Int, textColumn1 :: Int, textName :: String } |
                     UnknownSourceContext
                     deriving (Show)

isTextLocation (TextLocation _ _ _ _ _) = True
isTextLocation _ = False

data Ctx a = Ctx { srcCtx :: SourceContext, ctxItem :: a } deriving Show
instance Functor Ctx where
    fmap f (Ctx c v) = (Ctx c $ f v)

ctxItems = map ctxItem

fromMCtx Nothing = Nothing
fromMCtx (Just m) = Just $ ctxItem m

nullCtx = Ctx UnknownSourceContext

funcNames = map (ctxItem.funcName)

ctxVr2Vr (Ctx _ name,c) = (name,c)
