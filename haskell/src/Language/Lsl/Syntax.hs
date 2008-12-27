{-# OPTIONS_GHC -XDeriveDataTypeable -XTypeSynonymInstances -XFlexibleContexts -XGeneralizedNewtypeDeriving 
                -XTemplateHaskell -XNoMonomorphismRestriction #-}
-- | Defines the abstract syntax tree for LSL (and LSL Plus extensions).
module Language.Lsl.Syntax (
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
    Validity,
    Global(..),
    SourceContext(..),
    Ctx(..),
    CompiledLSLScript(..),
    Library,
    AugmentedLibrary,
    CodeErr,
    CtxVar,
    CtxName,
    CtxExpr,
    CtxStmt,
    ModuleInfo,
    Codebase(..),
    -- Values
    fromMCtx,
    ctxItems,
    nullCtx,
    ctxVr2Vr,
    findFunc,
    validLSLScript,
    validLibrary,
    moduleFromScript,
    findState,
    predefFuncs,
    findFuncDec,
    goodHandlers,
    libFromAugLib,
    isTextLocation,
    compileLSLScript,
    compileLSLScript',
    compileLibrary,
    VState,
    emptyValidationState) where

import Language.Lsl.Internal.Type(Component(..),LSLType(..),lslTypeString)
import Language.Lsl.Internal.Constants(isConstant,findConstType)
import Language.Lsl.Internal.EventSigs(simpleLslEventDescriptors)
import Language.Lsl.Internal.FuncSigs(funcSigs)
import Language.Lsl.Internal.AccessGenerator(genAccessorsForType,genMAccessorsForType)
import Data.Generics
import Data.Data(Data,Typeable)
import Data.List(find,sort,sortBy,nub,foldl')
import qualified Data.Map as M
import Data.Maybe(isJust,isNothing)
import Language.Lsl.Internal.Util(ctx,findM,lookupM,filtMap,throwStrError)
import Control.Monad(when,foldM,MonadPlus(..))
import Control.Monad.Error(MonadError(..),Error(..))
import qualified Control.Monad.State as S(State)
import Control.Monad.State hiding(State)

import Debug.Trace
--trace1 s v = trace (s ++ show v) v

data SourceContext = TextLocation { textLine0 :: Int, textColumn0 :: Int, textLine1 :: Int, textColumn1 :: Int, textName :: String } |
                     UnknownSourceContext
                     deriving (Show,Typeable,Data)

isTextLocation (TextLocation _ _ _ _ _) = True
isTextLocation _ = False

-- | A wrapper that can associate a source code context with a value (e.g. a syntax value).
data Ctx a = Ctx { srcCtx :: SourceContext, ctxItem :: a } deriving (Show,Typeable,Data)
instance Functor Ctx where
    fmap f (Ctx c v) = (Ctx c $ f v)

ctxItems = map ctxItem

fromMCtx :: Maybe (Ctx a) -> Maybe a
fromMCtx = fmap ctxItem

nullCtx :: a -> Ctx a
nullCtx = Ctx UnknownSourceContext

funcNames = map (ctxItem.funcName)

ctxVr2Vr :: (Ctx a,b) -> (a,b)
ctxVr2Vr (Ctx _ name,c) = (name,c)

type CtxVar = Ctx Var

-- | An LSL variable (a name, and a type).
data Var = Var { varName :: String, varType :: LSLType } deriving (Show,Typeable,Data)

type CtxName = Ctx String

-- | An LSL function declaration (the function name and type information, without associated
-- statements.
data FuncDec = FuncDec { funcName :: CtxName, funcType :: LSLType, funcParms :: [CtxVar] }
    deriving (Show,Typeable,Data)

type CtxStmt = Ctx Statement
-- | An LSL function definition (return type, parameters and statements.
data Func = Func FuncDec [CtxStmt] deriving (Show,Typeable,Data)

-- | An LSL Plus module, which is a separately 'compiled' unit that can contain
-- both global variables and functions, but not states or handlers.
data LModule = LModule [GlobDef] [CtxVar]
    deriving (Show,Typeable,Data)

type CtxExpr = Ctx Expr
-- | An LSL expression.
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
          | AQString String
          | AQInteger String
          | AQKey String
          | AQFloat String
            deriving (Show,Typeable,Data)

-- | An LSL statement.
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
    deriving (Show,Typeable,Data)

isLabel (Label _) = True
isLabel _ = False

-- | An LSL global variable (this is actually not a source level/syntactic entity -- the set of globals
-- for a script is derived after analyzing all included modules.
data Global = GDecl Var (Maybe Expr)
    deriving (Show,Typeable,Data)

-- | A global definition (a function, a variable, or a module import statement).
data GlobDef = GV CtxVar (Maybe CtxExpr) | GF Func | GI CtxName [(String,String)] String
    deriving (Show,Typeable,Data)
-- | An LSL event handler definition.
data Handler = Handler CtxName [CtxVar] [CtxStmt]
    deriving (Show,Typeable,Data)
    
-- | The set of valid handlers supported by LSL.
goodHandlers :: [(String,[LSLType])]
goodHandlers = simpleLslEventDescriptors

-- | An LSL state definition.
data State = State CtxName [Handler]
    deriving (Show,Typeable,Data)

-- | An LSL script.
data LSLScript = LSLScript [GlobDef] [State] deriving (Show,Typeable,Data)

type ModuleInfo = ([Global],[Func])
-- | A collection of modules.
type Library = [(String,Validity LModule)]
-- | A collection of mouldes, augmented with additional derived information.
type AugmentedLibrary = [(String,Validity (LModule,ModuleInfo))]

data Codebase = Codebase { codebaseLib :: AugmentedLibrary, codebaseScripts :: [(String,Validity CompiledLSLScript)] }

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
        Nothing -> throwStrError "unknown module"
        Just (Left (_,s)) -> throwStrError ("invalid library (" ++ s ++ ")")
        Just (Right m) -> return m

-- A description of an error and where to find it in the source.
type CodeErr = (SourceContext,String)

ctxFromCodeErr = fst
msgFromCodeErr = snd

-- | An error monad for representing validation errors with respect to LSL code.
type Validity a = Either CodeErr a

instance Error CodeErr where
    noMsg = (UnknownSourceContext,"")
    strMsg s = (UnknownSourceContext,s)
    
incontext (ctx,s) (Left (ctx',s')) =
    case ctx' of
        UnknownSourceContext -> Left (ctx,msg)
        _ -> Left (ctx',msg) 
    where msg = if null s then s' else s ++ ": " ++ s'
incontext _ v = v

incontext' (ctx,s) (Left (_,s')) = Left (ctx,s ++ ": " ++ s')
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
    when (n `elem` used) $ throwError (ctx, n ++ " already defined")
    noDupVars (n:used) vs

checkName :: Maybe SourceContext -> String -> [String] -> Validity ()
checkName (Just ctx) name names = 
    when (name `elem` names) $ throwError (ctx, name ++ " is multiply defined")

data CompiledLSLScript = CompiledLSLScript {
    scriptGlobals :: ![Global],
    scriptFuncs :: ![Func],
    scriptStates :: ![State]}
    deriving (Show)

data RefPos = RefPos { refPosName :: !String, refPosLine :: !Int, refPosCol :: !Int }
    deriving (Show,Eq,Ord)
    
data ValidationState = ValidationState {
        vsLib :: !Library,
        vsGlobalRegistry :: !(M.Map String SourceContext),
        vsLocalRegistry :: ![M.Map String SourceContext],
        vsLocalVars :: ![[Var]],
        vsRefs :: !(M.Map RefPos SourceContext),
        vsLabels :: ![[String]],
        vsModules :: ![String],
        vsGlobDefs :: ![GlobDef],
        vsStates :: ![State],
        vsGlobals :: ![Global],
        vsFuncs :: ![Func],
        vsErr :: ![CodeErr],
        vsWarn :: ![CodeErr],
        vsNamesUsed :: [String],
        vsGVs :: ![Var],
        vsGFs :: ![FuncDec],
        vsStateNames :: ![String],
        vsEntryPointInfo :: !(LSLType,Bool),
        vsBranchReturns :: !Bool,
        vsHandlersUsed :: ![String],
        vsImports :: ![(String,[(String,String)],String)],
        vsContext :: [SourceContext]
    }
    
emptyValidationState = ValidationState {
    vsLib = [], 
    vsGlobalRegistry = M.empty,
    vsLocalRegistry = [], 
    vsLocalVars = [], 
    vsRefs = M.empty, 
    vsLabels = [],
    vsModules = [], 
    vsGlobDefs = [], 
    vsStates = [], 
    vsGlobals = [], 
    vsFuncs = [],
    vsErr = [], 
    vsWarn = [], 
    vsNamesUsed = [], 
    vsGVs = [], 
    vsGFs = [], 
    vsStateNames = [], 
    vsEntryPointInfo = (LLVoid,False), 
    vsBranchReturns = False, 
    vsHandlersUsed = [], 
    vsImports = [], 
    vsContext = [] 
    }
-- generate set'xxx methods for all the record selectors in the above type...
-- warning: all types mentioned in the above declaration must be defined (lexically) prior to
-- the following splice
-- double warning: the 'set' functions generated can only be used in code that
-- lexically follows this splice   
$(genAccessorsForType ''ValidationState) -- a Template Haskell splice...
-- this splice requires the above splice, and, because the author of the splice was too lazy to
-- generate type signatures, also requires -XNoMonomorphismRestriction
$(genMAccessorsForType ''ValidationState) -- a Template Haskell splice...

type VState a = S.State ValidationState a

--vsmRegisterGlobal :: Ctx Var -> VState ()
vsmRegisterGlobalName name ctx = get'vsGlobalRegistry >>= put'vsGlobalRegistry . (M.insert name ctx)
vsmRegisterGlobal (Ctx ctx (Var name t)) = vsmRegisterGlobalName name ctx
vsmRegisterFunc (Func (FuncDec (Ctx ctx name) _ _) _) = vsmRegisterGlobalName name ctx

vsmRegisterLocalName name ctx = do
    lregistry <- get'vsLocalRegistry
    case lregistry of
        [] -> error "no local scope on stack"
        (top:rest) -> put'vsLocalRegistry (M.insert name ctx top : rest)

vsmAddGlobalRef name ctx = do
    registry <- get'vsGlobalRegistry
    case M.lookup name registry of
        Nothing -> return ()
        Just UnknownSourceContext -> return ()
        Just ctx -> do
            refs <- get'vsRefs
            put'vsRefs (M.insert (RefPos name (textLine0 ctx) (textColumn0 ctx)) ctx refs)

vsmAddRef name ctx = do
      lr <- get'vsLocalRegistry
      go lr
   where go [] = vsmAddGlobalRef name ctx
         go (top:rest) = do
             case M.lookup name top of
                 Nothing -> go rest
                 Just UnknownSourceContext -> return ()
                 Just ctx -> do
                     refs <- get'vsRefs
                     put'vsRefs (M.insert (RefPos name (textLine0 ctx) (textColumn0 ctx)) ctx refs)
                   
vsmAddGV :: Var -> VState ()
vsmAddGV var = get'vsGVs >>= put'vsGVs . (var:)
vsmAddGF :: FuncDec -> VState ()
vsmAddGF fd = get'vsGFs >>= put'vsGFs . (fd:)

vsmAddFunc :: Func -> VState ()
vsmAddFunc func = get'vsFuncs >>= put'vsFuncs . (func :)

vsmAddGlobal :: Global -> VState ()
vsmAddGlobal global = get'vsGlobals >>= put'vsGlobals . (global :)

vsmAddLocal :: SourceContext -> Var -> VState ()
vsmAddLocal ctx v@(Var name _) = do
    locals <- get'vsLocalVars
    case locals of
        [] -> error "internal error - no local scope"
        (top:rest) -> 
            if (defined name $ concat locals) || (isConstant name)
                then vsmAddErr (ctx, name ++ " is already defined")
                else do
                    put'vsLocalVars ((v:top):rest)
                    vsmRegisterLocalName name ctx
            
vsmAddToNamesUsed :: String -> VState ()
vsmAddToNamesUsed name = get'vsNamesUsed >>= put'vsNamesUsed . (name :)

vsmWithNewScope :: VState a -> VState a
vsmWithNewScope action = do
    get'vsLocalRegistry >>= put'vsLocalRegistry . (M.empty:)
    get'vsLabels >>= put'vsLabels . ([]:)
    get'vsLocalVars >>= put'vsLocalVars . ([]:)
    val <- action
    get'vsLocalRegistry >>= (\ (_:reg) -> put'vsLocalRegistry reg)
    get'vsLabels >>= (\ (_:labels) -> put'vsLabels labels)
    get'vsLocalVars >>= (\ (_:vars) -> put'vsLocalVars vars)
    return val

vsmWithModule :: String -> VState () -> VState ()
vsmWithModule mname action = get'vsModules >>= put'vsModules . (mname:) >> action >> get'vsModules >>= (\ (_:ms) -> put'vsModules ms)

vsmAddErr :: CodeErr -> VState ()
vsmAddErr err = do
    ctx <- get'vsContext >>= return . safeHead
    errs <- get'vsErr
    put'vsErr ((maybe (ctxFromCodeErr err) id ctx, msgFromCodeErr err) : errs)

vsmAddLabel :: String -> VState ()
vsmAddLabel label = do
    labels <- get'vsLabels
    case labels of
        [] -> error "no local scope!"
        (top:rest) -> put'vsLabels ((label:top):rest)

vsmAddHandler handlerName = get'vsHandlersUsed >>= put'vsHandlersUsed . (handlerName:)
        
vsmWithContext :: SourceContext -> VState a -> VState a
vsmWithContext srcCtx action = do
    ctxs <- get'vsContext
    put'vsContext (srcCtx:ctxs)
    v <- action
    put'vsContext ctxs
    return v
    
vsmInEntryPoint :: LSLType -> Bool -> VState a -> VState a
vsmInEntryPoint t stateChangeAllow action = do
    put'vsEntryPointInfo (t,stateChangeAllow)
    action
    
vsmInBranch :: VState a -> VState a
vsmInBranch action = do
    info <- get'vsBranchReturns
    put'vsBranchReturns False
    val <- action
    put'vsBranchReturns info
    return val
    
vsmWithinState action = do
    put'vsHandlersUsed []
    action

whenM mp action = do
    p <- mp
    when p action
    
whenJust mv action =
    case mv of
        Nothing -> return ()
        Just v -> action v

whenIsJust mv p action =
    case mv of
        Nothing -> return ()
        Just v | p v -> action
               | otherwise -> return ()
               
safeHead [] = Nothing
safeHead (x:_) = Just x
    
vsmFirstErr :: VState (Maybe CodeErr)
vsmFirstErr = get'vsErr >>= \ l -> case l of
     l@(_:_) -> return $ Just $ last l -- list is reversed...
     _       -> return Nothing

compileLSLScript' :: Library ->LSLScript -> Validity CompiledLSLScript
compileLSLScript' library script = evalState (compileLSLScript script) (emptyValidationState { vsLib = library })

compileLSLScript :: LSLScript -> VState (Validity CompiledLSLScript)
compileLSLScript (LSLScript globs states) = do
    preprocessGlobDefs_ "" globs
    preprocessStates states
    mapM_ vsmAddGF predefFuncs
    mapM_ compileGlob globs
    mapM_ compileState states
    err <- vsmFirstErr
    case err of
        Nothing -> do
           globals <- get'vsGlobals
           funcs <- get'vsFuncs
           states <- get'vsStates
           return $ Right $ CompiledLSLScript globals funcs states
        Just err -> return $ Left err

preprocessStates states = let snames = map (\ (State cn _) -> ctxItem cn) states in put'vsStateNames snames

preprocessGlobDefs :: String -> [GlobDef] -> VState ([Var],[FuncDec])
preprocessGlobDefs prefix globs = do
    preprocessGlobDefs_ prefix globs
    vars <- get'vsGVs
    funcDecs <- get'vsGFs
    return (vars,funcDecs)
    
preprocessGlobDefs_ :: String -> [GlobDef] -> VState ()
preprocessGlobDefs_ = mapM_ . preprocessGlobDef
    
preprocessGlobDef :: String -> GlobDef -> VState ()
preprocessGlobDef prefix (GV (Ctx ctx v@(Var name t)) _) = vsmAddGV (Var (prefix ++ name) t)
preprocessGlobDef prefix (GF (Func (FuncDec name t params) _)) = vsmAddGF (FuncDec (fmap (prefix++) name) t params)
preprocessGlobDef prefix (GI moduleName _ prefix') = do
    lib <- get'vsLib
    case lookupModule (ctxItem moduleName) lib of
        Left err -> vsmAddErr err
        Right (LModule globs _) -> mapM_ (preprocessGlobDef (prefix++prefix')) globs
        
compileGlob :: GlobDef -> VState ()
compileGlob (GV v mexpr) = do
    let v' = ctxItem v
    when (isConstant $ varName v') (vsmAddErr (srcCtx v, varName v' ++ " is a predefined constant"))
    namesUsed <- get'vsNamesUsed
    gvs <- get'vsGVs
    when (varName v' `elem` namesUsed) (vsmAddErr (srcCtx v, varName v' ++ " is already defined"))
    whenJust mexpr $ \ expr -> do
       let (gvs',_) = break (\ var -> varName var == varName v') gvs
       mt <- compileCtxSimple gvs' expr
       whenJust mt $ \ t -> when (not (varType v' `matchTypes` t)) (vsmAddErr (srcCtx v, "expression not of the correct type"))
    vsmRegisterGlobal v
    vsmAddToNamesUsed (varName v')
    vsmAddGlobal (GDecl v' (fmap ctxItem mexpr))
compileGlob (GF f@(Func (FuncDec name t params) statements)) =
    vsmWithNewScope $ do
        compileParams params
        vsmInEntryPoint t False $ do
            whenM ((return elem) `ap` (return $ ctxItem name) `ap` get'vsNamesUsed) (vsmAddErr (srcCtx name, ctxItem name ++ " is already defined"))
            returns <- compileStatements statements
            when (not returns && t /= LLVoid) (vsmAddErr (srcCtx name, ctxItem name ++ ": not all code paths return a value"))
            vsmRegisterFunc f
            vsmAddToNamesUsed $ ctxItem name
            vsmAddFunc f
compileGlob (GI (Ctx ctx name) bindings prefix) =
    vsmWithModule name $ do
        let imp = (name, sort bindings, prefix)
        imports <- get'vsImports
        when (not (imp `elem` imports)) $ do
            library <- get'vsLib
            case lookupModule name library of
                Left err -> vsmAddErr (ctx, "module " ++ name ++ ": " ++ err)
                Right (LModule globs freevars) -> do
                   vars <- get'vsGVs
                   case validBindings vars freevars bindings of
                       Left err -> vsmAddErr (ctx, err)
                       Right () -> do
                           let (vars',funcDecs') = evalState (preprocessGlobDefs "" globs) (emptyValidationState { vsLib = library })
                           let renames = bindings ++ (map (\ x -> (x,prefix ++ x)) ((map varName vars') ++ (funcNames funcDecs')))
                           vsmWithContext ctx $ mapM_ (rewriteGlob' prefix renames (map ctxItem freevars ++ vars')) globs

rewriteGlob' prefix renames vars (GF (Func (FuncDec name t params) statements)) =
    case lookup (ctxItem name) renames of
        Nothing -> vsmAddErr (srcCtx name, "can't rename " ++ ctxItem name ++ ": not found")
        Just name' -> do
            namesUsed <- get'vsNamesUsed
            if name' `elem` namesUsed 
                then  vsmAddErr (srcCtx name, name' ++ " imported from module is already defined")
                else let rewrittenFunc = (Func (FuncDec (Ctx (srcCtx name) name') t params) $ rewriteStatements 0 renames statements)
                     in do  vsmAddToNamesUsed name'
                            vsmRegisterFunc rewrittenFunc
                            vsmAddFunc rewrittenFunc
rewriteGlob' prefix renames vars (GV (Ctx ctx (Var name t)) mexpr) =
    case lookup name renames of
        Nothing -> vsmAddErr (ctx, "can't rename " ++ name ++ ": not found")
        Just name' -> do
            namesUsed <- get'vsNamesUsed
            if name' `elem` namesUsed
                then vsmAddErr (ctx, name' ++ " imported from module is already defined")
                else let rewrittenGlobVar = GDecl (Var name' t) (fmap (ctxItem . (rewriteCtxExpr renames)) mexpr)
                     in do vsmAddToNamesUsed name'
                           vsmRegisterGlobal (Ctx ctx (Var name' t))
                           vsmAddGlobal rewrittenGlobVar
rewriteGlob' prefix0 renames vars (GI (Ctx ctx mName) bindings prefix) = 
    do lib <- get'vsLib
       case lookupModule mName lib of
           Left err -> vsmAddErr (ctx, err)
           Right (LModule globs freevars) -> do
               case validBindings vars freevars bindings of
                  Left err -> vsmAddErr (ctx, err)
                  Right _ -> do
                      bindings' <- mapM rewriteBinding bindings
                      let imp = (mName,sort bindings', prefix0 ++ prefix)
                      imports <- get'vsImports
                      when (not (imp `elem` imports)) $ do
                          let (vars',funcDecs') = evalState (preprocessGlobDefs "" globs) (emptyValidationState { vsLib = lib })
                          let renames = bindings' ++ map (\ x -> (x,prefix0 ++ prefix ++ x)) (map varName vars' ++ map (ctxItem . funcName) funcDecs')
                          mapM_ (rewriteGlob' (prefix0 ++ prefix) renames vars') globs
    where rewriteBinding (fv,rn) = case lookup rn renames of
                                       Nothing -> vsmAddErr (ctx, rn ++ ": not found") >> return (fv,rn)
                                       Just rn' -> return (fv,rn')

compileState :: State -> VState ()
compileState (State nm handlers) = 
    vsmWithinState $ do
        states <- get'vsStates
        when (isJust (find (\ (State x _)-> ctxItem x == ctxItem nm) states)) $
            vsmAddErr (srcCtx nm, ctxItem nm ++ " already defined") 
        mapM_ compileHandler handlers

compileHandler  (Handler (Ctx ctx name) args stmts) =
    vsmWithNewScope $ do
        used <- get'vsHandlersUsed
        if name `elem` used then vsmAddErr (ctx, name ++ " already used in this state")
                            else vsmAddHandler name
        case lookup name goodHandlers of
            Nothing -> vsmAddErr (ctx, name ++ " is not a valid handler name")
            Just types -> when (types /= map varType (ctxItems args)) $ vsmAddErr (ctx, "invalid parameter types for handler " ++ name)
        compileParams args
        vsmInEntryPoint LLVoid True $ compileStatements stmts

compileCtxSimple :: [Var] -> CtxExpr -> VState (Maybe LSLType)
compileCtxSimple gvs (Ctx ctx expr) = do
    t <- compileSimple gvs expr
    case t of
        Nothing -> vsmAddErr (ctx,"expression is not valid as an initializer for a global variable") >> return t
        _ -> return t

compileSimple :: [Var] -> Expr -> VState (Maybe LSLType)
compileSimple _ (IntLit i) = return $ Just LLInteger
compileSimple _ (FloatLit i) = return $ Just LLFloat
compileSimple _ (StringLit s) = return $ Just LLString
compileSimple _ (KeyLit k) = return $ Just LLKey
compileSimple gvs (Get (Ctx ctx name, All)) = do
    vsmAddGlobalRef name ctx
    let var = find (\ v -> varName v == name) gvs
    case var of
        Just (Var _ t) -> return $ Just t
        Nothing -> case findConstType name of
            Nothing -> vsmAddErr (ctx,"variable " ++ name ++ " not found") >> return Nothing
            Just t -> return $ Just t
compileSimple _ (Get (Ctx ctx name,_)) = do
    vsmAddGlobalRef name ctx
    vsmAddErr (ctx, "can't access vector/rotation component in global variable initialization") >> return Nothing
compileSimple _ (ListExpr []) = return $ Just LLList
compileSimple vars (ListExpr (e:es)) = do
    mt <- compileCtxSimple vars e
    case mt of
        Nothing -> return Nothing
        Just t -> do
            when (t == LLList) (vsmAddErr (srcCtx e, "lists cannot contain other lists"))
            compileSimple vars (ListExpr es)
compileSimple vars (VecExpr e1 e2 e3) = compileSimpleStructure vars LLVector [e1,e2,e3]
compileSimple vars (RotExpr e1 e2 e3 e4) = compileSimpleStructure vars LLRot [e1,e2,e3,e4]
compileSimple vars (Neg e) = do
    mt <- compileCtxSimple vars e
    case mt of
        Nothing -> return Nothing
        Just t -> 
            if (t `notElem` [LLFloat,LLInteger]) 
                then vsmAddErr (srcCtx e, "operator only applicable to integers and floats in this context") >> return Nothing
                else return (Just t)
compileSimple _ e = return Nothing

compileSimpleStructure :: [Var] -> LSLType -> [CtxExpr] -> VState (Maybe LSLType)
compileSimpleStructure _ t [] = return (Just t)
compileSimpleStructure vars t (e:es) = do
    mt <- compileCtxSimple vars e
    case mt of
        Nothing -> return Nothing
        Just t' ->
            if (t' `notElem` [LLFloat,LLInteger])
                then vsmAddErr (srcCtx e, "literal of type " ++ lslTypeString t' ++  " is not a valid element of " ++ lslTypeString t) 
                     >> return Nothing
                else compileSimpleStructure vars t es

compileStatement :: Ctx Statement -> VState Bool
compileStatement  (Ctx ctx (Decl var@(Var name t) expr)) = do
    vsmAddLocal ctx var
    case expr of
       Nothing -> return ()
       Just expr' -> do mt' <- compileCtxExpr expr'
                        case mt' of
                            Nothing -> return ()
                            Just t' -> when (not $ matchTypes t t') $ vsmAddErr (srcCtx expr', "type of expression in declaration of " ++ name ++ " does not match " ++ lslTypeString t)
    get'vsBranchReturns
compileStatement  (Ctx ctx (While expr statement)) = do
    t <- compileCtxExpr expr
    vsmInBranch $ compileStatement (nullCtx statement)
    get'vsBranchReturns
compileStatement (Ctx ctx(DoWhile statement expr)) =
    do t <- compileCtxExpr expr
       vsmInBranch $ compileStatement (nullCtx statement)
       get'vsBranchReturns
compileStatement (Ctx ctx (For mexpr1 mexpr2 mexpr3 statement)) =
    do  compileExpressions mexpr1
        compileExpressions mexpr3
        t <- compileMExpression mexpr2
        vsmInBranch $ compileStatement (nullCtx statement)
        get'vsBranchReturns
compileStatement (Ctx ctx (If expr thenStmt elseStmt)) =
    do t <- compileCtxExpr expr
       ret1 <- vsmInBranch $ compileStatement (nullCtx thenStmt)
       ret2 <- vsmInBranch $ compileStatement (nullCtx elseStmt)
       returns <- get'vsBranchReturns
       put'vsBranchReturns (returns || (ret1 && ret2))
       get'vsBranchReturns
compileStatement (Ctx ctx NullStmt) = get'vsBranchReturns
compileStatement (Ctx ctx (Return Nothing)) = 
    do  (rtype,_) <- get'vsEntryPointInfo
        when (rtype /= LLVoid) (vsmAddErr (ctx,"function must return a value"))
        put'vsBranchReturns True
        return True
compileStatement (Ctx ctx (Return (Just expr))) = 
    do  t <- compileCtxExpr expr
        case t of
            Nothing -> return ()
            Just t' -> do
                (rtype,_) <- get'vsEntryPointInfo
                when (t' /= rtype && not (all (`elem` [LLString,LLKey]) [t',rtype])) (vsmAddErr (ctx,"inappropriate return type for function/handler"))
        put'vsBranchReturns True
        return True
compileStatement (Ctx ctx (StateChange name)) = do
    (_,scallow) <- get'vsEntryPointInfo
    snames <- get'vsStateNames
    when (not scallow) $ vsmAddErr (ctx,"state changes not allowed from this context")
    when (not (name `elem` snames)) $ vsmAddErr (ctx,name ++ " is not a valid state")
    get'vsBranchReturns
compileStatement (Ctx ctx (Do expr)) = compileCtxExpr expr >> get'vsBranchReturns
compileStatement (Ctx ctx (Compound stmts)) = compileStatements stmts
compileStatement (Ctx ctx (Label _)) = put'vsBranchReturns False >> return False
compileStatement (Ctx ctx (Jump s)) = 
    do labels <- get'vsLabels
       when (s `notElem` concat labels) $ vsmAddErr (ctx, "no such label to jump to: " ++ s)
       get'vsBranchReturns


compileStatements :: [CtxStmt] -> VState Bool
compileStatements stmts =
    do let newLabels = map (\ (Label s) -> s) $ filter isLabel (ctxItems stmts)
       vsmWithNewScope $ do
           mapM_ vsmAddLabel newLabels
           mapM_ compileStatement stmts
           get'vsBranchReturns

compileParams :: [CtxVar] -> VState ()
compileParams vs = mapM_ ( \(Ctx ctx v) -> vsmAddLocal ctx v) vs

isCastValid t t' = (t' == t) || (t,t') `elem` validCasts
   where validCasts = [(LLInteger,LLFloat), (LLFloat,LLInteger),
                      (LLInteger,LLString),(LLString,LLInteger),
                      (LLFloat,LLString),(LLString,LLFloat),
                      (LLString,LLVector),(LLVector,LLString),
                      (LLString,LLKey),(LLKey,LLString),
                      (LLRot,LLString),(LLString,LLRot),
                      (LLList,LLString),(LLString,LLList)]

compileCtxExpr :: Ctx Expr -> VState (Maybe LSLType)
compileCtxExpr (Ctx ctx (Cast t expr)) = 
   do mt <- compileCtxExpr expr
      case mt of
          Nothing -> return ()
          Just t' -> when (not $ isCastValid t' t) $ vsmAddErr (ctx,"cannot cast a value of type " ++ lslTypeString t' ++ " to type " ++ lslTypeString t)
      return (Just t)
compileCtxExpr (Ctx ctx0 (Get ((Ctx ctx name),component))) = do
   vsmAddRef name ctx
   vars <- get'vsGVs
   locals <- get'vsLocalVars
   let varList = (concat locals ++ vars)
   case (findType name varList `mplus` findConstType name,component) of
       (Nothing,_) -> vsmAddErr (ctx, "undefined variable or constant: " ++ name) >> return Nothing
       (Just LLRot,All) -> return $ Just LLRot
       (Just LLRot,_) -> return $ Just LLFloat
       (Just LLVector,All) -> return $ Just LLVector
       (Just LLVector,S) -> vsmAddErr (ctx0,"s is not a valid component of a vector") >> (return $ Just LLFloat)
       (Just LLVector,_) -> return $ Just LLFloat
       (Just t,All) -> return $ Just t
       (Just t,_) -> vsmAddErr (ctx,"only vectors and rotations have components") >> return Nothing
compileCtxExpr (Ctx ctx (Call name exprs)) = compileCall name exprs
compileCtxExpr (Ctx ctx (Not expr)) =
    do mt <- compileCtxExpr expr
       (mt `whenIsJust` (/=LLInteger)) $ vsmAddErr (ctx, "operator not applicable to non-integer type")
       return $ Just LLInteger
compileCtxExpr (Ctx ctx (Neg expr))  =
    do mt <- compileCtxExpr expr
       (mt `whenIsJust` (==LLList)) $ vsmAddErr (ctx, "operator not applicable to list type")
       return mt
compileCtxExpr (Ctx ctx (Inv expr)) =
    do mt <- compileCtxExpr expr
       (mt `whenIsJust` (/=LLInteger)) $ vsmAddErr (srcCtx expr, "operator not applicable to non-integer type")
       return mt
compileCtxExpr (Ctx ctx plus@(Add expr1 expr2)) =
    do  (mt1,mt2) <- compileEach (expr1,expr2)
        case (mt1,mt2) of
            (Nothing,Just t2) -> return (Just t2)
            (Just t1,Nothing) -> return (Just t1)
            (Nothing,Nothing) -> return Nothing
            (Just LLInteger,Just LLInteger) -> return $ Just LLInteger
            (Just LLInteger,Just LLFloat) -> return $ Just LLFloat
            (Just LLFloat,Just LLInteger) -> return $ Just LLFloat
            (Just LLFloat,Just LLFloat) -> return $ Just LLFloat
            (Just LLVector,Just LLVector) -> return $ Just LLVector
            (Just LLRot,Just LLRot) -> return $ Just LLRot
            (Just LLString,Just LLString) -> return $ Just LLString
            (Just LLList,Just LLList) -> return $ Just LLList
            (Just t,Just LLList) -> return $ Just LLList
            (Just LLList,Just t) -> return $ Just LLList
            (Just t0,Just t1) -> reportIncompatibleOperands ctx t0 t1 >> return Nothing
compileCtxExpr (Ctx ctx minus@(Sub expr1 expr2)) =
    do  (mt1,mt2) <- compileEach (expr1,expr2)
        case (mt1,mt2) of
            (Nothing,Just t2) -> return (Just t2)
            (Just t1,Nothing) -> return (Just t1)
            (Nothing,Nothing) -> return Nothing
            (Just LLInteger,Just LLInteger) -> return $ Just LLInteger
            (Just LLInteger,Just LLFloat) -> return $ Just LLFloat
            (Just LLFloat,Just LLInteger) -> return $ Just LLFloat
            (Just LLFloat,Just LLFloat) -> return $ Just LLFloat
            (Just LLVector,Just LLVector) -> return $ Just LLVector
            (Just LLRot,Just LLRot) -> return $ Just LLRot
            (Just t0,Just t1) -> reportIncompatibleOperands ctx t0 t1 >> return Nothing
compileCtxExpr (Ctx ctx expr@(Mul expr1 expr2)) =
    do  (mt1,mt2) <- compileEach (expr1,expr2)
        case (mt1,mt2) of
            (Nothing,Just t2) -> return (Just t2)
            (Just t1,Nothing) -> return (Just t1)
            (Nothing,Nothing) -> return Nothing
            (Just LLInteger,Just LLInteger) -> return $ Just LLInteger
            (Just LLInteger,Just LLFloat) -> return $ Just LLFloat
            (Just LLFloat,Just LLInteger) -> return $ Just LLFloat
            (Just LLFloat,Just LLFloat) -> return $ Just LLFloat
            (Just LLVector,Just LLInteger) -> return $ Just LLVector
            (Just LLVector,Just LLFloat) -> return $ Just LLVector
            (Just LLFloat,Just LLVector) -> return $ Just LLVector
            (Just LLInteger,Just LLVector) -> return $ Just LLVector
            (Just LLVector,Just LLVector) -> return $ Just LLFloat
            (Just LLVector,Just LLRot) -> return $ Just LLVector
            (Just LLRot,Just LLRot) -> return $ Just LLRot
            (Just t0,Just t1) -> reportIncompatibleOperands ctx t0 t1 >> return Nothing
compileCtxExpr (Ctx ctx expr@(Div expr1 expr2)) =
    do  (mt1,mt2) <- compileEach (expr1,expr2)
        case (mt1,mt2) of
            (Nothing,Just t2) -> return (Just t2)
            (Just t1,Nothing) -> return (Just t1)
            (Nothing,Nothing) -> return Nothing
            (Just LLInteger,Just LLInteger) -> return $ Just LLInteger
            (Just LLInteger,Just LLFloat) -> return $ Just LLFloat
            (Just LLFloat,Just LLInteger) -> return $ Just LLFloat
            (Just LLFloat,Just LLFloat) -> return $ Just LLFloat
            (Just LLVector,Just LLInteger) -> return $ Just LLVector
            (Just LLVector,Just LLFloat) -> return $ Just LLVector
            (Just LLVector,Just LLRot) -> return $ Just LLVector
            (Just LLRot,Just LLRot) -> return $ Just LLRot
            (Just t0,Just t1) -> reportIncompatibleOperands ctx t0 t1 >> return Nothing
compileCtxExpr (Ctx ctx expr@(Mod expr1 expr2)) =
    do (mt1,mt2) <- compileEach (expr1,expr2)
       case (mt1,mt2) of
           (Nothing,Just t2) -> return (Just t2)
           (Just t1,Nothing) -> return (Just t1)
           (Nothing,Nothing) -> return Nothing
           (Just LLInteger,Just LLInteger) -> return $ Just LLInteger
           (Just LLVector,Just LLVector) -> return $ Just LLVector
           (Just t1,Just t2) -> reportIncompatibleOperands ctx t1 t2 >> return Nothing
compileCtxExpr (Ctx ctx e@(Equal expr1 expr2)) =
    do (mt1,mt2) <- compileEach (expr1,expr2)
       case (mt1,mt2) of
           (Nothing,Just t2) -> return (Just t2)
           (Just t1,Nothing) -> return (Just t1)
           (Nothing,Nothing) -> return Nothing
           (Just LLInteger,Just LLFloat) -> return $ Just LLInteger
           (Just LLFloat,Just LLInteger) -> return $ Just LLInteger
           (Just LLString,Just LLKey) -> return $ Just LLInteger
           (Just LLKey,Just LLString) -> return $ Just LLInteger
           (Just t1,Just t2) | (t1 == t2) -> return $ Just LLInteger
                             | otherwise  -> reportIncompatibleOperands ctx t1 t2 >> return Nothing
compileCtxExpr (Ctx ctx e@(NotEqual expr1 expr2)) =
    do (mt1,mt2) <- compileEach (expr1,expr2)
       case (mt1,mt2) of
           (Nothing,Just t2) -> return (Just t2)
           (Just t1,Nothing) -> return (Just t1)
           (Nothing,Nothing) -> return Nothing
           (Just LLInteger,Just LLFloat) -> return $ Just LLInteger
           (Just LLFloat,Just LLInteger) -> return $ Just LLInteger
           (Just LLString,Just LLKey) -> return $ Just LLInteger
           (Just LLKey,Just LLString) -> return $ Just LLInteger
           (Just t1,Just t2) | (t1 == t2) -> return $ Just LLInteger
                             | otherwise  -> reportIncompatibleOperands ctx t1 t2 >> return Nothing
compileCtxExpr (Ctx ctx e@(BAnd expr1 expr2)) = compileBothInteger ctx (expr1,expr2)
compileCtxExpr (Ctx ctx e@(BOr expr1 expr2)) = compileBothInteger ctx (expr1,expr2)
compileCtxExpr (Ctx ctx e@(Xor expr1 expr2)) = compileBothInteger ctx (expr1,expr2)
compileCtxExpr (Ctx ctx e@(ShiftL expr1 expr2)) = compileBothInteger ctx (expr1,expr2)
compileCtxExpr (Ctx ctx e@(ShiftR expr1 expr2)) = compileBothInteger ctx (expr1,expr2)
compileCtxExpr (Ctx ctx e@(Gt expr1 expr2)) = compileRelExpr ctx (expr1,expr2)
compileCtxExpr (Ctx ctx e@(Ge expr1 expr2)) = compileRelExpr ctx (expr1,expr2)
compileCtxExpr (Ctx ctx e@(Le expr1 expr2)) = compileRelExpr ctx (expr1,expr2)
compileCtxExpr (Ctx ctx e@(Lt expr1 expr2)) = compileRelExpr ctx (expr1, expr2)
compileCtxExpr (Ctx ctx e@(And expr1 expr2)) = compileBothInteger ctx (expr1, expr2)
compileCtxExpr (Ctx ctx e@(Or expr1 expr2)) = compileBothInteger ctx (expr1, expr2)
compileCtxExpr (Ctx ctx e@(IncBy (name,All) expr)) =
    do  reportErrorIfNoModify name
        (mt1,mt2) <- compileNameExpr (name,expr)
        case (mt1,mt2) of
            (Nothing,Nothing) -> return Nothing
            (Just t1,Nothing) -> return $ Just t1
            (Nothing,Just t2) -> return $ Just t2
            (Just LLInteger,Just LLInteger) -> return $ Just  LLInteger
            (Just LLFloat,Just LLInteger) -> return $ Just  LLFloat
            (Just LLFloat,Just LLFloat) -> return $ Just  LLFloat
            (Just LLVector,Just LLVector) -> return $ Just  LLVector
            (Just LLRot,Just LLRot) -> return $ Just  LLRot
            (Just LLString,Just LLString) -> return $ Just  LLString
            (Just LLList,Just LLList) -> return $ Just  LLList
            (Just LLList,Just t) -> return $ Just  LLList
            (Just t1,Just t2) -> reportIncompatibleOperands ctx t1 t2 >> return Nothing
compileCtxExpr (Ctx ctx e@(IncBy (name,_) expr) ) =
    do  reportErrorIfNoModify name
        (mt1,mt2) <- compileNameExpr (name,expr)
        case (mt1,mt2) of
            (Nothing,Nothing) -> return Nothing
            (Just t1,Nothing) -> return $ Just t1
            (Nothing,Just t2) -> return $ Just t2
            (Just t1,Just t2) | t1 `elem` [LLVector,LLRot] && t2 `elem` [LLFloat,LLInteger] -> return $ Just  LLFloat
                          | otherwise -> reportIncompatibleOperands ctx t1 t2 >> return Nothing
compileCtxExpr (Ctx ctx e@(DecBy (name,All) expr)) =
    do  reportErrorIfNoModify name
        (mt1,mt2) <- compileNameExpr (name,expr)
        case (mt1,mt2) of
            (Nothing,Nothing) -> return Nothing
            (Just t1,Nothing) -> return $ Just t1
            (Nothing,Just t2) -> return $ Just t2
            (Just LLInteger,Just LLInteger) -> return $ Just  LLInteger
            (Just LLFloat,Just LLInteger) -> return $ Just  LLFloat
            (Just LLFloat,Just LLFloat) -> return $ Just  LLFloat
            (Just LLVector,Just LLVector) -> return $ Just  LLVector
            (Just LLRot,Just LLRot) -> return $ Just  LLRot
            (Just t1,Just t2) -> reportIncompatibleOperands ctx t1 t2 >> return Nothing
compileCtxExpr (Ctx ctx e@(DecBy (name,_) expr)) =
    do  reportErrorIfNoModify name
        (mt1,mt2) <- compileNameExpr (name,expr)
        case (mt1,mt2) of
            (Nothing,Nothing) -> return Nothing
            (Just t1,Nothing) -> return $ Just t1
            (Nothing,Just t2) -> return $ Just t2
            (Just t1,Just t2) | t1 `elem` [LLVector,LLRot] && t2 `elem` [LLFloat,LLInteger] -> return $ Just  LLFloat
                              | otherwise -> reportIncompatibleOperands ctx t1 t2 >> return Nothing
compileCtxExpr (Ctx ctx e@(MulBy (name,All) expr)) =
    do  reportErrorIfNoModify name
        (mt1,mt2) <- compileNameExpr (name,expr)
        case (mt1,mt2) of
            (Nothing,Nothing) -> return Nothing
            (Just t1,Nothing) -> return $ Just t1
            (Nothing,Just t2) -> return $ Just t2
            (Just LLInteger,Just LLInteger) -> return $ Just  LLInteger
            (Just LLFloat,Just LLInteger) -> return $ Just  LLFloat
            (Just LLFloat,Just LLFloat) -> return $ Just  LLFloat
            (Just LLVector,Just LLInteger) -> return $ Just  LLVector
            (Just LLVector,Just LLFloat) -> return $ Just  LLVector
            (Just LLVector,Just LLVector) -> return $ Just  LLVector -- note: LSL compiles this, but it results in runtime error!
            (Just LLVector,Just LLRot) -> return $ Just  LLVector
            (Just LLRot,Just LLRot) -> return $ Just  LLRot
            (Just t1,Just t2) -> reportIncompatibleOperands ctx t1 t2 >> return Nothing
compileCtxExpr (Ctx ctx e@(MulBy (name,_) expr)) =
    do  reportErrorIfNoModify name
        (mt1,mt2) <- compileNameExpr (name,expr)
        case (mt1,mt2) of
            (Nothing,Nothing) -> return Nothing
            (Just t1,Nothing) -> return $ Just t1
            (Nothing,Just t2) -> return $ Just t2
            (Just t1,Just t2) | t1 `elem` [LLVector,LLRot] && t2 `elem` [LLFloat,LLInteger] -> return $ Just  LLFloat
                              | otherwise -> reportIncompatibleOperands ctx t1 t2 >> return Nothing
compileCtxExpr (Ctx ctx e@(DivBy (name,All) expr)) =
    do  reportErrorIfNoModify name
        (mt1,mt2) <- compileNameExpr (name,expr)
        case (mt1,mt2) of
            (Nothing,Nothing) -> return Nothing
            (Just t1,Nothing) -> return $ Just t1
            (Nothing,Just t2) -> return $ Just t2
            (Just LLInteger,Just LLInteger) -> return $ Just  LLInteger
            (Just LLFloat,Just LLInteger) -> return $ Just  LLFloat
            (Just LLFloat,Just LLFloat) -> return $ Just  LLFloat
            (Just LLVector,Just LLInteger) -> return $ Just  LLVector
            (Just LLVector,Just LLFloat) -> return $ Just  LLVector
            (Just LLVector,Just LLRot) -> return $ Just  LLVector
            (Just LLRot,Just LLRot) -> return $ Just  LLRot
            (Just t1,Just t2) -> reportIncompatibleOperands ctx t1 t2 >> return Nothing
compileCtxExpr (Ctx ctx e@(DivBy (name,_) expr)) =
    do  reportErrorIfNoModify name
        (mt1,mt2) <- compileNameExpr (name,expr)
        case (mt1,mt2) of
            (Nothing,Nothing) -> return Nothing
            (Just t1,Nothing) -> return $ Just t1
            (Nothing,Just t2) -> return $ Just t2
            (Just t1,Just t2) | t1 `elem` [LLVector,LLRot] && t2 `elem` [LLFloat,LLInteger] -> return $ Just  LLFloat
                              | otherwise -> reportIncompatibleOperands ctx t1 t2 >> return Nothing
compileCtxExpr (Ctx ctx e@(ModBy (name,All) expr)) =
    do  reportErrorIfNoModify name
        (mt1,mt2) <- compileNameExpr (name,expr)
        case (mt1,mt2) of
            (Nothing,Nothing) -> return Nothing
            (Just t1,Nothing) -> return $ Just t1
            (Nothing,Just t2) -> return $ Just t2
            (Just LLInteger,Just LLInteger) -> return $ Just  LLInteger
            (Just LLVector,Just LLVector) -> return $ Just  LLVector
            (Just t1,Just t2) -> reportIncompatibleOperands ctx t1 t2 >> return Nothing
compileCtxExpr (Ctx ctx e@(ModBy (name,_) expr)) =
    do  reportErrorIfNoModify name
        (mt1,mt2) <- compileNameExpr (name,expr)
        case (mt1,mt2) of
            (Nothing,Nothing) -> return Nothing
            (Just t1,Nothing) -> return $ Just t1
            (Nothing,Just t2) -> return $ Just t2
            (Just t1,Just t2) -> reportIncompatibleOperands ctx t1 t2 >> return Nothing
compileCtxExpr (Ctx ctx e@(PostInc var)) = compileIncDecOp var "++"
compileCtxExpr (Ctx ctx e@(PostDec var)) = compileIncDecOp var "--"
compileCtxExpr (Ctx ctx e@(PreInc var)) = compileIncDecOp var "++"
compileCtxExpr (Ctx ctx e@(PreDec var)) = compileIncDecOp var "++"
compileCtxExpr (Ctx ctx expr0@(Set (name,All) expr)) =
    do  reportErrorIfNoModify name
        (mt1,mt2) <- compileNameExpr (name,expr)
        case (mt1,mt2) of
            (Nothing,Nothing) -> return Nothing
            (Just t1,Nothing) -> return $ Just t1
            (Nothing,Just t2) -> return $ Just t2
            (Just LLFloat,Just LLInteger) -> return $ Just  LLFloat
            (Just LLKey,Just LLString) -> return $ Just  LLKey
            (Just LLString,Just LLKey) -> return $ Just  LLString
            (Just t1,Just t2) | t1 == t2 -> return $ Just  t1
                    | otherwise -> reportIncompatibleOperands ctx t1 t2 >> return Nothing
compileCtxExpr (Ctx ctx expr0@(Set (name,S) expr)) =
    do  reportErrorIfNoModify name
        (mt1,mt2) <- compileNameExpr (name,expr)
        case (mt1,mt2) of
            (Nothing,Nothing) -> return Nothing
            (Just t1,Nothing) -> return $ Just t1
            (Nothing,Just t2) -> return $ Just t2
            (Just LLRot,Just LLFloat) -> return $ Just  LLFloat
            (Just LLRot,Just LLInteger) -> return $ Just  LLFloat
            (Just t1,Just t2) -> reportIncompatibleOperands ctx t1 t2 >> return Nothing
compileCtxExpr (Ctx ctx expr0@(Set (name,_) expr)) =
    do  reportErrorIfNoModify name
        (mt1,mt2) <- compileNameExpr (name,expr)
        case (mt1,mt2) of
            (Nothing,Nothing) -> return Nothing
            (Just t1,Nothing) -> return $ Just t1
            (Nothing,Just t2) -> return $ Just t2
            (Just LLVector,Just LLFloat) -> return $ Just  LLFloat
            (Just LLVector,Just LLInteger) -> return $ Just  LLFloat
            (Just LLRot,Just LLFloat) -> return $ Just  LLFloat
            (Just LLRot,Just LLInteger) -> return $ Just  LLFloat
            (Just t1,Just t2) -> reportIncompatibleOperands ctx t1 t2 >> return Nothing
compileCtxExpr (Ctx ctx (IntLit i)) = return $ Just  LLInteger
compileCtxExpr (Ctx ctx (FloatLit _)) = return $ Just  LLFloat
compileCtxExpr (Ctx ctx (StringLit _)) = return $ Just  LLString
compileCtxExpr (Ctx ctx (KeyLit _)) = return $ Just  LLKey
compileCtxExpr (Ctx ctx (ListExpr es)) = do
    mapM compileListExprElement es
    return $ Just  LLList
compileCtxExpr (Ctx ctx (VecExpr xExpr yExpr zExpr)) = 
    do  xt <- compileCtxExpr xExpr
        yt <- compileCtxExpr yExpr
        zt <- compileCtxExpr zExpr
        when (not (all (`elem` [LLInteger,LLFloat]) [c | Just c <- [xt,yt,zt]])) $ vsmAddErr (ctx, "invalid components for vector")
        return $ Just  LLVector
compileCtxExpr (Ctx ctx (RotExpr xExpr yExpr zExpr sExpr)) = 
    do  xt <- compileCtxExpr xExpr
        yt <- compileCtxExpr yExpr
        zt <- compileCtxExpr zExpr
        st <- compileCtxExpr sExpr
        when (not (all (`elem` [LLInteger,LLFloat]) [ c | Just c <- [xt,yt,zt,st]])) $ vsmAddErr (ctx, "invalid components for rotation")
        return $ Just LLRot

reportErrorIfNoModify (Ctx ctx name) = 
    when (isConstant name) $ vsmAddErr (ctx, "cannot modify " ++ name ++ " because it is a constant")
    
reportIncompatibleOperands ctx t0 t1 = 
    vsmAddErr (ctx,"the types of the operands aren't compatible (" ++ lslTypeString t0 ++ " vs. " ++ lslTypeString t1 ++ ")")
    
compileExpressions es = mapM_ compileCtxExpr es
compileMExpression Nothing = return (Just LLVoid)
compileMExpression (Just expr) = compileCtxExpr expr

compileCall (Ctx ctx fname) exprs = do
    vsmAddRef fname ctx
    funcs <- get'vsGFs
    case findFuncDec fname funcs of
        Nothing -> vsmAddErr (ctx,fname ++ ": no such function in scope") >> return Nothing
        Just (FuncDec _ t params) ->
            let vArg :: Int -> [Var] -> [Ctx Expr] -> VState ()
                vArg _ [] [] = return ()
                vArg _ (p:ps) [] = vsmAddErr (ctx, "mismatch of arguments vs. formal paramters in call to function " ++ fname)
                vArg _ [] (a:as) = vsmAddErr (ctx, "mismatch of arguments vs. formal paramters in call to function " ++ fname)
                vArg n (Var name t:ts) (arg:args) = 
                  do mt' <- compileCtxExpr arg
                     case mt' of
                         Nothing -> return ()
                         Just t' -> when (not (matchTypes t t')) $ vsmAddErr (ctx, "argument " ++ (show n) ++ " in call to function (" ++ fname ++ ") is of wrong type:" ++ (lslTypeString t') ++ ", should be " ++ (lslTypeString t))
                     vArg (n+1) ts args
            in vArg 1 (ctxItems params) exprs >> return (Just t)
            
compileEach :: (Ctx Expr,Ctx Expr) -> VState (Maybe LSLType, Maybe LSLType)
compileEach (expr1,expr2) = do
    mt1 <- compileCtxExpr expr1
    mt2 <- compileCtxExpr expr2
    return (mt1,mt2)
compileBothInteger ctx (expr1,expr2) = do
    (mt1,mt2) <- compileEach (expr1,expr2)
    case (mt1,mt2) of
        (Just LLInteger,Just LLInteger) -> return $ Just LLInteger
        (Just _, Just _) -> vsmAddErr (ctx,"operands are of incompatible type") >> return Nothing
        _ -> return Nothing
compileRelExpr ctx (expr1,expr2) = do
    do (mt1,mt2) <- compileEach (expr1,expr2)
       case (mt1,mt2) of
           (Nothing,_) -> return Nothing
           (_,Nothing) -> return Nothing
           (Just LLInteger,Just LLInteger) -> return $ Just LLInteger
           (Just LLInteger,Just LLFloat) -> return $ Just LLInteger
           (Just LLFloat,Just LLInteger) -> return $ Just LLInteger
           (Just LLFloat,Just LLFloat) -> return $ Just LLInteger
           (Just t0,Just t1) -> vsmAddErr (ctx,"operands are of incompatible types") >> return Nothing
compileNameExpr (Ctx ctx name,expr) = do
    vsmAddRef name ctx
    locals <- get'vsLocalVars
    globvars <- get'vsGVs
    mt <- compileCtxExpr expr
    let mvt = findType name (concat locals ++ globvars)
    when (isNothing mvt) $ vsmAddErr (ctx,"variable " ++ name ++ " not defined")
    return (mvt,mt)

compileIncDecOp (n@(Ctx ctx name),c) op = do
    vsmAddRef name ctx
    reportErrorIfNoModify n
    locals <- get'vsLocalVars
    vars <- get'vsGVs
    case (findType name (concat locals ++ vars),c) of
        (Nothing,_) ->  vsmAddErr (ctx, "variable " ++ name ++ " not found") >> return Nothing
        (Just LLInteger,All) -> return $ Just LLInteger
        (Just LLFloat,All) -> return $ Just LLFloat
        (Just LLRot,S) -> return $ Just LLFloat
        (Just LLVector,S) -> vsmAddErr (ctx, "s is not a valid component of " ++ name) >> return Nothing
        (Just t,All) -> vsmAddErr (ctx, name ++ " is not a valid operand for " ++ op) >> return Nothing
        (Just LLVector,_) -> return $ Just LLFloat
        (Just LLRot,_) -> return $ Just LLFloat
        _ -> vsmAddErr (ctx, name ++ " is not a valid operand for " ++ op) >> return Nothing

compileListExprElement e@(Ctx ctx _) = do
    mt <- compileCtxExpr e
    case mt of
        Nothing -> return ()
        Just t | t `elem` [LLVoid,LLList] -> vsmAddErr (ctx,"invalid list element")
               | otherwise -> return ()

validLSLScript :: Library -> LSLScript -> Validity CompiledLSLScript
validLSLScript library (LSLScript globs states) = 
    do  (typedVars,typedFuncs) <- typeGlobs library globs
        let vars = reverse typedVars
        let funcDecs = typedFuncs ++ predefFuncs
        (globvars,funcs,_,_) <- foldM (validGlob library vars funcDecs) ([],[],[],[]) globs
        validStates snames [] vars funcDecs states
        return (CompiledLSLScript (reverse globvars) funcs states)
    where snames = let sname (State cn _) = ctxItem cn in map sname states
validGlob _ vars funcDecs (globvars,funcs,imports,namesUsed) (GV v mexpr) =
    do when (isConstant $ varName v') $ throwError (srcCtx v, varName v' ++ " is a predefined constant")
       -- find the vars that are defined prior to this global variable -- only one of these
       -- vars may be used to initialize the global variable.
       when (varName v' `elem` namesUsed) $ throwError (srcCtx v, varName v' ++ " is already defined")
       let (vars',_) = break (\ var -> varName var == varName v') vars
       case mexpr of
           Nothing -> return (GDecl v' Nothing:globvars,funcs,imports, (varName v'):namesUsed)
           Just expr -> do
               t <- validCtxSimple vars' expr
               let vt = varType v'
               when (not (matchTypes vt t)) $ throwError (srcCtx expr, "expression not of the correct type")
               return ((GDecl v' $ Just (ctxItem expr)):globvars,funcs,imports, (varName v'):namesUsed)
    where v' = ctxItem v
validGlob _ vars funcDecs (globvars,funcs,imports,namesUsed) (GF f@(Func (FuncDec name t params) statements)) =
    do  noDupVars [] params
        when (ctxItem name `elem` namesUsed) $ throwError (srcCtx name, ctxItem name ++ " is already defined")
        returns <- validStatements False [] funcDecs vars t [] [[],params'] statements
        when (not returns && t /= LLVoid) $
            throwError (srcCtx name, "function " ++ (ctxItem name) ++ ": not all code paths return a value")
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
        when (name' `elem` namesUsed) $ throwStrError (name' ++ " imported from module is already defined")
        let rewrittenFunc = (Func (FuncDec (Ctx (srcCtx name) name') t params) $ rewriteStatements 0 renames statements)
        return (globvars,rewrittenFunc:funcs,imports,name':namesUsed)
rewriteGlob _ _ renames vars (globvars,funcs,imports,namesUsed) (GV (Ctx ctx (Var name t)) mexpr) =
    do  name' <- incontext (ctx,"renaming variable " ++ name) $ lookupM name renames
        when (name' `elem` namesUsed) $ throwStrError (name' ++ " imported from module is already defined")
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
        throwStrError ("wrong number of bindings in import: " ++ (show $ length freevars) ++ " required")
    else let f [] = return ()
             f ((x,y):xys) = 
                case (findType x (ctxItems freevars), findType y vars) of
                    (Nothing,_) -> throwStrError ("free variable " ++ x ++ " not found")
                    (_,Nothing) -> throwStrError ("global variable " ++ y ++ " not found")
                    (Just t0,Just t1) | not (matchTypes t0 t1) -> throwStrError ("types of " ++ x ++ " and " ++ y ++ " don't match")
                            | otherwise -> f xys
         in f bindings

validState snames used vars funcs (State (Ctx ctx name) handlers) =
    do when (name `elem` used) $ throwError (ctx, name ++ " already used")
       incontext (ctx,"") $ validHandlers snames [] funcs vars handlers
       return name
       
validStates snames used vars funcs [] = return ()
validStates snames used vars funcs (s:ss) =
    do  name <- validState snames used vars funcs s
        validStates snames (name:used) vars funcs ss
        
validCast t0 t1 =
    let validCasts = [(LLInteger,LLFloat), (LLFloat,LLInteger),
                      (LLInteger,LLString),(LLString,LLInteger),
                      (LLFloat,LLString),(LLString,LLFloat),
                      (LLString,LLVector),(LLVector,LLString),
                      (LLString,LLKey),(LLKey,LLString),
                      (LLRot,LLString),(LLString,LLRot),
                      (LLList,LLString),(LLString,LLList)] in
    do when (t0 /= t1 && (t0,t1) `notElem` validCasts) $ throwStrError ("can't cast from " ++ (lslTypeString t0) ++ " to " ++ (lslTypeString t1))

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
validSimple vars (Get (Ctx ctx name,_)) = throwError (ctx,"can't access vector/rotation component in global variable initialization")
validSimple vars (ListExpr []) = return LLList
validSimple vars (ListExpr (e:es)) = 
    do t <- validCtxSimple vars e
       when (t == LLList) $ throwError (srcCtx e,"lists cannot contain other lists")
       validSimple vars (ListExpr es)
validSimple vars (VecExpr e1 e2 e3) = validSimpleStructure vars LLVector [e1,e2,e3]
validSimple vars (RotExpr e1 e2 e3 e4) = validSimpleStructure vars LLRot [e1,e2,e3,e4]
validSimple vars (Neg e) = 
    do t <- validCtxSimple vars e
       when (t `notElem` [LLFloat, LLInteger]) $ throwError (srcCtx e,"operator only applicable to integers and floats in this context")
       return t
validSimple vars e = throwStrError ("expression is not valid in a static context.")

validSimpleStructure vars t [] = return t
validSimpleStructure vars t (e:es) =
    do  t' <- validCtxSimple vars e
        when (t' `notElem` [LLFloat,LLInteger]) $ throwError (srcCtx e, "literal of type " ++
             (lslTypeString t') ++ " not a valid element of " ++ (lslTypeString t))
        validSimpleStructure vars t es

validExpression :: Expr -> [FuncDec] -> [Var] -> [[Var]] -> Validity LSLType
validExpression (Cast t expr) funcs vars locals  = 
   do t' <- validCtxExpr expr funcs vars locals
      incontext (srcCtx expr, "") $ validCast t' t
      return t
validExpression (Get ((Ctx ctx name),component)) funcs vars locals =
   case (findType name (concat locals ++ vars) `mplus` findConstType name,component) of
       (Nothing,_) -> throwError (ctx, "undefined variable or constant: " ++ name)
       (Just LLRot,All) -> return LLRot
       (Just LLRot,_) -> return LLFloat
       (Just LLVector,All) -> return LLVector
       (Just LLVector,S) -> throwError (ctx,"s is not a valid component of a vector")
       (Just LLVector,_) -> return LLFloat
       (Just t,All) -> return t
       (Just t,_) -> throwError (ctx,"only vectors and rotations have components")
validExpression (Call name exprs) funcs vars locals = validCall funcs vars locals name exprs
validExpression (Not expr) funcs vars locals =
    do t <- validCtxExpr expr funcs vars locals
       when (t /= LLInteger) $ throwError (srcCtx expr, "expression is not an integer expression, which is required for applying the Not operator")
       return t
validExpression (Neg expr) funcs vars locals =
    do t <- validCtxExpr expr funcs vars locals
       when (t == LLList) $ throwError (srcCtx expr, "operator not applicable to list type")
       return t
validExpression (Inv expr) funcs vars locals =
    do t <- validCtxExpr expr funcs vars locals
       when (t /= LLInteger) $ throwError (srcCtx expr, "expression is not an integer expression, which is required for applying the inverse operator")
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
        when (not (all (`elem` [LLInteger,LLFloat]) [xt,yt,zt])) $ throwStrError "invalid components for vector"
        return LLVector
validExpression (RotExpr xExpr yExpr zExpr sExpr) funcs vars locals = 
    do  xt <- validCtxExpr xExpr funcs vars locals
        yt <- validCtxExpr yExpr funcs vars locals
        zt <- validCtxExpr zExpr funcs vars locals
        st <- validCtxExpr sExpr funcs vars locals
        when (not (all (`elem` [LLInteger,LLFloat]) [xt,yt,zt,st])) $ throwStrError "invalid components for rotation"
        return LLRot
--validExpression x funcs vars locals = error ("what to do with " ++ (show x))

validListExprElement (Ctx ctx e) funcs vars locals = do
    t <- validExpression e funcs vars locals
    when (t `elem` [LLVoid,LLList]) $ throwError (ctx,"invalid type for list element")
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
           (t0,t1) -> throwStrError ("operands are of incompatible types")
validBothInteger (expr1, expr2) funcs vars locals =
    do (t1,t2) <- validEach (expr1,expr2) funcs vars locals
       when (t1 /= LLInteger || t2 /= LLInteger) $ throwStrError ("operands are of incompatible types") 
       return LLInteger
validEach (expr1, expr2) funcs vars locals =
    do t1 <- validCtxExpr expr1 funcs vars locals
       t2 <- validCtxExpr expr2 funcs vars locals
       return (t1,t2)

validNameExpr (Ctx ctx name, expr) funcs vars locals = 
    case (findType name (concat locals ++ vars), 
          validCtxExpr expr funcs vars locals) of
        (Just t1, Right t2) -> return (t1,t2)
        (Nothing, _) -> throwError (ctx, "variable " ++ name ++ " not defined")
        (_,Left s) -> throwError s
validCall funcs vars locals (Ctx ctx fname) exprs =
    do  (FuncDec _ t params) <- findFuncDec fname funcs
        let vArg _ [] [] = return ()
            vArg _ (p:ps) [] = throwError (ctx, "mismatch of arguments vs. formal paramters in call to function " ++ fname)
            vArg _ [] (a:as) = throwError (ctx, "mismatch of arguments vs. formal paramters in call to function " ++ fname)
            vArg n (Var name t:ts) (arg:args) = 
              do t' <- validCtxExpr arg funcs vars locals
                 when (not (matchTypes t t')) $ throwError (ctx, "argument " ++ (show n) ++ " in call to function (" ++ fname ++ ") is of wrong type:" ++ (lslTypeString t') ++ ", should be " ++ (lslTypeString t))
                 vArg (n+1) ts args
        vArg 1 (ctxItems params) exprs
        return t

validCtxExpr (Ctx ctx e) fs vs ls = incontext (ctx,"") $ validExpression e fs vs ls

validIncDecOp (n@(Ctx ctx name),c) vars locals op =
    do  failIfNoModify n
        case (findType name (concat locals ++ vars),c) of
            (Nothing,_) ->  throwError (ctx, "variable " ++ name ++ " not found")
            (Just LLInteger,All) -> return LLInteger
            (Just LLFloat,All) -> return LLFloat
            (Just LLRot,S) -> return LLFloat
            (Just LLVector,S) -> throwError (ctx, "s is not a valid component of " ++ name)
            (Just t,All) -> throwError (ctx, name ++ " is not a valid operand for " ++ op)
            (Just LLVector,_) -> return LLFloat
            (Just LLRot,_) -> return LLFloat
            _ -> throwError (ctx, name ++ " is not a valid operand for " ++ op)

failIfNoModify (Ctx ctx name) = 
    when (isConstant name) $ throwError (ctx,"cannot modify " ++ name ++ " because it is a constant")

incompatibleOperands expr t0 t1 = 
    throwStrError ("types of the operands aren't compatible (" ++ 
             (lslTypeString t0) ++ " vs. " ++ (lslTypeString t1) ++ ")")

defined :: String -> [Var] -> Bool
defined n = any (\ (Var n' _) -> n == n')

validStatement _ _ funcs vars rtype labels locals@(scope:scopes) returns (Decl var@(Var name t) expr) = 
    do when (defined name $ concat locals) $ throwStrError ("variable " ++ name ++ " already defined") -- can't hide another local, even in a surrounding scope
       when (isConstant name) $ throwStrError ("variable " ++ name ++ " is a predefined constant")
       case expr of
           Nothing -> return ((var:scope):scopes,returns)
           Just expr' -> do t' <- validCtxExpr expr' funcs vars locals
                            when (not $ matchTypes t t') $ throwError (srcCtx expr', "type of expression in declaration of " ++ name ++ " does not match " ++ lslTypeString t)
                            return ((var:scope):scopes,returns)
validStatement scallow snames funcs vars rtype labels locals returns (While expr statement) =
    do t <- validCtxExpr expr funcs vars locals
       --when (t /= LLInteger) $ throwError (srcCtx expr, "expression is not a valid loop condition")
       validStatement scallow snames funcs vars rtype labels locals False statement
       return (locals,returns)
validStatement scallow snames funcs vars rtype labels locals returns (DoWhile statement expr) =
    do t <- validCtxExpr expr funcs vars locals
       --when (t /= LLInteger) $ throwError (srcCtx expr, "expression is not a valid loop condition")
       validStatement scallow snames funcs vars rtype labels locals False statement
       return (locals,returns)
validStatement scallow snames funcs vars rtype labels locals returns (For mexpr1 mexpr2 mexpr3 statement) =
    do  validExpressions mexpr1 funcs vars locals
        validExpressions mexpr3 funcs vars locals
        t <- validMExpression mexpr2 funcs vars locals
        --when (t /= LLInteger) $ throwStrError ("expression is not a valid loop condition")
        validStatement scallow snames funcs vars rtype labels locals False statement
        return (locals,returns)
validStatement scallow snames funcs vars rtype labels locals returns (If expr thenStmt elseStmt) =
    do t <- validCtxExpr expr funcs vars locals
       --when (t /= LLInteger) $ throwError (srcCtx expr, "expression is not a valid 'if' condition")
       (_,ret1) <- validStatement scallow snames funcs vars rtype labels locals False thenStmt
       (_,ret2) <- validStatement scallow snames funcs vars rtype labels locals False elseStmt
       return (locals,returns || ret1 && ret2)
validStatement _ _ _ _ _ _ locals returns NullStmt = return (locals,returns)
validStatement _ _ funcs vars rtype labels locals _ (Return Nothing) = 
    do  when (rtype /= LLVoid) (throwStrError "function must return a value")
        return (locals,True)
validStatement _ _ funcs vars rtype labels locals _ (Return (Just expr)) = 
    do  t <- validCtxExpr expr funcs vars locals
        when (t /= rtype && not (all (`elem` [LLString,LLKey]) [t,rtype])) (throwStrError "inappropriate return type for function/handler")
        return (locals,True)
validStatement scallow snames funcs vars rtype labels locals returns (StateChange name) = do
    when (not scallow) $ throwStrError "state changes not allowed from this context"
    when (not (name `elem` snames)) $ throwStrError (name ++ " is not a valid state")
    return (locals,returns)
validStatement _ _ funcs vars rtype labels locals returns (Do expr) = validCtxExpr expr funcs vars locals>>return (locals,returns)
validStatement scallow snames funcs vars rtype labels locals returns (Compound stmts) = 
    do  returns' <- validStatements  scallow snames funcs vars rtype labels ([]:locals) stmts
        return (locals,returns || returns')
validStatement _ _ funcs vars rtype labels locals _ (Label _) = return (locals,False)
validStatement _ _ funcs vars rtype labels locals returns (Jump s) = 
    do when (s `notElem` concat labels) $ throwStrError ("no such label to jump to: " ++ s)
       return (locals,returns)

validStatement' scallow snames funcs vars rtype labels locals returns line (Ctx ctx stmt) = 
  incontext (ctx, "") $ validStatement scallow snames funcs vars rtype labels locals returns stmt

validStatements :: Bool -> [String] -> [FuncDec] -> [Var] -> LSLType -> [[String]] -> [[Var]] -> [CtxStmt] -> Validity Bool
validStatements scallow snames funcs vars rtype labels locals stmts =
    do let newLabels = map (\ (Label s) -> s) $ filter isLabel (ctxItems stmts)
       (_,r') <- foldM (\ (l,r) (n, s) -> 
           validStatement' scallow snames funcs vars rtype (newLabels:labels) l r n s) (locals,False) $ zip ([1..]::[Int]) stmts
       return r'

validHandler snames used funcs vars (Handler (Ctx ctx name) args stmts) = 
    do  when (name `elem` used) $ throwError (ctx,name ++ " already used in state")
        types <- incontext (ctx,"handler: ") $ lookupM name goodHandlers
        when (types /= map varType args') $ throwError (ctx,"invalid argument types for handler " ++ name)
        when (length args /= (length $ nub $ map varName args')) $ throwError (ctx,"not all argument names are unique for handler " ++ name)
        validStatements True snames funcs vars LLVoid [] [[],args'] stmts
        return name
    where args' = ctxItems args
        
validHandlers _ _ _ _ [] = return ()
validHandlers snames used funcs vars (h:hs) =
    do  name <- validHandler snames used funcs vars h
        validHandlers snames (name:used) funcs vars hs

-- Validating a library of modules

compileModule :: LModule -> VState (Validity ([Global],[Func]))
compileModule m@(LModule globs freevars) = do
    preprocessGlobDefs_ "" globs
    mapM_ vsmAddGF predefFuncs
    mapM_ compileGlob globs
    err <- vsmFirstErr
    case err of
        Nothing -> do
            globals <- get'vsGlobals
            funcs <- get'vsFuncs
            return $ Right $ (globals,funcs)
        Just err -> return $ Left err
-- 
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
                list -> if any (`elem` list) (m:chain) then throwStrError "circular dependency"
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
       
compileLibrary :: [(String,LModule)] -> [(String,Validity (LModule,ModuleInfo))]
compileLibrary modules =
    let checkDep (n,m) = case moduleDependencies modules [] n of
            Right deps -> (n,Right (m,deps))
            Left s -> (n,Left s)
        categorize (good,bad) (n,Left s) = (good,(n,s):bad)
        categorize (good,bad) (n,Right (m,deps)) = ((n,(m,deps)):good,bad)
        (good,bad) = foldl categorize ([],[]) $ map checkDep modules
        sorted = sortModules good
        validate augLib (name,m) =
            case evalState (compileModule m) (emptyValidationState { vsLib = libFromAugLib augLib })  of
                Left s -> (name, Left s):augLib
                Right gs -> (name,Right (m,gs)):augLib
    in (foldl validate [] sorted) ++ (map (\ (n,s) -> (n,Left s)) bad)

validLibrary modules =
    let checkDep (n,m) = case moduleDependencies modules [] n of
            Right deps -> (n,Right (m,deps))
            Left s -> (n,Left s)
        categorize (good,bad) (n,Left s) = (good,(n,s):bad)
        categorize (good,bad) (n,Right (m,deps)) = ((n,(m,deps)):good,bad)
        (good,bad) = foldl categorize ([],[]) $ map checkDep modules
        sorted = sortModules good
        validate augLib (name,m) =
            case validModule (libFromAugLib augLib) m of
                Left s -> (name, Left s):augLib
                Right gs -> (name,Right (m,gs)):augLib
    in (foldl validate [] sorted) ++ (map (\ (n,s) -> (n,Left s)) bad)

libFromAugLib :: AugmentedLibrary -> Library
libFromAugLib augLib = 
   let f (name,Left s) = (name,Left s)
       f (name,Right (lm,_)) = (name, Right lm)
   in map f augLib
   
tstLib = [
    ("alpha", LModule [GI (nullCtx "beta") [] []] []),
    ("beta", LModule  [GI (nullCtx "gamma") [] []] []),
    ("gamma", LModule [GI (nullCtx "alpha") [] []] []),
    ("omega", LModule [GI (nullCtx "lambda") [] []] []),
    ("lambda", LModule [GI (nullCtx "kappa") [] [], GI (nullCtx "sigma") [] []] []),
    ("kappa", LModule [] []),
    ("sigma", LModule [] [])]

-- | Transform a script into a module, generating function names for each handler, using the 
-- pattern <state-name>_state_<handler-name>.
-- The script following script:
--  
--  default {
--      state_entry() {
--          llSay(0,"Hello Avatar");
--      }
--  }
-- 
--  would be transformed into a module equivalent to:
-- 
--  $module
--  
--  default_state_state_entry() {
--      llSay(0,"Hello Avatar");
--  }
moduleFromScript :: CompiledLSLScript -> LModule
moduleFromScript script = LModule globDefs []
    where globDefs = globDefsFromGlobs (scriptGlobals script) ++
                     globDefsFromFuncs (scriptFuncs script) ++
                     funcDefsFromStates (scriptStates script)
          globDefsFromGlobs = map globDefFromGlob
          globDefsFromFuncs = map GF
          funcDefsFromStates = concatMap funcDefsFromState
          
globDefFromGlob (GDecl v me) = GV (nullCtx v) (fmap nullCtx me)
funcDefsFromState (State ctxnm handlers) = map (funcDefFromHandler (ctxItem ctxnm)) handlers
funcDefFromHandler stateName (Handler ctxnm params stmts) = GF $ Func (FuncDec combinedName LLVoid params) stmts
    where combinedName = nullCtx $ stateName ++ "$$" ++ (ctxItem ctxnm)

rewriteCtxExpr :: [(String,String)] -> Ctx Expr -> Ctx Expr
rewriteCtxExpr renames = everywhere (mkT (rewriteName renames))

rewriteName :: [(String,String)] -> Ctx String -> Ctx String
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

rewriteCtxExprs bindings ctxExprs = map (rewriteCtxExpr bindings) ctxExprs

rewriteMExpression bindings = fmap (rewriteCtxExpr bindings)


