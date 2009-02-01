{-# OPTIONS_GHC -fwarn-incomplete-patterns -XStandaloneDeriving -XNoMonomorphismRestriction #-}
module Language.Lsl.Internal.Optimize(optimizeScript,OptimizerOption(..)) where

import Control.Monad.State hiding (State)
import qualified Control.Monad.Identity as Id
import qualified Control.Monad.State as State(State)

import Data.Bits((.&.),(.|.),xor,shiftL,shiftR,complement)
import Data.Generics
import Data.Generics.Extras.Schemes(everythingTwice,downup)
import Data.List(foldl',nub,lookup)
import Data.Graph
import qualified Data.Set as Set
import qualified Data.Map as M

import Debug.Trace

import Language.Lsl.Parse
import Language.Lsl.Internal.Constants(allConstants,Constant(..),findConstVal)
import Language.Lsl.Internal.FuncSigs(funcSigs)
import Language.Lsl.Internal.InternalLLFuncs(internalLLFuncs,internalLLFuncNames)
import Language.Lsl.Internal.OptimizerOptions(OptimizerOption(..))
import Language.Lsl.Syntax(CompiledLSLScript(..),Expr(..),Statement(..),Var(..),
                           Func(..),FuncDec(..),State(..),Ctx(..),Handler(..),
                           Global(..),Component(..),SourceContext(..),rewriteCtxExpr)
import Language.Lsl.Internal.Type(LSLType(..))
import Language.Lsl.Internal.Pragmas(Pragma(..))
import Language.Lsl.Internal.Type(LSLValue(..))
import Language.Lsl.UnitTestEnv(simSFunc)

optionInlining = elem OptimizationInlining

-- deriving instance (Show a) => Show (SCC a)

optimizeScript :: [OptimizerOption] -> CompiledLSLScript -> CompiledLSLScript
optimizeScript options script@(CompiledLSLScript gs fsIn ss) = CompiledLSLScript gsReachable fsReachable ss1
   where inline = optionInlining options
         gcs = globalConstants gs fsIn ss
         scc = graphInfo fsIn
         funFacts = sccsPurity gcs basicFunctionFacts scc
         pure = Set.fromList [ nm | (nm,ff) <- M.toList funFacts, isPureFunction ff]
         --pure = trace (show pure') pure'
         ifs = [ f | AcyclicSCC f <- scc, inlineable f]  -- inlineables
         nifs = [ f | f <- fsIn, fname f `notElem` (map fname ifs)] -- non-inlineables
         ss1 = if inline then simp $ map runInliningOnState (simp ss) else ss
         ifs' = foldl (\ fs f -> let f' = runInliningOnFunc funFacts fs gs f in f':fs) [] ifs -- inlineables that have had any inlining done
         nifs' = map (runInliningOnFunc funFacts ifs' gs) nifs -- non-inlineables that have had any inlining done
         fs' = if inline then (nifs' ++ ifs') else fsIn
         fsReachable = reachableFuncs ss1 (simp fs') -- funcs that are still reachable from handlers
         gsReachable = reachableGlobs gs fsReachable ss1 -- globals that are still reachable from handlers/funcs
         simp = if inline then simplify script pure gcs else id
         runInliningOnState s@(State nm hs) = if noinlining nm then s
             else (State nm (map (runInliningOnHandler funFacts ifs' gs) hs))

hasPragma p (Ctx (Just SourceContext { srcPragmas = l }) _) | p `elem` l = True
                                                            | otherwise  = False
hasPragma _ _ = False

inlineable = hasPragma PragmaInline
noinlining = hasPragma PragmaNoInline

data EPKey = HK String String | FK String deriving (Show, Eq, Ord)

stateEdges :: [State] -> [(EPKey,EPKey,[EPKey])]
stateEdges ss = concatMap (\ (State (Ctx _ nm) hs) -> (map (\ h@(Handler (Ctx _ nm') _ _) -> (HK nm nm',HK nm nm', (map  FK (handlerCallsFuncs h)))) hs)) ss

funcEdges :: [Ctx Func] -> [(EPKey,EPKey,[EPKey])]
funcEdges fs = map (\ f -> let fn = fname f in (FK fn, FK fn,map FK $ funcCallsFuncs f)) fs

reachableFuncs ss fs = [ f | f <- fs, fname f `elem` allReachableFnames]
    where ses = stateEdges ss
          (graph,v2n,k2v) = graphFromEdges (ses ++ funcEdges fs)
          allReachableIndices = nub $ concatMap (reachable graph) [ i | Just i <- map ( \ (k,_,_) -> k2v k) ses]
          allReachableFnames = [fn | (FK fn,_,_) <- map ( \ i -> v2n i) allReachableIndices]

varNameInList :: Var -> [String]
varNameInList = (:[]) . varName

varsDefinedByHandler :: Handler -> [String]
varsDefinedByHandler = everything (++) ([] `mkQ` varNameInList)

varsDefinedByFunc :: Ctx Func -> [String]
varsDefinedByFunc = everything (++) ([] `mkQ` varNameInList)

labels (Label nm) = [nm]
labels _ = []

labelsDefinedByFunc :: Ctx Func -> [String]
labelsDefinedByFunc = everything (++) ([] `mkQ` labels)

labelsDefinedByHandler :: Handler -> [String]
labelsDefinedByHandler = everything (++) ([] `mkQ` labels)

namesDefinedByHandler :: Handler -> [String]
namesDefinedByHandler handler = labelsDefinedByHandler handler ++ varsDefinedByHandler handler

namesDefinedByFunc :: Ctx Func -> [String]
namesDefinedByFunc func = labelsDefinedByFunc func ++ varsDefinedByFunc func

exprCallsFuncDirectly :: Expr -> [String]
exprCallsFuncDirectly (Call (ctxName) _) = [ctxItem ctxName]
exprCallsFuncDirectly _ = []

funcCallsFuncs :: Ctx Func -> [String]
funcCallsFuncs = everything (++) ([] `mkQ` exprCallsFuncDirectly)
       
stateCallsFuncs :: State -> [String]
stateCallsFuncs = everything (++) ([] `mkQ` exprCallsFuncDirectly)

handlerCallsFuncs :: Handler -> [String]
handlerCallsFuncs = everything (++) ([] `mkQ` exprCallsFuncDirectly)

fname (Ctx _ (Func (FuncDec (Ctx _ name) _ _) _)) = name
fstmts (Ctx _ (Func _ stmts)) = stmts

graphInfo :: [Ctx Func] -> [SCC (Ctx Func)]
graphInfo funcs = scc
    where edges = (map (\ f -> (f, fname f, funcCallsFuncs f)) funcs)
          scc = stronglyConnComp edges
          acyc = [ f | AcyclicSCC f <- scc]
          cyc = [ l | CyclicSCC l <- scc]
          
newNames seed curNames verbotenNames = foldl' newName (seed,[]) curNames
    where newName (n,renames) name =
                  if name `notElem` verbotenNames 
                      then (n,(name,name):renames)
                      else if nm `notElem` verbotenNames 
                          then (n+1,(name,nm):renames)
                          else newName (n+1,renames) name
              where nm = "_" ++ show n ++ "inl"

nullCtx :: a -> Ctx a              
nullCtx = Ctx Nothing 

newtype FunctionFacts = FunctionFacts { isPureFunction :: Bool } deriving (Show)

data OptimizerState = OptimizerState { 
    optAllFuncs :: !(M.Map String (Ctx Func)),
    optFunFacts :: !(M.Map String FunctionFacts),
    optNameIndex :: Int, 
    optGlobalNames :: !(Set.Set String),
    optVerbotenNames :: !(Set.Set String),
    optLocals :: ![[String]],
    optInlinerRenames :: ![M.Map String String], -- names that must be renamed in the 'destination' function/handler
    optRenames :: !(M.Map String String), -- names that must be renamed in the function to-be-inlined
    optRewrites :: !(M.Map String Expr),
    optRetVar :: !(Maybe String),
    optStmts :: ![[Ctx Statement]] }
    
type OState a = State.State OptimizerState a

basicFunctionFacts = M.fromList (zip internalLLFuncNames (repeat (FunctionFacts { isPureFunction = True }))) `M.union`
                     M.fromList [(nm,FunctionFacts False) | (nm,_,_) <- funcSigs]

freshOptimizerState funFacts fs gnames = OptimizerState { 
    optAllFuncs = M.fromList (map (\ f@(Ctx _ (Func (FuncDec nm _ _) _)) -> (ctxItem nm,f)) fs),
    optFunFacts = funFacts,
    optNameIndex = 0, 
    optGlobalNames = (Set.fromList gnames),
    optVerbotenNames = Set.empty,
    optLocals = [],
    optInlinerRenames = [],
    optRenames = M.empty,
    optRewrites = M.empty,
    optRetVar = Nothing,
    optStmts = [] }
    
removeOStateStmts :: OState [Ctx Statement]
removeOStateStmts = do
    st <- get
    let stmts = (concat (reverse (optStmts st)))
    put st { optStmts = [] }
    return stmts
    
refreshOState :: OState ()
refreshOState = do
    st <- get
    put st { optRenames = M.empty, optRetVar = Nothing, optStmts = [] }

pushLocal s = do
    st <- get
    case optLocals st of
        [] -> put st { optLocals = [[s]] }
        top:rest -> put st { optLocals = (s:top):rest }
        
mkName s = do
    st <- get
    let i = optNameIndex st
    let name = "_" ++ s ++ show i
    put st { optNameIndex = i + 1 }
    st <- get
    verboten <- isVerboten name
    if  not verboten
        then do addVerboten name
                return name
        else mkName s -- make another...

putRetVar s = get >>= ( \ os -> put os { optRetVar = Just s })
clearRetVar s = get >>= (\ os -> put os { optRetVar = Nothing })

addVerboten name = do
    st <- get
    let s = optVerbotenNames st
    put st { optVerbotenNames = (Set.insert name s) }
    
addRename s s' = do
    st <- get
    put st { optRenames = M.insert s s' (optRenames st) }
    
rewriteLabel s = do
    renames <- get >>= return . optRenames
    return $ maybe s id (M.lookup s renames)

renameToNew s = do
   verboten <- isVerboten s
   if verboten 
       then do
           s' <- mkName s
           addRename s s'
           return s'
       else do
           st <- get
           put st { optVerbotenNames = Set.insert s (optVerbotenNames st) }
           return s
   
pushInlinerScope = do
    st <- get
    put st { optInlinerRenames = M.empty : (optInlinerRenames st) }
popInlinerScope = do
    st <- get
    case optInlinerRenames st of
        [] -> error "cannot pop inliner scope"
        (top:rest) -> put st { optInlinerRenames = rest }
        
renameToNewInInliner s = do
   --verboten <- isVerboten s
   verboten <- get >>= return . (Set.member s) . optGlobalNames
   if verboten 
       then do
           s' <- mkName s
           st <- get
           case optInlinerRenames st of
               [] -> error "empty inliner rename stack"
               (top:rest) -> put st { optInlinerRenames = M.insert s s' top : rest }
           return s'
       else do
           st <- get
           put st { optVerbotenNames = Set.insert s (optVerbotenNames st) }
           return s
   
inlinerRenamingFor s = do
        renameStack <- get >>= return . optInlinerRenames
        return $ renamingFor renameStack s
   where renamingFor [] s = s
         renamingFor (top:rest) s = case M.lookup s top of
             Just s' -> s'
             Nothing -> renamingFor rest s
             
renameVar (Var s t) = do
   s' <- renameToNew s
   return (Var s' t)
   
getRewriteInfo :: OState (M.Map String String, M.Map String Expr)
getRewriteInfo = get >>= ( \ st -> return (optRenames st, optRewrites st))

isVerboten s = do
    verbotenNames <- get >>= return . optVerbotenNames
    globalNames <- get >>= return . optGlobalNames
    return (s `Set.member` (verbotenNames `Set.union` globalNames))
          
inlineProc :: 
    Ctx Func -> -- the function to inline
    [Ctx Expr] -> -- the arguments to the call
    OState ([Ctx Statement],[Ctx Statement]) -- 
inlineProc (Ctx c (Func fd ss)) args = do
        endLabel <- mkName "end"
        parmVars <- mkParmVars ss (zip ps args)
        stmts <- inlineStmts endLabel (map ctxItem ss) >>= return . map nullCtx >>= return . withoutFinalJumpTo endLabel
        return (if jumpsTo endLabel stmts == 0 then [] else [nullCtx $ Label endLabel], parmVars ++ stmts)
               -- simplify $ nullCtx (Compound (parmVars ++ stmts)))
    where ft = funcType fd
          ps = funcParms fd
    
mkParmVars :: [Ctx Statement] -> [(Ctx Var,Ctx Expr)] -> OState [Ctx Statement]
mkParmVars ss ves = mapM (mkParmVar ss) ves >>= return . concat
mkParmVar ss (Ctx _ v@(Var nm _),arg) = do
    funFacts <- get >>= return . optFunFacts
    locals <- get >>= return . concat . optLocals
    if isRelativelyPure locals funFacts arg && 
       (staticComplexity arg < 2 || usageCount nm ss == 1) && not (nm `isModifiedIn` ss) && (simpleRef arg || nm `isUsedOnlyWholeIn` ss)
        then do
            st <- get
            put st { optRewrites = M.insert (varName v) (ctxItem arg) (optRewrites st) }
            return []
        else do
            v' <- renameVar v
            return [nullCtx $ Decl v' $ Just arg]
   where simpleRef (Ctx _ (Get (_,All))) = True
         simpleRef _                     = False
         
printStatement s = show s ++ "\n"

inlineFunc :: Ctx Func -> [Ctx Expr] -> OState (Expr,[Ctx Statement])
inlineFunc f@(Ctx _ (Func (FuncDec _ t _) _)) args 
    | t == LLVoid = error "cannot inline a void function in a context that requires a value (internal error!)"
    | otherwise = do
        ret <- mkName "ret"
        putRetVar ret
        (endStmts,stmts) <- inlineProc f args
        let count = countOfSetsOf ret stmts
        case (count,last stmts) of
            (1,Ctx _ (Do ((Ctx _ (Set _ expr))))) -> return (ctxItem expr,init stmts)
            _ -> return ((Get (nullCtx ret,All)),(nullCtx $ Decl (Var ret t) Nothing): (stmts ++ endStmts))

inlineVoidFunc :: Ctx Func -> [Ctx Expr] -> OState [Ctx Statement]
inlineVoidFunc f args = do
    (endStmts,stmts) <- inlineProc f args
    return (stmts ++ endStmts)

inlineStmts :: String -> [Statement] -> OState [Statement]
inlineStmts _ [] = return []
inlineStmts endLabel (NullStmt:NullStmt:stmts) = inlineStmts endLabel (NullStmt:stmts)
inlineStmts endLabel (NullStmt:stmts) = inlineStmts endLabel stmts >>= return . (NullStmt:)
inlineStmts endLabel (Return Nothing:stmts) = inlineStmts endLabel stmts >>= return . (Jump endLabel:)
inlineStmts endLabel (Return (Just expr):stmts) = do
    stmts' <- inlineStmts endLabel stmts
    retVar <- get >>= return . optRetVar
    (renames,rewrites) <- getRewriteInfo
    case retVar of
        Nothing -> return (Do (rewriteCtxExpr' renames rewrites expr):Jump endLabel:stmts')
        Just rv -> 
            return (Do (nullCtx (Set (nullCtx rv,All) (rewriteCtxExpr' renames rewrites expr))):Jump endLabel:stmts')
inlineStmts endLabel (Decl v mexpr:stmts) = do
    v' <- renameVar v
    (renames,rewrites) <- getRewriteInfo
    stmts' <- inlineStmts endLabel stmts
    return (Decl v' (fmap (rewriteCtxExpr' renames rewrites) mexpr):stmts')
inlineStmts endLabel (Do expr:stmts) = do
    stmts' <- inlineStmts endLabel stmts
    (renames,rewrites) <- getRewriteInfo
    return (Do (rewriteCtxExpr' renames rewrites expr):stmts')
inlineStmts endLabel (Label s:stmts) = do
    -- must rename the label BEFORE rewriting the rest of the statements
    s' <- renameToNew s
    stmts' <- inlineStmts endLabel stmts
    return (Label s':stmts)
inlineStmts endLabel (Jump s:stmts) = do
    -- must process the rest of the statements BEFORE rewriting the Jump
    stmts' <- inlineStmts endLabel stmts
    s' <- rewriteLabel s
    return (Jump s':stmts')
inlineStmts endLabel (StateChange s:stmts) = inlineStmts endLabel stmts >>= return . (StateChange s:)
inlineStmts endLabel (Compound ss:stmts) = do
    stmts' <- inlineStmts endLabel stmts
    st <- get
    ss' <- inlineStmts endLabel (map ctxItem ss)
    return (Compound (map nullCtx ss'):stmts')
inlineStmts endLabel (While expr stmt:stmts) =  do
    (renames,rewrites) <- getRewriteInfo
    stmts' <- inlineStmts endLabel stmts
    ss <- inlineStmts endLabel [stmt]
    return (While (rewriteCtxExpr' renames rewrites expr) (Compound (map nullCtx ss)) : stmts')
inlineStmts endLabel (DoWhile stmt expr:stmts) = do
    stmts' <- inlineStmts endLabel stmts
    ss <- inlineStmts endLabel [stmt]
    (renames,rewrites) <- getRewriteInfo
    return (DoWhile (Compound (map nullCtx ss)) (rewriteCtxExpr' renames rewrites expr) : stmts')
inlineStmts endLabel (For es0 e es1 stmt:stmts) = do
        (renames,rewrites) <- getRewriteInfo
        let rewriteExprs = map (rewriteCtxExpr' renames rewrites)
        stmts' <- inlineStmts endLabel stmts
        ss <- inlineStmts endLabel [stmt]
        let body = case ss of
                s:[] -> s
                _ -> Compound (map nullCtx ss)
        return (For (rewriteExprs es0) (fmap (rewriteCtxExpr' renames rewrites) e) (rewriteExprs es1) body: stmts')
inlineStmts endLabel (If e s0 s1:stmts) = do
    (renames,rewrites) <- getRewriteInfo
    stmts' <- inlineStmts endLabel stmts
    s0s <- inlineStmts endLabel [s0]
    s1s <- inlineStmts endLabel [s1]
    return (If (rewriteCtxExpr' renames rewrites e) (Compound (map nullCtx s0s)) (Compound (map nullCtx s1s)) : stmts')
    
runInliningOnFunc :: M.Map String FunctionFacts -> [Ctx Func] -> [Global] -> Ctx Func -> Ctx Func
runInliningOnFunc ff fs gs f = if noinlining f then f else
    evalState (performInliningOnFunc f) (freshOptimizerState ff fs (map (\ (GDecl (Var nm _) _) -> nm) gs))
    
performInliningOnFunc :: Ctx Func -> OState (Ctx Func)
performInliningOnFunc f@(Ctx ctx (Func (FuncDec nm t parms) stmts)) = do
        pushInlinerScope
        parms' <- mapM renameParm parms
        st <- get
        put st { optVerbotenNames = Set.fromList verbotenNames, optLocals = [map (\ (Ctx _ v) -> varName v) parms'] }
        stmts' <- mapM performInliningForStmt stmts
        popInlinerScope
        return (Ctx ctx (Func (FuncDec nm t parms') $ concat stmts'))
    where verbotenNames = namesDefinedByFunc f
          renameParm (Ctx _ (Var nm t)) = do
              nm' <- renameToNewInInliner nm
              return (nullCtx $ Var nm' t)
    
runInliningOnHandler :: M.Map String FunctionFacts -> [Ctx Func] -> [Global] -> Handler -> Handler
runInliningOnHandler ff fs gs h = if noinlining (handlerName h) then h else
    evalState (performInliningOnHandler h) (freshOptimizerState ff fs (map (\ (GDecl (Var nm _) _) -> nm) gs))
    
performInliningOnHandler :: Handler -> OState Handler
performInliningOnHandler h@(Handler nm parms stmts) = do
        pushInlinerScope
        parms' <- mapM renameParm parms
        st <- get
        put st { optVerbotenNames = Set.fromList verbotenNames, optLocals = [map (\ (Ctx _ v) -> varName v) parms] }
        stmts' <- mapM performInliningForStmt stmts
        popInlinerScope
        return (Handler nm parms' (concat stmts'))
    where verbotenNames = namesDefinedByHandler h
          renameParm (Ctx _ (Var nm t)) = do
              nm' <- renameToNewInInliner nm
              return (nullCtx $ Var nm' t)
    
performInliningForStmt :: Ctx Statement -> OState [Ctx Statement]
performInliningForStmt s@(Ctx _ (Do (Ctx _ (Call cnm exprs)))) = do
        refreshOState
        (es,sss) <- inlineExprs exprs
        fs <- get >>= return . optAllFuncs
        case M.lookup (ctxItem cnm) fs of
            Nothing -> return (concat sss ++ [nullCtx $ (Do (nullCtx (Call cnm es)))])
            Just f -> do 
               ss <- inlineVoidFunc f es
               return $ (concat sss) ++ ss
performInliningForStmt s@(Ctx _ (Do expr)) = do
    refreshOState
    expr' <- inlineExpr expr
    stmts <- removeOStateStmts
    return (stmts ++ [nullCtx (Do expr')])
performInliningForStmt s@(Ctx _ (Compound ss)) = do
    pushInlinerScope
    refreshOState
    st <- get
    let locals = optLocals st
    put st { optLocals = []:locals }
    sss <- mapM performInliningForStmt ss
    put st { optLocals = locals }
    popInlinerScope
    return [(nullCtx (Compound (concat sss)))]
performInliningForStmt (Ctx _ (While expr s)) = do
    refreshOState
    bgnLoop <- mkName "bgnLoop"
    (expr',stmts) <- inlineExpr' expr
    ss <- performInliningForStmt (nullCtx s)
    return $ case (stmts,ss) of
        ([],[s']) -> [nullCtx $ While expr' (ctxItem s')]
        ([],_) -> [nullCtx $ While expr' (Compound ss)]
        _ -> ((nullCtx $ Label bgnLoop) : (stmts ++ case ss of
            [] -> [nullCtx $ If expr' (Jump bgnLoop) NullStmt]
            _ -> [nullCtx $ If expr' (Compound (ss ++ [nullCtx $ Jump bgnLoop])) NullStmt]))
performInliningForStmt s@(Ctx _ NullStmt) = refreshOState >> return [nullCtx NullStmt]
performInliningForStmt s@(Ctx _ (Decl (Var v t) Nothing)) = do
    refreshOState
    v' <- renameToNewInInliner v
    pushLocal $ v'
    return [nullCtx (Decl (Var v' t) Nothing)]
performInliningForStmt s@(Ctx _ (Decl (Var v t) (Just expr))) = do
    refreshOState
    (expr',stmts) <- inlineExpr' expr
    v' <- renameToNewInInliner v
    pushLocal v'
    return (stmts ++ [nullCtx (Decl (Var v' t) (Just expr'))])
performInliningForStmt s@(Ctx _ (Return Nothing)) = refreshOState >> return [nullCtx (Return Nothing)]
performInliningForStmt s@(Ctx _ (Return (Just expr))) = do
    refreshOState
    expr' <- inlineExpr expr
    stmts <- removeOStateStmts
    return (stmts ++ [nullCtx (Return (Just expr'))])
performInliningForStmt (Ctx _ l@(Label _)) = refreshOState >> return [nullCtx l]
performInliningForStmt (Ctx _ j@(Jump _)) = refreshOState >> return [nullCtx j]
performInliningForStmt (Ctx _ s@(StateChange _)) = refreshOState >> return [nullCtx s]
performInliningForStmt (Ctx _ (DoWhile s expr)) = do
    refreshOState
    bgnLoop <- mkName "bgnLoop"
    ss <- performInliningForStmt (nullCtx s)
    (expr',stmts) <- inlineExpr' expr
    let s' = case ss ++ stmts of
                 [s''] -> ctxItem s''
                 ss' -> Compound ss'
    return [nullCtx $ DoWhile s' expr']
    --return ((nullCtx $ Label bgnLoop) : (ss ++ stmts ++ [nullCtx $ If expr' (Jump bgnLoop) NullStmt]))
performInliningForStmt (Ctx _ (For ies1 mte ses2 s)) = do
    refreshOState
    bgnLoop <- mkName "bgnLoop"
    (ies1',iss1s) <- inlineExprs ies1
    (mte',tss) <- case mte of
              Nothing -> return (Nothing,[])
              Just te -> do (te',tss) <- inlineExpr' te
                            return (Just te',tss)
    (ses2', sss2s) <- inlineExprs ses2
    ss <- performInliningForStmt (nullCtx s)
    let iss1 = concat iss1s
    let sss2 = concat sss2s
    case (iss1,tss,sss2,ss) of
        ([],[],[],[s']) -> return [(nullCtx $ For ies1' mte' ses2' (ctxItem s'))]
        ([],[],[],_) -> return [(nullCtx $ For ies1' mte' ses2' (Compound ss))]
        _ -> return (iss1 ++ (map (nullCtx . Do) ies1') ++ [nullCtx $ Label bgnLoop] ++ rest)
            where rest = case mte' of
                     Nothing -> ss ++ sss2 ++ (map (nullCtx . Do) ses2') ++ [nullCtx $ Jump bgnLoop]
                     Just te' -> tss ++ 
                        [nullCtx $ If te' (Compound (ss ++ sss2 ++ (map (nullCtx . Do) ses2') ++ [nullCtx $ Jump bgnLoop])) NullStmt]
performInliningForStmt (Ctx _ (If e s0 s1)) = do
    refreshOState
    (e',ess) <- inlineExpr' e
    s0s <- performInliningForStmt (nullCtx s0)
    s1s <- performInliningForStmt (nullCtx s1)
    let branch1 = case s0s of
            [s] -> ctxItem s
            _ -> Compound s0s
    let branch2 = case s1s of
            [s] -> ctxItem s
            _ -> Compound s1s
    return (ess ++ [nullCtx $ If e' branch1 branch2])
    
inlineExprs :: [Ctx Expr] -> OState ([Ctx Expr], [[Ctx Statement]])
inlineExprs exprs = do
      es <- mapM inlineExpr' exprs
      return $ foldr combine ([],[]) es
   where combine :: (Ctx Expr, [Ctx Statement]) -> ([Ctx Expr],[[Ctx Statement]]) -> ([Ctx Expr],[[Ctx Statement]])
         combine (e,ss) (es,sss) = (e:es,ss:sss)
inlineExpr' :: Ctx Expr -> OState (Ctx Expr,[Ctx Statement])
inlineExpr' expr = do
    expr' <- inlineExpr expr
    stmts <- removeOStateStmts
    return (expr',stmts)
    
inlineExpr :: Ctx Expr -> OState (Ctx Expr)
inlineExpr = everywhereM (mkM inlineCall `extM` renameRef)

inlineCall c@(Call (Ctx _ nm) es) = do
    fs <- get >>= return . optAllFuncs
    tmp <- get >>= return . optInlinerRenames
    tmp1 <- get >>= return . optRenames
    case M.lookup nm fs of
        Nothing -> return c
        Just f -> do
            (e, stmts) <- inlineFunc f es
            st <- get
            put st { optStmts = (stmts:(optStmts st)) }
            return e
inlineCall e = return e
renameRef :: (Ctx String, Component) -> OState (Ctx String,Component)
renameRef (Ctx ctx nm, v) = do
    nm' <- inlinerRenamingFor nm
    return (Ctx ctx nm', v)
    
isRelativelyPure :: [String] -> (M.Map String FunctionFacts) -> Ctx Expr -> Bool
isRelativelyPure locals ff = everything (&&) (True `mkQ` go)
    where
        go (Get (cnm,_)) = nm `elem` locals || nm `elem` (map constName allConstants) where nm = ctxItem cnm
        go (Set _ _) = False
        go (IncBy _ _) = False
        go (DecBy _ _) = False
        go (MulBy _ _) = False
        go (DivBy _ _) = False
        go (ModBy _ _) = False
        go (PostInc _) = False
        go (PostDec _) = False
        go (PreInc _) = False
        go (PreDec _) = False
        go (Call (Ctx _ nm) _) = 
            case M.lookup nm ff of
                Nothing -> False
                Just facts -> isPureFunction facts
        go _ = True

-- a notion of how much codespace would be wasted if an expression
-- was copied wherever it was used, versus computed in one place
staticComplexity :: Ctx Expr -> Int
staticComplexity = everything (+) (0 `mkQ` go)
   where go :: Expr -> Int
         go e = 1

rewriteCtxExpr' :: (M.Map String String) -> (M.Map String Expr) -> Ctx Expr -> Ctx Expr
rewriteCtxExpr' renames rewrites = everywhere' (mkT rwName `extT` rwExpr)
    where 
          rwExpr e@(Get (Ctx _ nm,All)) =
              case M.lookup nm rewrites of
                  Nothing -> e
                  Just e' -> e'
          rwExpr e = e
          rwName c@(Ctx _ nm) =
              case M.lookup nm renames of
                  Nothing -> c
                  Just nm' -> nullCtx nm'
              
usageCount :: String -> [Ctx Statement] -> Int
usageCount nm = everything (+) (0 `mkQ` refCount)
    where refCount :: (Ctx String, Component) -> Int
          refCount (Ctx _ nm',_) | nm == nm' = 1
                                 | otherwise = 0

isUsedOnlyWholeIn :: String -> [Ctx Statement] -> Bool
isUsedOnlyWholeIn nm = everything (&&) (True `mkQ` whole)
   where whole :: (Ctx String, Component) -> Bool
         whole (_, All) = True
         whole (Ctx _ nm',_) | nm == nm' = False
                             | otherwise = True

countOfSetsOf :: String -> [Ctx Statement] -> Int
countOfSetsOf nm = everything (+) (0 `mkQ` count)
    where count (Set (Ctx _ nm',All) _) | nm == nm' = 1
                                        | otherwise = 0
          count _ = 0
          
isModifiedIn :: String -> [Ctx Statement] -> Bool
isModifiedIn nm = everything (||) (False `mkQ` modified)
    where modified (Set (Ctx _ nm',_) _) = nm == nm'
          modified (IncBy (Ctx _ nm',_) _) = nm == nm'
          modified (DecBy (Ctx _ nm',_) _) = nm == nm'
          modified (MulBy (Ctx _ nm',_) _) = nm == nm'
          modified (DivBy (Ctx _ nm',_) _) = nm == nm'
          modified (ModBy (Ctx _ nm',_) _) = nm == nm'
          modified (PreDec (Ctx _ nm',_)) = nm == nm'
          modified (PreInc (Ctx _ nm',_)) = nm == nm'
          modified (PostDec (Ctx _ nm',_)) = nm == nm'
          modified (PostInc (Ctx _ nm',_)) = nm == nm'
          modified _ = False

jumpsTo :: String -> [Ctx Statement] -> Int
jumpsTo label = everything (+) (0 `mkQ` count)
    where count (Jump l) | l == label = 1
                         | otherwise = 0
          count _ = 0
          
withoutFinalJumpTo label [] = []
withoutFinalJumpTo label ss =
        case final of
           (Ctx _ (Jump l)) | l == label -> initial
                            | otherwise -> ss
           (Ctx c (Compound ss')) -> initial ++ [(Ctx c (Compound (withoutFinalJumpTo label ss')))]
           (Ctx c (If expr s0 s1)) -> initial ++ [Ctx c (If expr (rmvj s0) (rmvj s1))]
           (Ctx c s) -> ss
    where final = last ss
          initial = init ss
          rmvj s@(Jump l) | l == label = NullStmt
                          | otherwise  = s
          rmvj (Compound ss)           = Compound (withoutFinalJumpTo label ss)
          rmvj s                       = s
          
-- an explicit dictionary (could create a class for this, but seems unnecessary)
data ScopeFuncs m = ScopeFuncs { sfPushFrame :: m (), sfPopFrame :: m (), sfPushVar :: String -> m (), sfVars :: m [String] }

type NamesState = State.State [[String]]

nameStateScopeFuncs :: ScopeFuncs NamesState
nameStateScopeFuncs = ScopeFuncs pushFrame popFrame pushVar (get >>= return . concat)
     
pushFrame :: NamesState ()
pushFrame = get >>= put . ([]:)
popFrame :: NamesState ()
popFrame = get >>= ( \ s -> if null s then error "stack empty: cannot pop frame" else put (tail s))
pushVar v = do
    st <- get
    case st of
       [] -> error "stack empty: cannot add variable"
       (f:fs) -> put ((v:f):fs)  

sccsPurity :: M.Map String Expr -> M.Map String FunctionFacts -> [SCC (Ctx Func)] -> M.Map String FunctionFacts
sccsPurity gcs ff = foldl' (sccPurity gcs) ff

sccPurity :: M.Map String Expr -> M.Map String FunctionFacts -> SCC (Ctx Func) -> M.Map String FunctionFacts
sccPurity gcs ff scc =
       case scc of
           AcyclicSCC f -> go [f]
           CyclicSCC fs -> go fs
   where go fs = ff `M.union` ( M.fromList $ map ( \ f -> (fname f, FunctionFacts purity)) fs)
             where purity = not $ or $ map (isImpure (M.keysSet gcs) ff) fs
   
stmtIn sfs s@(Compound _) = (sfPushFrame sfs) >> return s
stmtIn _ s = return s
funcDecIn sfs fd@(Func (FuncDec _ _ parms) _) = (sfPushFrame sfs) >> mapM_ (\ cv -> (sfPushVar sfs) $ (varName . ctxItem) cv) parms >> return fd
stmtOut sfs s@(Compound _) = (sfPopFrame sfs) >> return s
stmtOut sfs s@(Decl v _) = ((sfPushVar sfs) $ varName v) >> return s
stmtOut _ s = return s
handlerDecIn sfs h@(Handler _ parms _) = (sfPushFrame sfs) >> mapM_ (\ cv -> (sfPushVar sfs) $ (varName . ctxItem) cv) parms >> return h
handlerDecOut sfs h@(Handler _ _ _) = (sfPopFrame sfs) >> return h
funcDecOut sfs f@(Func (FuncDec _ _ _) _) = (sfPopFrame sfs) >> return f

cvt :: Monad m => (a -> m a) -> b -> a -> m b
cvt f v x = f x >> return v

isImpure :: Set.Set String -> M.Map String FunctionFacts -> Ctx Func -> Bool
isImpure consts ff f =
        evalState (go f) []
    where go :: Ctx Func -> NamesState Bool
          go = everythingTwice (liftM2 (||)) (return False `mkQ` cvt (funcDecIn nameStateScopeFuncs) False `extQ` 
                                              cvt (stmtIn nameStateScopeFuncs) False `extQ` call `extQ` ref nameStateScopeFuncs)
                                             (return False `mkQ` cvt (stmtOut nameStateScopeFuncs) False)
          call c@(Call nm _) = do
              case M.lookup (ctxItem nm) ff of
                  Just (FunctionFacts { isPureFunction = False }) -> return True
                  _ -> return False
          call e = return False
          ref:: ScopeFuncs NamesState -> (Ctx String, Component) -> NamesState Bool
          ref sfs v@(Ctx _ nm,_) = do 
              locals <- sfVars sfs
              return (nm `notElem` locals && (not $ isConst nm))
          isConst nm = (nm `Set.member` consts) || (nm `elem` map constName allConstants)
          
isConstant :: Data a => String -> [a] -> Bool
isConstant s xs = not $ evalState (isntConstantM s xs) []
        
isntConstantM :: (Data a) => String -> [a] -> NamesState Bool
isntConstantM s = everythingTwice (liftM2 (||)) (return False `mkQ` cvt (funcDecIn sfs) False `extQ` 
                                                 cvt (stmtIn sfs) False `extQ` modified `extQ` cvt (handlerDecIn sfs) False)
                                                (return False `mkQ` cvt (funcDecOut sfs) False `extQ` 
                                                 cvt (handlerDecOut sfs) False `extQ` cvt (stmtOut sfs) False)
    where sfs = nameStateScopeFuncs
          checkNm nm = (sfVars sfs) >>= return . ((nm == s) &&) . (notElem nm)
          modified (Set (Ctx _ nm,_) _)   = checkNm nm
          modified (IncBy (Ctx _ nm,_) _) = checkNm nm
          modified (DecBy (Ctx _ nm,_) _) = checkNm nm
          modified (MulBy (Ctx _ nm,_) _) = checkNm nm
          modified (DivBy (Ctx _ nm,_) _) = checkNm nm
          modified (ModBy (Ctx _ nm,_) _) = checkNm nm
          modified (PreDec (Ctx _ nm,_))  = checkNm nm
          modified (PreInc (Ctx _ nm,_))  = checkNm nm
          modified (PostDec (Ctx _ nm,_)) = checkNm nm
          modified (PostInc (Ctx _ nm,_)) = checkNm nm
          modified _ = return False

reachableGlobs gs fs ss = [ g | g@(GDecl (Var nm _) _) <- gs, (nm `isUsedIn` fs || nm `isUsedIn` ss)]

isUsedIn :: Data a => String -> a -> Bool
isUsedIn s v = 
    evalState
        (everythingTwice (liftM2 (||)) (return False `mkQ` cvt (funcDecIn sfs) False `extQ`
                                        cvt (stmtIn sfs) False `extQ` used `extQ` cvt (handlerDecIn sfs) False)
                                       (return False `mkQ` cvt (funcDecOut sfs) False `extQ`
                                        cvt (handlerDecOut sfs) False `extQ` cvt (stmtOut sfs) False) v) []
    where sfs = nameStateScopeFuncs
          used :: (Ctx String,Component) -> NamesState Bool
          used (Ctx _ nm,_) = (sfVars sfs) >>= return . ((nm == s) &&) . (notElem nm)
          
globalConstants :: [Global] -> [Ctx Func] -> [State] -> M.Map String Expr
globalConstants gs fs ss =
        foldl globalConstant M.empty gs
    where globalConstant m (GDecl (Var nm t) mexpr) = if isAConst nm then M.insert nm (mexpr2expr m t mexpr) m else m
          isAConst nm = isConstant nm fs && isConstant nm ss
          expr2expr :: M.Map String Expr -> Expr -> Expr
          expr2expr m = everywhere (mkT go)
              where go e@(Get (nm,All)) = case M.lookup (ctxItem nm) m of
                      Nothing -> e
                      Just e' -> e'
                    go e@(Neg (Ctx _ (IntLit i))) = (IntLit (-i))
                    go e@(Neg (Ctx _ (FloatLit f))) = (FloatLit (-f))
                    go e = e
          mexpr2expr m _ (Just expr) = expr2expr m expr
          mexpr2expr m LLFloat Nothing = FloatLit 0
          mexpr2expr m LLInteger Nothing = IntLit 0
          mexpr2expr m LLString Nothing = StringLit ""
          mexpr2expr m LLList Nothing = ListExpr []
          mexpr2expr m LLVector Nothing = VecExpr (nullCtx $ FloatLit 0) (nullCtx $ FloatLit 0) (nullCtx $ FloatLit 0)
          mexpr2expr m LLRot Nothing = RotExpr (nullCtx $ FloatLit 0) (nullCtx $ FloatLit 0) (nullCtx $ FloatLit 0) (nullCtx $ FloatLit 1)
          mexpr2expr m LLKey Nothing = KeyLit ""
          mexpr2expr m LLVoid Nothing = error "somehow, an expression of type void?"

bb2int :: (a -> a -> Bool) -> a -> a -> Int
bb2int op x y = if op x y then 1 else 0

fromBool :: Num a => Bool -> a
fromBool x = if x then 1 else 0

liftBool :: Monad m => Bool -> m Expr
liftBool b = return (IntLit (fromBool b))

data SimplificationInfo = SimplificationInfo {
      siScript :: !CompiledLSLScript,
      siPureFuncs :: !(Set.Set String),
      siConstants :: !(M.Map String Expr),
      siLocalsInScope :: ![[String]]
    }

simpInfoScopeFuncs = ScopeFuncs {
        sfPushFrame = get >>= \ si -> put si { siLocalsInScope = [] : (siLocalsInScope si) },
        sfPopFrame = do
            si <- get
            case siLocalsInScope si of
                [] -> error "stack empty, cannot pop frame"
                (f:fs) -> put si { siLocalsInScope = fs },
        sfPushVar = (\ s -> do
             si <- get
             case siLocalsInScope si of
                 [] -> error "stack empty, cannot push variable"
                 (f:fs) -> put si { siLocalsInScope = ((s:f):fs) }),
        sfVars = get >>= return . concat . siLocalsInScope
    }
    
type SimpState a = State.State SimplificationInfo a

floatToLit :: RealFloat a => a -> Expr
floatToLit = FloatLit . realToFrac

valToExpr :: RealFloat a => LSLValue a -> Expr
valToExpr (IVal i) = IntLit i
valToExpr (FVal f) = floatToLit f
valToExpr (SVal s) = StringLit s
valToExpr (KVal k) = KeyLit k
valToExpr (VVal x y z) = VecExpr (nullCtx $ floatToLit x) (nullCtx $ floatToLit y) (nullCtx $ floatToLit z)
valToExpr (RVal x y z s) = RotExpr (nullCtx $ floatToLit x) (nullCtx $ floatToLit y) (nullCtx $ floatToLit z) (nullCtx $ floatToLit s)
valToExpr (LVal l) = ListExpr (map (nullCtx . valToExpr) l)
valToExpr VoidVal = error "can't convert the void value to an expression"

predefToLit :: String -> Maybe Expr
predefToLit s = fmap valToExpr (findConstVal s)

constVarToLit :: M.Map String Expr -> String -> Maybe Expr
constVarToLit m s = M.lookup s m

nameToLit :: M.Map String Expr -> String -> Maybe Expr
nameToLit m s = predefToLit s `mplus` constVarToLit m s

exprsToVals :: [Ctx Expr] -> Maybe [LSLValue Double]
exprsToVals es = mapM exprToVal es
    where exprToVal :: Ctx Expr -> Maybe (LSLValue Double)
          exprToVal (Ctx _ (IntLit i)) = Just (IVal i)
          exprToVal (Ctx _ (FloatLit f)) = Just (FVal f)
          exprToVal (Ctx _ (StringLit s)) = Just (SVal s)
          exprToVal (Ctx _ (KeyLit k)) = Just (KVal k)
          exprToVal (Ctx _ (VecExpr ex ey ez)) = 
              case (exprToVal ex, exprToVal ey, exprToVal ez) of
                  (Just (FVal x),Just (FVal y),Just (FVal z)) -> Just (VVal x y z)
                  _ -> Nothing
          exprToVal (Ctx _ (RotExpr ex ey ez es)) = 
              case (exprToVal ex, exprToVal ey, exprToVal ez, exprToVal es) of
                  (Just (FVal x),Just (FVal y),Just (FVal z),Just (FVal s)) -> Just (RVal x y z s)
                  _ -> Nothing
          exprToVal (Ctx _ (ListExpr es)) = case exprsToVals es of
              Nothing -> Nothing
              Just vs -> Just $ LVal vs
          exprToVal _ = Nothing
              
simplifyE :: Expr -> SimpState Expr
simplifyE (Neg (Ctx _ (IntLit i))) = return (IntLit (-i))
simplifyE (Not (Ctx _ (IntLit i))) = return (IntLit (fromBool (i == 0)))
simplifyE (Add (Ctx _ (IntLit i)) (Ctx _ (IntLit j))) = return (IntLit (i + j))
simplifyE (Mul (Ctx _ (IntLit i)) (Ctx _ (IntLit j))) = return (IntLit (i * j))
simplifyE (Sub (Ctx _ (IntLit i)) (Ctx _ (IntLit j))) = return (IntLit (i - j))
simplifyE (And (Ctx _ (IntLit i)) (Ctx _ (IntLit j))) = return (IntLit (fromBool (i /= 0 && j /= 0)))
simplifyE (Or (Ctx _ (IntLit i)) (Ctx _ (IntLit j)))  = return (IntLit (fromBool (i /= 0 || j /= 0)))
simplifyE (Lt (Ctx _ (IntLit i)) (Ctx _ (IntLit j)))  = return (IntLit (bb2int (<) i j))
simplifyE (Gt (Ctx _ (IntLit i)) (Ctx _ (IntLit j)))  = return (IntLit (bb2int (>) i j))
simplifyE (Ge (Ctx _ (IntLit i)) (Ctx _ (IntLit j)))  = return (IntLit (bb2int (>=) i j))
simplifyE (Le (Ctx _ (IntLit i)) (Ctx _ (IntLit j)))  = return (IntLit (bb2int (<=) i j))
simplifyE (Equal (Ctx _ (IntLit i)) (Ctx _ (IntLit j)))  = return (IntLit (bb2int (==) i j))
simplifyE (NotEqual (Ctx _ (IntLit i)) (Ctx _ (IntLit j)))  = return (IntLit (bb2int (==) i j))
simplifyE (BAnd (Ctx _ (IntLit i)) (Ctx _ (IntLit j)))  = return (IntLit (i .&. j))
simplifyE (BOr (Ctx _ (IntLit i)) (Ctx _ (IntLit j)))  = return (IntLit (i .|. j))
simplifyE (Xor (Ctx _ (IntLit i)) (Ctx _ (IntLit j)))  = return (IntLit (i `xor` j))
simplifyE (ShiftL (Ctx _ (IntLit i)) (Ctx _ (IntLit j)))  = return (IntLit (i `shiftL` j))
simplifyE (ShiftR (Ctx _ (IntLit i)) (Ctx _ (IntLit j)))  = return (IntLit (i `shiftR` j))
simplifyE e@(Div (Ctx _ (IntLit i)) (Ctx _ (IntLit j))) | j /= 0 = return (IntLit ( i `div` j))
                                                        | otherwise = return e
simplifyE e@(Mod (Ctx _ (IntLit i)) (Ctx _ (IntLit j))) | j /= 0 = return (IntLit ( i `mod` j))
                                                        | otherwise = return e
simplifyE (Neg (Ctx _ (FloatLit i))) = return (FloatLit (-i))
simplifyE (Add (Ctx _ (FloatLit i)) (Ctx _ (FloatLit j))) = return (FloatLit (i + j))
simplifyE (Mul (Ctx _ (FloatLit i)) (Ctx _ (FloatLit j))) = return (FloatLit (i * j))
simplifyE (Sub (Ctx _ (FloatLit i)) (Ctx _ (FloatLit j))) = return (FloatLit (i - j))
simplifyE (Lt (Ctx _ (FloatLit i)) (Ctx _ (FloatLit j)))  = return (IntLit (bb2int (<) i j))
simplifyE (Gt (Ctx _ (FloatLit i)) (Ctx _ (FloatLit j)))  = return (IntLit (bb2int (>) i j))
simplifyE (Ge (Ctx _ (FloatLit i)) (Ctx _ (FloatLit j)))  = return (IntLit (bb2int (>=) i j))
simplifyE (Le (Ctx _ (FloatLit i)) (Ctx _ (FloatLit j)))  = return (IntLit (bb2int (<=) i j))
simplifyE (Equal (Ctx _ (FloatLit i)) (Ctx _ (FloatLit j)))  = return (IntLit (bb2int (==) i j))
simplifyE (NotEqual (Ctx _ (FloatLit i)) (Ctx _ (FloatLit j)))  = return (IntLit (bb2int (==) i j))
simplifyE e@(Div (Ctx _ (FloatLit i)) (Ctx _ (FloatLit j))) | j /= 0 = return (FloatLit ( i / j))
                                                            | otherwise = return e
simplifyE (Add (Ctx _ (IntLit i)) (Ctx _ (FloatLit j))) = return (FloatLit (fromIntegral i + j))
simplifyE (Mul (Ctx _ (IntLit i)) (Ctx _ (FloatLit j))) = return (FloatLit (fromIntegral i * j))
simplifyE (Sub (Ctx _ (IntLit i)) (Ctx _ (FloatLit j))) = return (FloatLit (fromIntegral i - j))
simplifyE e@(Div (Ctx _ (IntLit i)) (Ctx _ (FloatLit j))) | j /= 0 = return (FloatLit ( fromIntegral i / j))
                                                          | otherwise = return e
simplifyE (Equal (Ctx _ (IntLit i)) (Ctx _ (FloatLit j))) = return (IntLit (if fromIntegral i == j then 1 else 0))
simplifyE (NotEqual (Ctx _ (IntLit i)) (Ctx _ (FloatLit j))) = return (IntLit (if fromIntegral i == j then 0 else 1))
simplifyE (Lt (Ctx _ (IntLit i)) (Ctx _ (FloatLit j))) = return (IntLit (if fromIntegral i < j then 1 else 0))
simplifyE (Gt (Ctx _ (IntLit i)) (Ctx _ (FloatLit j))) = return (IntLit (if fromIntegral i > j then 1 else 0))
simplifyE (Le (Ctx _ (IntLit i)) (Ctx _ (FloatLit j))) = return (IntLit (if fromIntegral i <= j then 1 else 0))
simplifyE (Ge (Ctx _ (IntLit i)) (Ctx _ (FloatLit j))) = return (IntLit (if fromIntegral i >= j then 1 else 0))
simplifyE (Add (Ctx _ (FloatLit i)) (Ctx _ (IntLit j))) = return (FloatLit (i + fromIntegral j))
simplifyE (Mul (Ctx _ (FloatLit i)) (Ctx _ (IntLit j))) = return (FloatLit (i * fromIntegral j))
simplifyE (Sub (Ctx _ (FloatLit i)) (Ctx _ (IntLit j))) = return (FloatLit (i - fromIntegral j))
simplifyE e@(Div (Ctx _ (FloatLit i)) (Ctx _ (IntLit j))) | j /= 0 = return (FloatLit ( i / fromIntegral j))
                                                          | otherwise = return e
simplifyE (Equal (Ctx _ (FloatLit i)) (Ctx _ (IntLit j))) = return (IntLit (if i == fromIntegral j then 1 else 0))
simplifyE (NotEqual (Ctx _ (FloatLit i)) (Ctx _ (IntLit j))) = return (IntLit (if i == fromIntegral j then 0 else 1))
simplifyE (Lt (Ctx _ (FloatLit i)) (Ctx _ (IntLit j))) = return (IntLit (if i < fromIntegral j then 1 else 0))
simplifyE (Gt (Ctx _ (FloatLit i)) (Ctx _ (IntLit j))) = return (IntLit (if i > fromIntegral j then 1 else 0))
simplifyE (Le (Ctx _ (FloatLit i)) (Ctx _ (IntLit j))) = return (IntLit (if i <= fromIntegral j then 1 else 0))
simplifyE (Ge (Ctx _ (FloatLit i)) (Ctx _ (IntLit j))) = return (IntLit (if i >= fromIntegral j then 1 else 0))
simplifyE e@(Get (nm,c)) = do
       locals <- get >>= return . concat . siLocalsInScope
       if name `elem` locals 
           then return e
           else newExpr
    where name = ctxItem nm
          newExpr = do
            m <- get >>= return . siConstants
            return $ case nameToLit m name of
                Nothing -> e
                Just e' -> case (c,e') of
                    (All,_) -> e'
                    (X,VecExpr x _ _) -> ctxItem x
                    (X,RotExpr x _ _ _) -> ctxItem x
                    (Y,VecExpr _ y _) -> ctxItem y
                    (Y,RotExpr _ y _ _) -> ctxItem y
                    (Z,VecExpr _ _ z) -> ctxItem z
                    (Z,RotExpr _ _ z _) -> ctxItem z
                    (S,RotExpr _ _ _ s) -> ctxItem s
                    _ -> e
simplifyE e@(Call (Ctx _ nm) exprs) =
    case exprsToVals exprs of
        Nothing -> return e
        Just vs -> 
            case lookup nm internalLLFuncs of
                Just f -> return (valToExpr $ snd (Id.runIdentity (f () vs)))
                Nothing -> do
                    pureFuncs <- get >>= return . siPureFuncs
                    script <- get >>= return . siScript
                    if nm `Set.member` pureFuncs
                        then case simSFunc (script,[nm]) [] vs of
                            Left _ -> return e
                            Right (VoidVal,_) -> return e
                            Right (v,_) -> return $ valToExpr v
                        else return e
simplifyE e = return e
  
simplify :: Data a => CompiledLSLScript -> Set.Set String -> M.Map String Expr -> a -> a
simplify script pureFuncs gcs v =
        evalState (go v) (SimplificationInfo script pureFuncs gcs [])
    where go :: Data a => a -> SimpState a
          go = downup (mkM (stmtIn simpInfoScopeFuncs) `extM` funcDecIn simpInfoScopeFuncs `extM` handlerDecIn simpInfoScopeFuncs)
                      (mkM simplifyE `extM` stmtOut simpInfoScopeFuncs `extM` 
                       funcDecOut simpInfoScopeFuncs `extM` handlerDecOut simpInfoScopeFuncs)