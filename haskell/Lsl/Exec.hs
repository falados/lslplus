module Lsl.Exec(
    ScriptImage(..),
    EvalState,
    ExecutionInfo(..),
    ExecutionState(..),
    FrameInfo(..),
    MemRegion,
    Binding,
    executeLsl,
    frameInfo,
    initLSLScript,
    initStateSimple,
    setupSimple,
    evalSimple,
    emptyMemRegion,
    runEval,
    scriptImage,
    softReset,
    hardReset) where

import Debug.Trace
import Data.Bits
import Data.List
import Lsl.Breakpoint hiding (checkBreakpoint)
import Lsl.CodeHelper
import Lsl.Log
import Lsl.Util
import Lsl.Structure
import Lsl.Type
import Lsl.Key
import Lsl.Evaluation
import Lsl.Constants
import Control.Monad
import Control.Monad.State hiding (State)
import Control.Monad.Error

-- initialize a script for execution
initLSLScript :: CompiledLSLScript -> ScriptImage
initLSLScript (globals,fs,ss)  =
    ScriptImage {
        scriptImageName = "",
        curState = "default",
        executionState = Waiting,
        glob = initGlobals globals,
        funcs = fs,
        states = ss,
        valueStack = [],
        callStack = [],
        stepManager = emptyStepManager,
        globals = globals,
        currentEvent = Nothing }

initStateSimple script perfAction log qtick utick chkBp =
    EvalState { scriptImage = initLSLScript script,
                objectId = nextKey nullKey,
                primId = 0,
                scriptName = "script",
                myPrimKey = nextKey nullKey,
                performAction = perfAction,
                logMessage = log,
                qwtick = qtick,
                uwtick = utick,
                checkBreakpoint = chkBp,
                nextEvent = undefined }

runEval = (runStateT . runErrorT)
executeLsl img oid pid sid pkey perfAction log qtick utick chkBp queue maxTick =
     do let state = (EvalState { scriptImage = img,
                            objectId = oid,
                            primId = pid,
                            scriptName = sid,
                            myPrimKey = pkey,
                            performAction = perfAction,
                            logMessage = log,
                            qwtick = qtick,
                            uwtick = utick,
                            checkBreakpoint = chkBp,
                            nextEvent = undefined})
        result <- (runEval $ evalScript maxTick queue) state
        case result of
            (Left s,_) -> return $ Left s
            (Right queue',evalState) -> return $ Right $ (scriptImage evalState, queue')

-- The state of evaluation for a script.
data EvalState m = EvalState {
     scriptImage :: ScriptImage,
     objectId :: String,
     primId :: Int,
     scriptName :: String,
     myPrimKey :: String,
     performAction :: String -> ScriptInfo -> [LSLValue] -> m (EvalResult,LSLValue),
     logMessage :: String -> m (),
     qwtick :: m Int,
     uwtick :: Int -> m (),
     checkBreakpoint :: Breakpoint -> StepManager -> m (Bool, StepManager),
     nextEvent :: String -> String -> m (Maybe Event)  }

type Eval m = ErrorT String (StateT (EvalState m) m)

evalT :: Monad m => ((EvalState m) -> (a,EvalState m)) -> Eval m a
evalT v = lift ((\ f -> StateT (\ s -> return (f s))) v)

-- the image, or 'memory state' of a running script.  Includes the
-- status of all stacks, global variables, and immutable items like
-- the functions and state definitions.
data ScriptImage = ScriptImage {
                     scriptImageName :: String,
                     curState :: StateName,
                     executionState :: ExecutionState,
                     glob :: MemRegion,
                     funcs :: [Func],
                     states :: [State],
                     valueStack :: ValueStack,
                     callStack :: CallStack,
                     stepManager :: StepManager,
                     globals :: [Global],
                     currentEvent :: Maybe Event
                 } deriving (Show)

data FrameInfo = FrameInfo { frameInfoImageName :: String, frameInfoFrames :: [(String,SourceContext,Maybe Int,[(String,LSLValue)])] }

frameInfo scriptImage = FrameInfo (scriptImageName scriptImage) $
    frames ++ [("glob", bottomContext, Nothing, glob scriptImage)]
    where frames = (map collapseFrame $ callStack scriptImage)
          (bottomContext,bottomLine) = case frames of
              [] -> (UnknownSourceContext,Nothing)
              _ -> let (_,ctx,_,_) = last frames in (ctx,Just 1)
          collapseFrame (Frame name ctx line (ss,_)) =
              (name,ctx,line,concat $ map fst ss)
-- a soft reset occurs when a script that has been running, but
-- has been persisted to inventory, is reactivated.  The curState
-- stays the same, but if the script was running or sleeping, the
-- script goes to the 'waiting' state, with its call stack and value
-- stack cleared.

-- if a script was halted, it stays halted; if it was waiting, it
-- stays waiting; if it was erroneous, it stays erroneous
softReset image =
   let image' = image { valueStack = [], callStack = [] } in
       case executionState image' of
           SleepingTil _ -> image' { executionState = Waiting }
           Executing -> image' { executionState = Waiting }
           _ -> image'

-- a hard reset occurs when a script is explicitly reset, either
-- via an LSL call, or 'manually' (in the real SL, via the GUI)
hardReset image = 
    case executionState image of
        Erroneous _ -> image -- no effect
        Halted -> image -- halted scripts are scripts that are explicitly not in the run state
        _ -> image {
            curState = "default",
            executionState = Waiting,
            glob = initGlobals (globals image),
            valueStack = [],
            callStack = [] }
            
type StateName = String

-- a labeled block: a label and all the statements that follow it.
data LBlock = LBlock String [Ctx Statement]
    deriving (Show)

-- find a block in a list of labeled blocks
findLBlock name = find (\ (LBlock n _) -> name == n)

-- label the blocks in a list of statements `(i.e.
-- (EXAMPLE:
--        { 
--           @foo;
--           if (i == 7) jump bar;
--           @bar;
--           llSay(0, (string) (i + j));
--           if (j == 10) jump foo;
--        }
--  In the above code, there are two labels, and there for two labelled
--  blocks.  The first is 'foo', labelling the block:
--           if (i == 7) jump bar;
--           @bar;
--           llSay(0, (string) (i + j));
--           if (j == 10) jump foo;
--
--  The 2nd is 'bar', labelling the block:
--           llSay(0, (string) (i + j));
--           if (j == 10) jump foo;
--  
labelBlocks [] = []
-- labelBlocks ((Label s):stmts) = (LBlock s stmts):(labelBlocks stmts)
-- labelBlocks (_:stmts) = (labelBlocks stmts)
labelBlocks (ctxStmt:stmts) =
    case ctxItem ctxStmt of
        Label s -> (LBlock s stmts):(labelBlocks stmts)
        _ -> (labelBlocks stmts)
        
-- A name/element pair is the basic unit of memory.  Each memory 
-- location has a name (rather than an address) and can hold a value
-- which can be of any LSL type.  So 1 memory location can hold a
-- vector, or a string, or a list, etc.
type Binding = (String,LSLValue)

-- a memory region is just a collection of NameElementPairs
type MemRegion = [Binding]
emptyMemRegion = []

initVar name LLInteger Nothing = (name,IVal 0)
initVar name LLFloat Nothing  = (name,FVal 0.0)
initVar name LLString Nothing = (name,SVal "")
initVar name LLKey Nothing = (name,SVal "")
initVar name LLList Nothing = (name,LVal [])
initVar name LLVector Nothing = (name,VVal 0.0 0.0 0.0)
initVar name LLRot Nothing = (name,RVal 0.0 0.0 0.0 1.0)
initVar name _ (Just v) = (name,v)

writeMem :: Monad m => String -> LSLValue -> MemRegion -> m MemRegion
writeMem name value cells = 
  case break (\(name',element) -> name' == name) cells of
      (cells',[]) -> fail "no such variable"
      (xs,y:ys) -> return ((name,value):(xs ++ ys))

readMem :: Monad m => String -> MemRegion -> m LSLValue
readMem = lookupM

goodEvents = goodHandlers

-- validEvent :: Event -> Validity ()
-- validEvent (Event name values) =
--     do expectedTypes <- incontext ("looking for event named: " ++ name) $ lookupM name goodEvents
--        let valueTypes = map typeOfLSLValue values
--        when (valueTypes /= expectedTypes) $ fail ("invalid types for event " ++ name)

initGlobals :: [Global] -> MemRegion
initGlobals globals = map (initGlobal globals) globals
 
initGlobal globals (GDecl (Var name t) mexpr) = initVar name t $ fmap (evalLit globals) mexpr

evalCtxLit globals (Ctx _ expr) = evalLit globals expr
       
evalLit globals expr =
    let litExpr2Float expr =
            case evalCtxLit globals expr of
                (FVal f) -> f
                (IVal i) -> fromInteger $ toInteger i
                _ -> error "invalid float expression"
    in case expr of
        IntLit i        -> IVal i
        FloatLit f      -> FVal f
        StringLit s     -> SVal s
        KeyLit k        -> KVal k
        ListExpr l      -> LVal $ map (evalCtxLit globals) l
        VecExpr a b c   -> VVal (litExpr2Float a) (litExpr2Float b) (litExpr2Float c)
        RotExpr a b c d -> RVal (litExpr2Float a) (litExpr2Float b) (litExpr2Float c) (litExpr2Float d)
        Get (Ctx _ nm,All)    -> 
            case find (\ (GDecl (Var nm' t) mexpr') -> nm == nm') globals of
                Nothing -> error ("invalid global " ++ nm ++ " referenced in initializer")
                Just g ->
                    let (_,v) = initGlobal globals g in v

bindParm (Var name t) lslVal = if typeOfLSLValue lslVal == t then return (name,lslVal) else fail "type mismatch!"
bindParms vars vals = zipWithM bindParm vars vals

bindParmForgiving (Var name t) lslVal =
    case (t,typeOfLSLValue lslVal) of
        (LLInteger,LLFloat) -> let FVal v = lslVal in return (name,IVal $ floor v)
        (LLFloat,LLInteger) -> let IVal v = lslVal in return (name,FVal $ fromInt v)
        (LLKey,LLString) -> let SVal s = lslVal in return (name,KVal s)
        (LLString,LLKey) -> let KVal k = lslVal in return (name,SVal k)
        (t0,t1) | t0 == t1 -> return (name,lslVal)
                | otherwise -> fail "type mismatch!"

bindParmsForgiving vars vals = zipWithM bindParmForgiving vars vals

toBool x = if x == 0 then False else True

--type Frame = (ScopeStack,EvalStack)
data Frame = Frame { frameName :: String, frameContext :: SourceContext, frameSourceLine :: Maybe Int,
                     frameStacks :: (ScopeStack, EvalStack) }
     deriving (Show)
type LabelSet = [LBlock]
type Scope = (MemRegion,LabelSet)
type ScopeStack = [Scope]
type CallStack = [Frame]


readVarScope :: String -> Scope -> Maybe LSLValue
readVarScope name (mem,_) = readMem name mem
readVarSStack :: String -> ScopeStack -> Maybe LSLValue
readVarSStack name ss = foldl mplus Nothing $ map (readVarScope name) ss
readVarFrame :: String -> Frame -> Maybe LSLValue
readVarFrame name = (readVarSStack name) . fst . frameStacks
readVarCallStack :: String -> CallStack -> Maybe LSLValue
readVarCallStack name = (readVarFrame name) . head

writeVarScope :: String -> LSLValue -> Scope -> Maybe Scope
writeVarScope name val (mem,l) = writeMem name val mem >>= return . (flip (,) l)
writeVarSStack rs name val [] = Nothing
writeVarSStack rs name val (s:ss) =
    case writeVarScope name val s of
        Nothing -> writeVarSStack (s:rs) name val ss
        Just s' -> Just $ (reverse rs) ++ (s':ss)
writeVarFrame name val frame =
    let (ss,es) = frameStacks frame in
    case writeVarSStack [] name val ss of
        Nothing -> Nothing
        Just ss' -> Just frame { frameStacks = (ss',es) }
writeVarCallStack name val (frame:cs) = writeVarFrame name val frame >>= return . (flip (:) cs)

type EvalStack = [EvalElement]

type ValueStack = [LSLValue]

data EvalElement = EvBlock [Ctx Statement] | EvCtxStatement (Ctx Statement)
                 | EvStatement Statement | EvExpr Expr | EvMexpr (Maybe Expr)
                 | EvAdd | EvSub | EvMul | EvDiv | EvMod | EvBAnd | EvBOr | EvBXor | EvBInv | EvNeg
                 | EvNot | EvAnd | EvOr  | EvLe  | EvLt  | EvGe   | EvGt  | EvEq | EvNe
                 | EvShiftL | EvShiftR | EvCast LSLType | EvGet (String,Component) | EvSet (String,Component)
                 | EvCons | EvMkVec | EvMkRot | EvPop
                 | EvReturn | EvDiscard | EvBind String LSLType
                 | EvCond Statement Statement | EvCall String SourceContext [Var] [Ctx Statement] Bool
                 | EvPredef String | EvLoop Expr (Maybe Expr) Statement
    deriving (Show)
    
-- Note: lifted from Hudak, p.273
queryState :: Monad w => (EvalState w -> a) -> Eval w a
queryState q = evalT (\s -> (q s, s))
updateState :: Monad w => (EvalState w -> EvalState w) -> Eval w ()
updateState u = evalT (\s -> ((), u s))
queryExState q = queryState (q . scriptImage)
updateExState :: Monad w => (ScriptImage -> ScriptImage) -> Eval w ()
updateExState u = updateState (\s -> s { scriptImage = u $ scriptImage s })

getTick :: Monad w => Eval w Int
getTick = unwrap $ evalT (\s -> (lift $ lift $ qwtick s,s))
setTick v = do f <- (queryState uwtick) 
               lift $ lift $ f v
getNextEvent key name =  do f <- (queryState nextEvent)
                            lift $ lift $  f key name
doAction name scriptInfo args = do perform <- (queryState performAction)
                                   lift $ lift $ perform name scriptInfo args
logMsg s = do log <- queryState logMessage
              lift $ lift $ log s

checkBp bp = do sm <- getStepManager
                chk <- queryState checkBreakpoint
                (result,sm') <- lift $ lift $ chk bp sm
                setStepManager sm'
                return result
               
getGlob :: Monad w => Eval w MemRegion
getGlob = queryExState glob
getFuncs :: Monad w => Eval w [Func]
getFuncs = queryExState funcs
getVStack :: Monad w => Eval w ValueStack
getVStack = queryExState valueStack
getCallStack :: Monad w => Eval w CallStack
getCallStack = queryExState callStack
getStates :: Monad w => Eval w [State]
getStates = queryExState states
getCurrentEvent :: Monad w => Eval w (Maybe Event)
getCurrentEvent = queryExState currentEvent
getExecutionState :: Monad w => Eval w ExecutionState
getExecutionState = queryExState executionState
getCurState :: Monad w => Eval w String
getCurState = queryExState curState
getEvalState :: Monad w => Eval w (EvalState w)
getEvalState = queryState id
getObjectId :: Monad w => Eval w String
getObjectId = queryState objectId
getPrimId :: Monad w => Eval w Int
getPrimId = queryState primId
getScriptName :: Monad w => Eval w String
getScriptName = queryState scriptName
getScriptImageName :: Monad w => Eval w String
getScriptImageName = queryExState scriptImageName
getMyPrimKey :: Monad w => Eval w String
getMyPrimKey = queryState myPrimKey
getStepManager :: Monad w => Eval w StepManager
getStepManager = queryExState stepManager

setGlob g = updateExState (\e -> e { glob = g })
setVStack v = updateExState (\e -> e { valueStack = v })
setCallStack c = updateExState (\e -> e { callStack = c })
setStepManager m = updateExState (\e -> e { stepManager = m })
setExecutionState state = updateExState (\e -> e { executionState = state })
setCurState state = updateExState (\e -> e { curState = state })
setCurrentEvent event = updateExState (\e -> e { currentEvent = Just event })
setScriptImageName n = updateExState (\ e -> e { scriptImageName = n })

initStacks :: Monad w => Eval w ()
initStacks = 
    do setVStack []
       setCallStack []

popScope :: Monad w => Eval w Scope
popScope =
    do --((s:ss,es):cs) <- getCallStack
       (frame:frames) <- getCallStack
       let (s:ss,es) = frameStacks frame
       setCallStack (frame { frameStacks = (ss,es) }:frames)
       return s

pushScope :: Monad w => MemRegion -> LabelSet -> Eval w ()
pushScope mem labels =
    do (frame:frames) <- getCallStack
       let (ss,es) = frameStacks frame
       setCallStack (frame { frameStacks = ((mem,labels):ss,es) }:frames)
        
pushVal value = 
    do vstack <- getVStack
       setVStack (value:vstack)

popVal :: Monad w => Eval w LSLValue
popVal =
    do vstack <- getVStack
       case vstack of
           [] -> fail "empty value stack"
           (v:vs) -> do
               setVStack vs
               return v

peekVal :: Monad w => Eval w LSLValue
peekVal =
    do vstack <- getVStack
       case vstack of
           [] -> fail "empty value stack"
           (v:_) -> return v
           
valStackEmpty :: Monad w => Eval w Bool
valStackEmpty = getVStack >>= (return . (==[]))

elementStackEmpty :: Monad w => Eval w Bool
elementStackEmpty = 
    do (frame:cs) <- getCallStack
       case frameStacks frame of
           (_,[]) -> return True
           _ -> return False
           
popElement :: Monad w => Eval w EvalElement
popElement =
    do (frame:frames) <- getCallStack
       let (ss,e:es) = frameStacks frame
       setCallStack (frame { frameStacks = (ss,es) }:frames)
       return e
       
popElements 0 = return ()
popElements n = 
    do popElement
       popElements (n - 1)

pushElement element =
   do (frame:frames) <- getCallStack
      let (ss,es) = frameStacks frame
      setCallStack (frame { frameStacks = (ss,element:es) }:frames)

pushElements elements =
  do mapM pushElement elements
     return EvalIncomplete

callStackEmpty :: Monad w => Eval w Bool
callStackEmpty = getCallStack >>= return . null

popFrame :: Monad w => Eval w ()           
popFrame =
    do (f:cs) <- getCallStack
       stepMgr <- getStepManager
       let stepMgr' = popStepManagerFrame stepMgr
       setStepManager stepMgr'
       setCallStack cs

--pushFrame :: Monad w => Eval w ()
pushFrame name ctx line =
    do stepMgr <- getStepManager
       let stepMgr' = pushStepManagerFrame stepMgr
       setStepManager stepMgr'
       cs <- getCallStack
       setCallStack (Frame { frameName = name, frameContext = ctx, frameSourceLine = line, frameStacks = ([],[])}:cs)
       
getFunc :: Monad w => String -> Eval w Func
getFunc name =
    do funcs <- getFuncs
       incontext ("func: " ++ name) $ findFunc name funcs

setVar :: Monad w => String -> LSLValue -> Eval w ()
setVar name val =
    do cs <- getCallStack
       case writeVarCallStack name val cs of
           Just cs' -> setCallStack cs'
           Nothing ->
               do glob <- getGlob
                  glob' <- incontext ("setting " ++ name ++ ":") $ writeMem name val glob
                  setGlob glob'
                 
getVar :: Monad w => String -> Eval w LSLValue
getVar name =
    do cs <- getCallStack
       glob <- getGlob
       case msum [findConstVal name, readVarCallStack name cs, readMem name glob] of
           Nothing -> fail ("no such variable " ++ name)
           Just val -> return val

initVar1 :: Monad w => String -> LSLType -> Maybe LSLValue -> Eval w ()
initVar1 name t mval = 
    do --(((m,l):ss,es):cs) <- getCallStack
       (frame:frames) <- getCallStack
       let ((m,l):ss,es) = frameStacks frame
       let frame' = frame { frameStacks = (((initVar name t mval):m,l):ss,es) }
       setCallStack (frame':frames)

initVars1 :: Monad w => [Var] -> [LSLValue] -> Eval w ()
initVars1 vars vals =
    foldM_ (\_ -> \ (Var n t, v) -> initVar1 n t $ Just v) () $ zip vars vals
    
unwindToLabel name =
    let f n =
            do (frame:frames) <- getCallStack
               let (ss,es) = frameStacks frame
               case ss of
                   [] -> fail ("label " ++ name ++ " not found")
                   ((m,l):ss') ->
                       case findLBlock name l of
                           Just (LBlock _ stmts) -> return $ (n,stmts)
                           Nothing ->
                               do setCallStack (frame { frameStacks = (ss',es) }:frames)
                                  f (n + 1)
    in f 1


data ExecutionState = Waiting | Executing | Halted | SleepingTil Int | Erroneous String | Crashed String | Suspended Breakpoint
    deriving (Show,Eq)

matchEvent (Event name _ _) [] = fail ("no such handler" ++ name)
matchEvent event@(Event name values _) ((Handler (Ctx ctx name') parms stmts):hs) 
        | name == name' = do mem <- bindParms (ctxItems parms) values
                             return (mem,stmts,name,ctx)
        | otherwise = matchEvent event hs

findHandler name handlers = ctx ("finding handler " ++ name) $ 
    findM (\ (Handler (Ctx _ name') _ _) -> name' == name) handlers


evalScriptSimple :: Monad w => Int -> [String] -> [Binding] -> [LSLValue] -> Eval w (EvalResult,Maybe LSLValue)
evalScriptSimple maxTick path globbindings args =
    do  setupSimple path globbindings args
        evalSimple maxTick
        
evalSimple maxTick =
    do  result <- eval maxTick
        glob <- getGlob
        case result of
            EvalComplete Nothing ->
                do empty <- valStackEmpty
                   if empty then return (result,Just VoidVal) else do
                       val <- popVal
                       return (result,Just val)
            _ -> return (result,Nothing)
            
setupSimple path globbindings args =
    do  setScriptImageName (concat (intersperse "." path))
        updateGlobals globbindings
        (params,stmts,ctx) <- getEntryPoint path
        mem <- bindParmsForgiving params args
        initStacks
        pushFrame (concat $ separateWith "." path) ctx Nothing
        pushScope mem $ labelBlocks stmts
        pushElement (EvBlock stmts)
        setExecutionState Executing
    where updateGlobals [] = return ()
          updateGlobals ((name,val):bs) =
              do glob <- getGlob
                 glob' <- writeMem name val glob
                 setGlob glob'
                 updateGlobals bs
          getEntryPoint [funcName] = 
              do funcs <- getFuncs
                 (Func (FuncDec name _ params) stmts) <- findFunc funcName funcs
                 return (ctxItems params,stmts,srcCtx name)
          getEntryPoint [stateName,handlerName] =
              do states <- getStates
                 (State _ handlers) <- findState stateName states
                 (Handler name params stmts) <- findHandler handlerName handlers
                 return (ctxItems params,stmts, srcCtx name)

incontext s f =
    case f of
        Left s -> fail s
        Right v -> return v                             

evalScript :: Monad w => Int -> [Event] -> Eval w [Event]
evalScript maxTick queue =
    do executionState <- getExecutionState
       case executionState of
           Suspended _ -> setExecutionState Executing >> evalScript maxTick queue
           Erroneous _ -> return queue
           Waiting ->
               do oid <- getObjectId
                  pid <- getPrimId
                  primKey <- getMyPrimKey
                  scriptName <- getScriptName
                  case queue of
                      [] -> return queue
                      (event:queue') ->
                         do states <- getStates
                            curState <- getCurState
                            (State _ handlers) <- incontext ("state " ++ curState ++ ":") $ findState curState states
                            case matchEvent event handlers of
                                Nothing -> return queue'
                                Just (mem,stmts,name,ctx) ->
                                    do  initStacks
                                        pushFrame name ctx Nothing
                                        pushScope mem $ labelBlocks stmts
                                        pushElement (EvBlock stmts)
                                        setExecutionState Executing
                                        setCurrentEvent event
                                        evalScript maxTick queue'
           Executing -> do result <- eval maxTick
                           case result of
                               EvalComplete (Just newState) -> 
                                   do setCurState newState
                                      setExecutionState Waiting
                               EvalComplete _ -> setExecutionState Waiting
                               YieldTil i -> setExecutionState $ SleepingTil i
                               BrokeAt bp -> setExecutionState (Suspended bp)
                               _ -> return ()
                           return queue
           SleepingTil i -> do tick <- getTick
                               if (tick >= i) 
                                   then do setExecutionState Executing
                                           evalScript maxTick queue
                                   else return queue
           Halted -> return queue

eval :: Monad w => Int -> Eval w EvalResult
eval maxTick =
    do 
       t <- getTick
       let t' = t + 1
       setTick t'
       (if (t' <= maxTick) then
           do result <- eval'
              case result of
                  EvalIncomplete -> eval maxTick
                  x -> return x
        else
            return EvalIncomplete)

eval' :: Monad w => Eval w EvalResult
eval' =
    let continue = return EvalIncomplete 
        popAndCheck :: Monad w => Eval w EvalResult
        popAndCheck = do  
               popFrame
               noMoreFrames <- callStackEmpty
               return (if noMoreFrames then EvalComplete Nothing else EvalIncomplete)
    in
    do cs <- getCallStack
       vs <- getVStack
       noMoreElements <- elementStackEmpty
       if noMoreElements then popAndCheck
         else do
           element <- popElement
           case element of
               EvReturn -> do
                   val <- peekVal
                   logMsg  ("return: " ++ lslShowVal val)
                   popAndCheck
               EvBlock [] -> popScope >> eval'
               EvBlock (s:ss) -> pushElements [EvBlock ss,EvCtxStatement s]
               EvCtxStatement s -> do
                   pushElements [EvStatement $ ctxItem s]
                   if (isTextLocation $ srcCtx s) then
                       let bp = mkBreakpoint (textName $ srcCtx s) (textLine0 $ srcCtx s) 0 in
                               do  brk <- checkBp bp
                                   if brk then return $ BrokeAt bp
                                          else continue
                       else continue
               EvStatement (Return mexpr) -> pushElements [EvReturn,EvMexpr $ fromMCtx mexpr]
               EvStatement (NullStmt) -> eval'
               EvStatement (StateChange s) -> return $ EvalComplete $ Just s
               EvStatement (Do expr) -> pushElements [EvDiscard,EvExpr $ ctxItem expr]
               EvStatement (Decl (Var name t) Nothing) ->
                   do initVar1 name t Nothing
                      continue
               EvStatement (Decl (Var name t) mexpr) -> pushElements [EvBind name t,EvMexpr $ fromMCtx mexpr]
               EvStatement (If expr stmt1 stmt2) -> pushElements [EvCond stmt1 stmt2,EvExpr $ ctxItem expr]
               EvStatement (While expr stmt) -> pushElements [EvLoop (ctxItem expr) Nothing stmt,EvExpr $ ctxItem expr]
               EvStatement (DoWhile stmt expr) -> 
                   pushElements [EvLoop (ctxItem expr) Nothing stmt, EvExpr (ctxItem expr),EvStatement stmt]
               EvStatement (For mexpr1 mexpr2 mexpr3 stmt) ->
                   do let expr =  case mexpr2 of
                                      Nothing -> (IntLit 1)
                                      Just expr2 -> ctxItem expr2
                      pushElement (EvLoop expr (fromMCtx mexpr3) stmt)
                      pushElement (EvExpr $ expr)
                      case mexpr1 of
                          Nothing -> return ()
                          Just expr1 -> pushElement (EvExpr $ ctxItem expr1)
                      continue
               EvStatement (Compound ss) ->
                   do pushScope [] $ labelBlocks ss
                      pushElement (EvBlock ss)
                      continue
               EvStatement (Label _) -> eval'
               EvStatement (Jump name) ->
                   do (n,stmts) <- unwindToLabel name
                      popElements n
                      pushElement (EvBlock stmts)
                      continue
               EvMexpr Nothing ->  pushVal VoidVal >> continue
               EvMexpr (Just expr) -> pushElements [EvExpr expr]
               EvDiscard -> popVal >> continue
               EvBind name t ->
                   do val <- popVal
                      initVar1 name t (Just val)
                      continue
               EvLoop expr mexpr stmt ->
                   do (IVal i) <- popVal
                      when (i /= 0) $ do
                              pushElement element               -- last, re-evaluate loop
                              pushElement (EvExpr expr)         -- next-to-last, re-evaluate expr)
                              case mexpr of                     -- if there's an end-of-loop expression, evaluate it
                                 Nothing -> return ()
                                 Just expr1 -> pushElement (EvExpr expr1)
                              pushElement (EvStatement stmt)    -- first, evaluate statement)
                      continue
               EvCond stmt1 stmt2 ->
                   do (IVal i) <- popVal
                      pushElement (EvStatement (if i == 0 then stmt2 else stmt1))
                      continue
               EvExpr (IntLit i) -> pushVal (IVal i) >> continue
               EvExpr (FloatLit f) -> pushVal (FVal f) >> continue
               EvExpr (StringLit s) -> pushVal (SVal s) >> continue
               EvExpr (KeyLit k) -> pushVal (KVal k) >> continue
               EvExpr (VecExpr e1 e2 e3) ->
                   -- TODO: this is probably the WRONG order of evaluation!!!
                   pushElements [EvMkVec,EvExpr $ ctxItem e3, EvExpr $ ctxItem e2, EvExpr $ ctxItem e1]
               EvExpr (RotExpr e1 e2 e3 e4) ->
                   pushElements [EvMkRot,EvExpr $ ctxItem e4, EvExpr $ ctxItem e3, EvExpr $ ctxItem e2, EvExpr $ ctxItem e1]
               EvExpr (ListExpr []) -> pushVal (LVal []) >> continue
               EvExpr (ListExpr ((Ctx _ e):es)) ->
                   pushElements [EvCons,EvExpr (ListExpr es), EvExpr e]
               EvExpr (Add expr1 expr2) -> pushBinary EvAdd expr1 expr2
               EvExpr (Sub expr1 expr2) -> pushBinary EvSub expr1 expr2
               EvExpr (Mul expr1 expr2) -> pushBinary EvMul expr1 expr2
               EvExpr (Div expr1 expr2) -> pushBinary EvDiv expr1 expr2
               EvExpr (Mod expr1 expr2) -> pushBinary EvMod expr1 expr2
               EvExpr (BAnd expr1 expr2) -> pushBinary EvBAnd expr1 expr2
               EvExpr (BOr expr1 expr2) -> pushBinary EvBOr expr1 expr2
               EvExpr (Xor expr1 expr2) -> pushBinary EvBXor expr1 expr2
               EvExpr (And expr1 expr2) -> pushBinary EvAnd expr1 expr2
               EvExpr (Or expr1 expr2) -> pushBinary EvOr expr1 expr2
               EvExpr (Lt expr1 expr2) -> pushBinary EvLt expr1 expr2
               EvExpr (Le expr1 expr2) -> pushBinary EvLe expr1 expr2
               EvExpr (Gt expr1 expr2) -> pushBinary EvGt expr1 expr2
               EvExpr (Ge expr1 expr2) -> pushBinary EvGe expr1 expr2
               EvExpr (Equal expr1 expr2) -> pushBinary EvEq expr1 expr2
               EvExpr (NotEqual expr1 expr2) -> pushBinary EvNe expr1 expr2
               EvExpr (ShiftL expr1 expr2) -> pushBinary EvShiftL expr1 expr2
               EvExpr (ShiftR expr1 expr2) -> pushBinary EvShiftR expr1 expr2
               EvExpr (Not expr) -> pushUnary EvNot expr
               EvExpr (Neg expr) -> pushUnary EvNeg expr
               EvExpr (Inv expr) -> pushUnary EvBInv expr
               EvExpr (Cast t expr) -> pushElements [EvCast t,EvExpr $ ctxItem expr]
               EvExpr (Get var) -> pushElements [EvGet $ ctxVr2Vr var]
               EvExpr (Set var expr) -> pushElements [EvSet $ ctxVr2Vr var, EvExpr $ ctxItem expr]
               EvExpr (IncBy var expr) -> pushModBy var EvAdd $ ctxItem expr
               EvExpr (DecBy var expr) -> pushModBy var EvSub $ ctxItem expr
               EvExpr (MulBy var expr) -> pushModBy var EvMul $ ctxItem expr
               EvExpr (DivBy var expr) -> pushModBy var EvDiv $ ctxItem expr
               EvExpr (ModBy var expr) -> pushModBy var EvMod $ ctxItem expr
               EvExpr (PreInc var) -> pushModBy var EvAdd (IntLit 1)
               EvExpr (PreDec var) -> pushModBy var EvSub (IntLit 1)
               EvExpr (PostInc var) ->
                   do  -- after all operations, top of stack should be var value prior to increment
                       pushElement (EvPop) -- take that value off, leaving the original value
                       pushModBy var EvAdd (IntLit 1)
                       pushElement (EvGet $ ctxVr2Vr var) -- put the current value on the stack
                       continue
               EvExpr (PostDec var) ->
                   do  -- after all operations, top of stack should be var value prior to increment
                       pushElement (EvPop)
                       pushModBy var EvSub (IntLit 1)
                       pushElement (EvGet $ ctxVr2Vr var) -- put the current value on the stack
                       continue
               EvExpr (Call (Ctx _ name) exprs) ->
                   case findFuncDec name predefFuncs of
                       Just (FuncDec _ t parms) -> pushElements [EvPredef name, EvExpr (ListExpr exprs)]
                       Nothing ->
                           do 
                              (Func (FuncDec ctxName t parms) stmts) <- getFunc name
                              pushElement (EvCall (ctxItem ctxName) (srcCtx ctxName) (ctxItems parms) stmts (t == LLVoid))
                              pushElement (EvExpr (ListExpr exprs))  -- first evaluate the arguments
                              continue
               EvCons ->
                   do (LVal l) <- popVal
                      val <- popVal
                      pushVal (LVal (val:l))
                      continue
               EvCall name ctx parms stmts voidFunc ->
                   do (LVal val) <- popVal -- should be the list of arguments
                      logMsg ("call: " ++ renderCall name val)
                      pushFrame name ctx Nothing
                      pushScope [] $ labelBlocks stmts
                      initVars1 parms val
                      -- a void function may not have an explicit return; if it does, this
                      -- element will get popped off without being evaluated.
                      when voidFunc (pushElement EvReturn >> pushElement (EvMexpr Nothing))
                      pushElement (EvBlock stmts)
                      continue
               EvPredef name -> evalPredef' name
               EvGet (name,c) ->
                   do val <- getVar name
                      pushVal $ lslValueComponent c val
                      continue
               EvPop -> popVal >> continue
               EvSet (name,c) ->
                   do val <- peekVal
                      varVal <- getVar name
                      let t = typeOfLSLComponent varVal c
                      let val' = case (t,val) of
                              (LLFloat,IVal i) -> FVal (fromInt i)
                              (LLString,KVal k) -> SVal k
                              (LLKey,SVal s) -> KVal s
                              (t, v) | t == typeOfLSLValue v -> v
                                     | otherwise -> error ("can't implicitly convert from " ++ 
                                                           (show $ typeOfLSLValue v) ++ 
                                                           " to " ++ (show t))
                      let varVal' = replaceLslValueComponent c varVal val'
                      setVar name varVal'
                      continue
               EvAdd -> evalBinary $ \val1 val2 -> case (val1,val2) of
                    (IVal v1, IVal v2) -> IVal (v1 + v2)
                    (FVal v1, IVal v2) -> FVal (v1 + fromInt v2)
                    (IVal v1, FVal v2) -> FVal (v2 + fromInt v1)
                    (FVal v1, FVal v2) -> FVal (v1 + v2)
                    (VVal x1 y1 z1, VVal x2 y2 z2) -> VVal (x1 + x2) (y1 + y2) (z1 + z2)
                    (RVal x1 y1 z1 s1,RVal x2 y2 z2 s2) -> RVal (x1 + x2) (y1 + y2) (z1 + z2) (s1 + s2)
                    (LVal l1,LVal l2) -> LVal (l1 ++ l2)
                    (v,LVal l2) -> LVal (v:l2)
                    (LVal l1, v) -> LVal (l1 ++ [v])
                    (SVal s1,SVal s2) -> SVal (s1 ++ s2)
                    _ -> error ("invalid Add operands: " ++ (show val1) ++ ", " ++ (show val2))
               EvSub -> evalBinary $ \ val1 val2 -> case (val1,val2) of
                    (IVal i1,IVal i2) -> IVal (i1 - i2)
                    (IVal i1,FVal f2) -> FVal (fromInt i1 - f2)
                    (FVal f1,IVal i2) -> FVal (f1 - fromInt i2)
                    (FVal f1,FVal f2) -> FVal (f1 - f2)
                    (VVal x1 y1 z1,VVal x2 y2 z2) -> VVal (x1 - x2) (y1 - y2) (z1 - z2)
                    (RVal x1 y1 z1 s1,RVal x2 y2 z2 s2) -> RVal (x1 - x2) (y1 - y2) (z1 - z2) (s1 - s2)
                    _ -> error ("cannot apply - operator to " ++ (show val1) ++ " and " ++ (show val2))
               EvMul -> evalBinary $ \ val1 val2 -> case (val1,val2) of
                    (IVal i1,IVal i2) -> IVal (i1*i2)
                    (IVal i1,FVal f2) -> FVal (fromInt i1 * f2)
                    (FVal f1,IVal i2) -> FVal (f1 * fromInt i2)
                    (FVal f1,FVal f2) -> FVal (f1 * f2)
                    (v@(VVal _ _ _),IVal i) -> let f = fromInt i in vecMulScalar v f
                    (v@(VVal _ _ _),FVal f) -> vecMulScalar v f
                    ((VVal x1 y1 z1),(VVal x2 y2 z2)) -> FVal $ x1 * x2 + y1 * y2 + z1 * z2
                    (v@(VVal _ _ _),r@(RVal _ _ _ _)) -> rotMulVec r v
                    (r1@(RVal _ _ _ _),r2@(RVal _ _ _ _)) -> rotMul r1 r2
                    _ -> error ("cannot apply * operator to " ++ (show val1) ++ " and " ++ (show val2))
               EvDiv -> evalBinary $ \ val1 val2 -> case (val1,val2) of
                    (IVal i1,IVal i2) -> IVal (i1 `div` i2) -- TODO: how does SL handle divide by zero?
                    (IVal i1,FVal f2) -> FVal (fromInt i1 / f2)
                    (FVal f1,IVal i2) -> FVal (f1 / fromInt i2)
                    (FVal f1,FVal f2) -> FVal (f1/f2)
                    (v@(VVal _ _ _),IVal i) -> let f = 1.0 / fromInt i in vecMulScalar v f
                    (v@(VVal _ _ _),FVal f) -> vecMulScalar v f
                    (v@(VVal _ _ _),r@(RVal _ _ _ _)) -> rotMulVec (invRot r) v
                    (r1@(RVal _ _ _ _),r2@(RVal _ _ _ _)) -> rotMul r1 $ invRot r2
                    _ -> error ("cannot apply operator to " ++ (show val1) ++ " and " ++ (show val2))
               EvMod -> evalBinary $ \ val1 val2 -> case (val1,val2) of
                    (IVal i1,IVal i2) -> IVal (i1 `mod` i2)
                    (v1@(VVal _ _ _),v2@(VVal _ _ _)) -> v1 `vcross` v2
                    _ -> error ("cannot apply operator to " ++ (show val1) ++ " and " ++ (show val2))
               EvBAnd -> evalBinary $ \ val1 val2 -> case (val1,val2) of
                    (IVal i1,IVal i2) -> IVal (i1 .&. i2)
                    _ -> error ("cannot apply operator to " ++ (show val1) ++ " and " ++ (show val2))
               EvBOr -> evalBinary $ \ val1 val2 -> case (val1,val2) of
                    (IVal i1,IVal i2) -> IVal (i1 .|. i2)
                    _ -> error ("cannot apply operator to " ++ (show val1) ++ " and " ++ (show val2))
               EvBXor -> evalBinary $ \ val1 val2 -> case (val1,val2) of
                    (IVal i1,IVal i2) -> IVal (i1 `xor` i2)
                    _ -> error ("cannot apply operator to " ++ (show val1) ++ " and " ++ (show val2))
               EvAnd -> evalBinary $ \ val1 val2 -> case (val1,val2) of
                    (IVal i1,IVal i2) -> IVal (if (toBool i1 && toBool i2) then 1 else 0)
                    _ -> error ("cannot apply operator to " ++ (show val1) ++ " and " ++ (show val2))
               EvOr -> evalBinary $ \ val1 val2 -> case (val1,val2) of
                    (IVal i1,IVal i2) -> IVal (if (toBool i1 || toBool i2) then 1 else 0)
                    _ -> error ("cannot apply operator to " ++ (show val1) ++ " and " ++ (show val2))
               EvLt -> evalBinary $ \ val1 val2 -> case (val1,val2) of
                    (IVal i1,IVal i2) -> toLslBool $ i1 < i2
                    (FVal f1,FVal f2) -> toLslBool $ f1 < f2
                    (FVal f1,IVal i2) -> toLslBool $ f1 < fromInt i2
                    (IVal i1,FVal f2) -> toLslBool $ fromInt i1 < f2
                    _ -> error ("cannot apply operator to " ++ (show val1) ++ " and " ++ (show val2))
               EvLe -> evalBinary $ \ val1 val2 -> case (val1,val2) of
                    (IVal i1,IVal i2) -> toLslBool $ i1 <= i2
                    (FVal f1,FVal f2) -> toLslBool $ f1 <= f2
                    (FVal f1,IVal i2) -> toLslBool $ f1 <= fromInt i2
                    (IVal i1,FVal f2) -> toLslBool $ fromInt i1 <= f2
                    _ -> error ("cannot apply operator to " ++ (show val1) ++ " and " ++ (show val2))
               EvGt -> evalBinary $ \ val1 val2 -> case (val1,val2) of
                    (IVal i1,IVal i2) -> toLslBool $ i1 > i2
                    (FVal f1,FVal f2) -> toLslBool $ f1 > f2
                    (FVal f1,IVal i2) -> toLslBool $ f1 > fromInt i2
                    (IVal i1,FVal f2) -> toLslBool $ fromInt i1 > f2
                    _ -> error ("cannot apply operator to " ++ (show val1) ++ " and " ++ (show val2))
               EvGe -> evalBinary $ \ val1 val2 -> case (val1,val2) of
                    (IVal i1,IVal i2) -> toLslBool $ i1 >= i2
                    (FVal f1,FVal f2) -> toLslBool $ f1 >= f2
                    (FVal f1,IVal i2) -> toLslBool $ f1 >= fromInt i2
                    (IVal i1,FVal f2) -> toLslBool $ fromInt i1 >= f2
                    _ -> error ("cannot apply operator to " ++ (show val1) ++ " and " ++ (show val2))
               EvEq -> evalBinary $ \ val1 val2 -> case (val1,val2) of
                    (LVal l1, LVal l2) -> toLslBool $ length l1 == length l2 -- special case of LSL weirdness
                    (SVal s, KVal k) -> toLslBool $ s == k
                    (KVal k, SVal s) -> toLslBool $ k == s
                    (FVal f1,IVal i2) -> toLslBool $ f1 == fromInt i2
                    (IVal i1,FVal f2) -> toLslBool $ fromInt i1 == f2
                    (v1,v2) -> toLslBool $ v1 == v2
               EvNe -> evalBinary $ \ val1 val2 -> case (val1,val2) of
                    (LVal l1, LVal l2) -> toLslBool $ length l1 /= length l2 -- special case of LSL weirdness
                    (SVal s, KVal k) -> toLslBool $ s /= k
                    (KVal k, SVal s) -> toLslBool $ k /= s
                    (FVal f1,IVal i2) -> toLslBool $ f1 /= fromInt i2
                    (IVal i1,FVal f2) -> toLslBool $ fromInt i1 /= f2
                    (v1,v2) -> toLslBool $ v1 /= v2
               EvShiftL -> evalBinary $ \ val1 val2 -> case (val1,val2) of
                    (IVal i1,IVal i2) -> IVal (i1 `shiftL` i2)
                    _ -> error ("cannot apply operator to " ++ (show val1) ++ " and " ++ (show val2))
               EvShiftR -> evalBinary $ \ val1 val2 -> case (val1,val2) of
                    (IVal i1,IVal i2) -> IVal (i1 `shiftR` i2)
                    _ -> error ("cannot apply operator to " ++ (show val1) ++ " and " ++ (show val2))
               EvNot -> do
                    val <- popVal
                    pushVal $ case val of
                       (IVal i) -> IVal (if i == 0 then 1 else 0)
                       _ -> error ("cannot apply operator to " ++ (show val))
                    continue
               EvBInv -> do
                    val <- popVal
                    pushVal $ case val of
                       (IVal i) -> IVal $ complement i
                       _ -> error ("cannot apply operator to " ++ (show val))
                    continue
               EvNeg -> do
                    val <- popVal
                    pushVal $ case val of
                       (IVal i) -> IVal (-i)
                       (FVal f) -> FVal (-f)
                       (VVal x y z) -> VVal (-x) (-y) (-z)
                       (RVal x y z s) -> RVal (-x) (-y) (-z) (-s)
                       _ -> error ("cannot apply operator to " ++ (show val))
                    continue
               EvCast t -> do
                    -- TODO: what are the valid typecasts?
                    -- TODO: what are the formats?
                    val <- popVal
                    pushVal $ case (t,val) of
                       (LLInteger,IVal i) -> IVal i
                       (LLInteger,FVal f) -> IVal (truncate f)
                       (LLInteger,SVal s) -> IVal (parseInt s)
                       -- TODO: can you cast a key to an int?
                       (LLFloat,FVal f) -> FVal f
                       (LLFloat,IVal i) -> FVal (fromInteger $ toInteger i)
                       (LLFloat,SVal s) -> FVal (parseFloat s)
                       -- TODO: can you cast a key to a float?
                       (LLString,v) -> toSVal v
                       -- TODO: can you cast anything but a string to a key?
                       (LLVector,SVal s) -> parseVector s
                       (LLRot,SVal s) -> parseRotation s
                       (LLList,SVal s) -> LVal [SVal s]
                    continue
               EvMkVec -> do
                   z <- popVal
                   y <- popVal
                   x <- popVal
                   pushVal $ VVal (toFloat x) (toFloat y) (toFloat z)
                   continue
               EvMkRot -> do
                   s <- popVal
                   z <- popVal
                   y <- popVal
                   x <- popVal
                   pushVal $ RVal (toFloat x) (toFloat y) (toFloat z) (toFloat s)
                   continue
               x -> error ("TODO: add all operators: " ++ (show x))

-- TODO: LSL also will parse hex notation for strings and floats...
parseVector s =
    case [(VVal x y z,t) | ("<",t0) <- lex s,
	                       (x,t1) <- reads t0,
	                       (",",t2) <- lex t1,
	 					   (y,t3) <- reads t2,
						   (",",t4) <- lex t3,
		                   (z,t5) <- reads t4,
		                   (">",t) <- lex t5] of
        [] -> VVal 0.0 0.0 0.0
        (v,_):_ -> v
parseRotation s =
    case [(RVal x y z w,t) | ("<",t0) <- lex s,
	                       (x,t1) <- reads t0,
	                       (",",t2) <- lex t1,
	 					   (y,t3) <- reads t2,
						   (",",t4) <- lex t3,
		                   (z,t5) <- reads t4,
						   (",",t6) <- lex t5,
		                   (w,t7) <- reads t6,
		                   (">",t) <- lex t7] of
        [] -> RVal 0.0 0.0 0.0 0.0
        (v,_):_ -> v

toLslBool bool = IVal (if bool then 1 else 0)
evalBinary f = 
    do val1 <- popVal
       val2 <- popVal
       pushVal $ f val1 val2
       return EvalIncomplete

pushUnary evalElement expr = pushElements [evalElement,EvExpr $ ctxItem expr]
pushBinary evalElement expr1 expr2 = pushElements [evalElement,EvExpr $ ctxItem expr1,EvExpr $ ctxItem expr2]
pushModBy var evalElement expr = pushElements [EvSet $ ctxVr2Vr var,evalElement,EvGet $ ctxVr2Vr var,EvExpr expr]

evalPredef' name = 
    do (LVal args) <- popVal
       key <- getMyPrimKey
       sid <- getScriptName
       oid <- getObjectId
       pid <- getPrimId
       event <- getCurrentEvent
       let scriptInfo = ScriptInfo oid pid sid key event
       (evalResult,retval) <- doAction name scriptInfo args
       pushVal retval
       return evalResult

ctxList es = map (Ctx UnknownSourceContext) es

data ExecutionInfo = ExecutionInfo String Int FrameInfo
