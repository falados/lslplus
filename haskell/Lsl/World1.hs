module Lsl.World1 where

import Control.Monad
import Control.Monad.State hiding (State,get)
import Control.Monad.Identity
import Control.Monad.Error
import Data.List
import Data.Bits
import Data.Map(Map)
import qualified Data.Map as M
import Debug.Trace

import Lsl.Avatar
import Lsl.Breakpoint
import Lsl.Builder
import Lsl.Evaluation
import Lsl.EventSigs
import Lsl.Exec
import Lsl.ExpressionHandler
import Lsl.FuncSigs
import Lsl.InternalLLFuncs
import Lsl.Key
import Lsl.Log
import Lsl.Parse
import Lsl.Structure
import Lsl.Type
import Lsl.Util
import Lsl.ValueDB
import Lsl.WorldDef

import System.Random
import Test.QuickCheck

-- a data type that defines the state of the 'world'
data World m = World {
                    sliceSize :: Int,
                    maxTick :: Int,
                    nextPause :: Int,
                    wqueue :: WorldEventQueue,
                    wlisteners :: [(Int,Listener)],
                    nextListenerId :: Int,
                    wobjects :: Map String LSLObject,
                    wprims :: Map String Prim,
                    worldScripts :: Map (String,String) (Validity ScriptImage,[Event]), 
                    inventory :: [(String,LSLObject)],
                    tick :: Int,
                    msglog :: [LogMessage],
                    predefs :: [PredefFunc m],
                    randGen :: StdGen,
                    wlibrary :: [(String,Validity LModule)],
                    wscripts :: [(String,Validity CompiledLSLScript)],
                    worldDB :: ValueDB,
                    worldAvatars :: [(String,Avatar)],
                    worldBreakpointManager :: BreakpointManager,
                    worldSuspended :: Maybe (String,String) -- prim-key, script-name, image
                } deriving (Show)

-- a state monad for the World
type WorldM m = StateT (World m) m

worldM f = StateT (\ s -> return (f s))

-- execute a predefined ('ll') function
doPredef :: Monad m => String -> ScriptInfo -> [LSLValue] -> WorldM m (EvalResult,LSLValue)
doPredef name info@(ScriptInfo oid pid sid pkey event) args =
    do predefs <- getPredefFuncs
       -- find the correct function to execute
       case tryPredefs name pkey sid args predefs of
           -- if found, execute it
           Just f -> f info args
           -- otherwise, perform a default action:
           -- log the fact that the function was called,
           -- return a default value
           Nothing -> do
               (_,rettype,argtypes) <- findM (\ (x,y,z) -> x == name) funcSigs
               logAMessage ("called " ++ name ++ (show args))
               return (EvalIncomplete,case rettype of
                      LLVoid -> VoidVal
                      LLInteger -> IVal 0
                      LLFloat -> FVal 0.0
                      LLString -> SVal ""
                      LLKey -> KVal nullKey
                      LLList -> LVal []
                      LLVector -> VVal 0.0 0.0 0.0
                      LLRot -> RVal 0.0 0.0 0.0 1.0)
                   
queryWorld q = worldM (\w -> (q w, w))
updateWorld u = worldM (\w -> ((), u w))

getWorldDB :: Monad m => WorldM m ValueDB
getWorldDB = queryWorld worldDB
setWorldDB db = updateWorld (\ w -> w { worldDB = db })

getSliceSize :: Monad m => WorldM m Int
getSliceSize = queryWorld sliceSize
getListeners :: Monad m => WorldM m [(Int,Listener)]
getListeners = queryWorld wlisteners
getMaxTick :: Monad m => WorldM m Int
getMaxTick = queryWorld maxTick
getNextPause :: Monad m => WorldM m Int
getNextPause = queryWorld nextPause
getTick :: Monad m => WorldM m Int
getTick = queryWorld tick
getNextListenerId :: Monad m => WorldM m Int
getNextListenerId = queryWorld nextListenerId
getObjects :: Monad m => WorldM m (Map String LSLObject)
getObjects = (queryWorld wobjects)
getPrims :: Monad m => WorldM m (Map String Prim)
getPrims = queryWorld wprims
getWorldScripts :: Monad m => WorldM m (Map (String,String) (Validity ScriptImage, [Event]))
getWorldScripts = queryWorld worldScripts
getInventory :: Monad m => WorldM m [(String,LSLObject)]
getInventory = queryWorld inventory
getMsgLog :: Monad m => WorldM m [LogMessage]
getMsgLog = queryWorld msglog
getWQueue :: Monad m => WorldM m WorldEventQueue
getWQueue = queryWorld wqueue
getPredefFuncs :: Monad m => WorldM m [PredefFunc m]
getPredefFuncs = queryWorld predefs
getRandGen :: Monad m => WorldM m StdGen
getRandGen = queryWorld randGen
getWScripts :: Monad m => WorldM m  [(String,Validity CompiledLSLScript)]
getWScripts = queryWorld wscripts
getWorldAvatars :: Monad m => WorldM m [(String,Avatar)]
getWorldAvatars = queryWorld worldAvatars
getWorldBreakpointManager :: Monad m => WorldM m BreakpointManager
getWorldBreakpointManager = queryWorld worldBreakpointManager
getWorldSuspended :: Monad m => WorldM m (Maybe (String,String))
getWorldSuspended = queryWorld worldSuspended

setListeners l = updateWorld (\w -> w { wlisteners = l })   
setNextListenerId i = updateWorld (\w -> w { nextListenerId = i })
setObjects os = updateWorld (\w -> w { wobjects = os })
setPrims ps = updateWorld (\w -> w { wprims = ps })
setInventory i = updateWorld (\w -> w { inventory = i })
setTick t = updateWorld (\w -> w { tick = t })
setMsgLog l = updateWorld (\w -> w { msglog = l })
setWQueue q = updateWorld (\w -> w { wqueue = q })
setRandGen g = updateWorld (\w -> w { randGen = g })
setWorldAvatars l = updateWorld (\w -> w { worldAvatars = l })
setWorldScripts s = updateWorld (\w -> w { worldScripts = s })
setWorldBreakpointManager m = updateWorld (\w -> w { worldBreakpointManager = m })
setWorldSuspended v = updateWorld (\w -> w { worldSuspended = v })

insertWorldDB path value = do
    wdb <- getWorldDB
    setWorldDB (insertDB path value wdb)

lookupWorldDB path = do
    wdb <- getWorldDB
    return $ lookupDB path wdb
    
nextTick :: Monad m => WorldM m Int
nextTick = do
    t <- getTick
    let t' = t + 1
    setTick t'
    return t'

wrand :: (Monad m, Random a) => WorldM m a
wrand = do g <- getRandGen
           let (v,g') = random g
           setRandGen g'
           return v
           
instance Monad m => Show (ScriptInfo -> [LSLValue] -> WorldM m (EvalResult,LSLValue)) where
    showsPrec _ _ = showString "(function :: ScriptInfo -> [LSLValue] -> WorldM m (EvalResult,LSLValue))"
    
data PredefFunc m = PredefFunc { predefFuncName :: String, 
                               predefAllowedKey :: Maybe String,
                               predefAllowedSID :: Maybe String,
                               predefExpectedArgs :: [Maybe LSLValue],
                               predef :: ScriptInfo -> [LSLValue] -> WorldM m (EvalResult,LSLValue) }
    deriving (Show)

tryPredef name key sid args (PredefFunc pname pkey psid pargs predef) =
    if (name == pname &&
        pkey `elem` [Nothing,Just key] &&
        psid `elem` [Nothing,Just sid] &&
       (foldl (&&) True $ zipWith (elem) pargs $ map ((:Nothing:[]).Just) args))
    then Just predef else Nothing

tryPredefs name key sid args predefs = foldl (mplus) Nothing $ map (tryPredef name key sid args) predefs
        
defaultPredef name predef =
    case lookup name $ map (\ (FuncDec n _ ps) -> (ctxItem n,length ps)) predefFuncs of
        Nothing -> error ("undefined predef " ++ name)
        Just numParams -> PredefFunc name Nothing Nothing (replicate numParams Nothing) predef

llMessageLinked :: Monad m => ScriptInfo -> [LSLValue] -> WorldM m (EvalResult,LSLValue)       
llMessageLinked (ScriptInfo oid pid sid pkey _) [IVal link,IVal val,SVal msg,KVal key] =
    do mNumPrims <- queryObject oid (\ o -> return $ length (primKeys o))
       case mNumPrims of
           Nothing -> do logAMessage ("object " ++ oid ++ " not found!")
                         return (EvalIncomplete,VoidVal)
           Just n ->
              let targetList =
                      if link == linkSet then [0..(n-1)]
                      else if link == linkAllOthers then delete pid [0..(n-1)]
                      else if link == linkAllChildren then [1..(n-1)]
                      else if link == linkThis then [pid]
                      else [link - 1] in
                   do mapM (flip (pushEvents oid) (Event "link_message" [IVal pid,IVal val,SVal msg, KVal key] [])) targetList
                      return (EvalIncomplete,VoidVal)

-- the key to 'sleep' is that instead of returning 'EvalIncomplete' as its EvalResult, it returns
-- 'YieldTil <some-tick>', which puts the script into a sleep state.
-- other functions which have a built in delay should use this mechanism as well.
llSleep info@(ScriptInfo oid pid sid pkey event) [FVal f] =
    do tick <- getTick
       return $ (YieldTil (tick + durationToTicks f),VoidVal)
llSay info@(ScriptInfo oid pid sid pkey event) [IVal chan, SVal message] =
    do logFromScript info $ concat ["chan = ", show chan, ", message = ", message]
       tick <- getTick
       Just name <- queryPrim pkey primName
       putWorldEvent tick $ Chat chan name pkey message
       return (EvalIncomplete,VoidVal)
llListen (ScriptInfo oid pid sid pkey event) [IVal chan, SVal sender, KVal key, SVal msg] =
    do lid <- registerListener $ Listener pkey sid chan sender key msg
       return (EvalIncomplete,IVal lid)

llFrand _ [FVal maxval] =
    do r <- wrand
       continueWith $ FVal (maxval * r)

llSetPos (ScriptInfo oid pid sid pkey event) [val] =
    do insertWorldDB ["prim", pkey, "pos" ] val
       continueWith VoidVal

llGetPos (ScriptInfo oid pid sid pkey event) [] =
    do val <- lookupWorldDB ["prim", pkey, "pos" ]
       case val of
           Just (Right v@(VVal _ _ _)) -> continueWith v
           _ -> continueWith (VVal 0.0 0.0 0.0)
           
groupList _ [] = []
groupList n l | n > 0 = take n l : groupList n (drop n l)
              | otherwise = groupList 1 l

genNum :: [Int] -> Integer -> Integer
genNum rands maxval = 
    let rands' :: [Integer]
        rands' = map ((0x80000000-).toInteger) rands
        topval :: Rational
        topval = fromInteger $ 
            foldl (.|.) 0 $ zipWith (shiftL) (replicate (length rands') 0xffffffff) [0,32..]
        randval :: Rational
        randval = fromInteger $ foldl (.|.) 0 $ zipWith (shiftL) rands' [0,32]
    in floor ((randval / topval) * (fromInteger maxval))
        
    
llListRandomize _ [LVal list, IVal stride] =
    let l1 = groupList stride list
        n = fac (toInteger $ length l1)
        randsNeeded = ceiling $ (logBase 2 (fromInteger n)) / 32
        wrands = replicate randsNeeded wrand
    in  do (rands::[Int]) <- sequence wrands
           let permutation = genNum rands n
           let list' = generatePermutation list permutation
           continueWith $ LVal list'
        
continueWith val = return (EvalIncomplete,val)

-- all the predefined functions for which implementations have been created
defaultPredefs :: Monad m => [PredefFunc m]
defaultPredefs = map (\(x,y) -> defaultPredef x y) 
    ([
        ("llFrand",llFrand),
        ("llListRandomize",llListRandomize),
        ("llListen", llListen),
        ("llMessageLinked", llMessageLinked),
        ("llSay",llSay),
        ("llSleep", llSleep),
        ("llGetPos", llGetPos),
        ("llSetPos",llSetPos)
    ] ++ internalLLFuncs)

logFromScript :: Monad m => ScriptInfo -> String -> WorldM m ()
logFromScript scriptInfo msg = logAMessage (scriptInfoPrimKey scriptInfo ++ ": " ++ msg)

indent n = showString $ (replicate (4 * n) ' ')
niceShowsList n l = showString "[\n" . indent (n+1) . 
    (foldl (.) (showString "") $ 
    weave (map shows l) (replicate (length l - 1) (showString ",\n" . indent (n+1)))) .
    showString "]"
niceShowsWorld w =
    showString "World:\n" .
    indent 1 . showString "wqueue: " . niceShowsList 1 (wqueue w) . showString "\n" .
    indent 1 . showString "msglog: " . niceShowsList 1 (reverse $ msglog w) . showString "\n"
niceShowWorld w = niceShowsWorld w ""
    
-- data LSLObject = LSLObject { primKeys :: [String] } deriving (Show)

-- data Prim = Prim {
--                     primName :: String,
--                     primKey :: String,
--                     primParent :: Maybe String,
--                     primScripts :: [String],
--                     notecards :: [(String,[String])],
--                     animations :: [String],
--                     textures :: [String],
--                     sounds :: [String],
--                     inventoryObjects :: [LSLObject] } deriving (Show)

emptyPrim name key =
    Prim { primName = name,
           primKey = key,
           primParent = Nothing,
           primScripts = [],
           notecards = [],
           animations = [],
           textures = [],
           sounds = [],
           inventoryObjects = [] }
addScript prim name = prim { primScripts = (name:(primScripts prim)) }
         
data Listener = Listener {
                       listenerPrimKey :: String,
                       listenerScriptName :: String,
                       listenerChannel :: Int,
                       listenerName :: String,
                       listenerKey :: String,
                       listenerMsg :: String }
    deriving (Show)
    

-- should monadify further...
getPrimInfo pkey =
    do prims <- getPrims
       case M.lookup pkey prims of
           Nothing -> return Nothing
           Just p ->
               case primParent p of 
                   Nothing -> return $ Just (pkey, 0, primName p)
                   Just p1Key -> do
                       objects <- getObjects
                       case M.lookup p1Key objects of
                           Nothing -> return Nothing
                           Just (LSLObject plist) ->
                               case elemIndex pkey plist of
                                   Nothing -> return Nothing
                                   Just i -> return $ Just (p1Key, i, primName p)
                                   
pushEvents oid pid e =
    do objects <- getObjects
       case M.lookup oid objects of
           Nothing -> return ()
           Just o ->
               case lookupByIndex pid (primKeys o) of
                   Nothing -> return ()
                   Just key -> do
                       prims <- getPrims
                       case M.lookup key prims of
                           Nothing -> return ()
                           Just p -> mapM_ (pushEvent e key) (primScripts p)
       
pushEvent e key sid =
    do scripts <- getWorldScripts
       case M.lookup (key,sid) scripts of
           Nothing -> logAMessage ("no such script: " ++ (show (key, sid)))
           Just (s,q) -> setWorldScripts (M.insert (key,sid) (s,q ++ [e]) scripts)

pushEventToPrim e key =
    do prims <- getPrims
       case M.lookup key prims of
           Nothing -> logAMessage ("no such prim: " ++ key)
           Just p -> mapM_ (pushEvent e key) (primScripts p)
           
pushEventToObject e key =
    do objects <- getObjects
       case M.lookup key objects of
           Nothing -> logAMessage("no such object: " ++ key)
           Just o ->  mapM_ (pushEventToPrim e) (primKeys o)

getObjectNames :: (Monad m) => WorldM m [String]
getObjectNames = getObjects >>= (return . M.keys)
getListenerIds :: (Monad m) => WorldM m [Int]
getListenerIds = liftM (map fst) getListeners
getObject name = liftM (M.lookup name) getObjects

newWorld slice maxt iq = World {
               sliceSize = slice,
               maxTick = maxt,
               nextPause = slice,
               wqueue = iq,
               wlisteners = [],
               nextListenerId = 0,
               wobjects = M.empty,
               wprims = M.empty,
               worldScripts = M.empty,
               inventory = [],
               tick = 0,
               msglog = [],
               predefs = defaultPredefs,
               randGen = mkStdGen 1,
               wlibrary = [],
               wscripts = [],
               worldDB = emptyDB,
               worldAvatars = [],
               worldBreakpointManager = emptyBreakpointManager,
               worldSuspended = Nothing
           }
           
newWorld' slice maxt iq lib scripts avatars = 
    (newWorld slice maxt iq) { 
        wscripts = scripts, 
        wlibrary = lib,
        worldAvatars = avatars }
newWorld'' slice maxt iq lib scripts avatars objs prims activeScripts =
    (newWorld' slice maxt iq lib scripts avatars) {
        wobjects = objs,
        wprims = prims,
        worldScripts = activeScripts }
        
addObjectToInventory name obj world = world { inventory = (name,obj):(inventory world) }

ticksPerSecond :: Int
ticksPerSecond = 1000

data WorldEventType = CreatePrim { wePrimName :: String, wePrimKey :: String }
                    | AddScript (String,String) String Bool -- script, prim key, activate
                    | ResetScript String String -- prim key, script name
                    | ResetScripts String -- object name
                    | WorldSimEvent { worldSimEventName :: String, worldSimEventArgs :: [SimEventArg] }
                    | Chat { chatChannel :: Int, chatterName :: String, chatterKey :: String, chatMessage :: String }
    deriving (Show)
    
type WorldEvent = (Int,WorldEventType) -- time, event

type WorldEventQueue = [WorldEvent]

takeWQ :: Int -> WorldEventQueue -> (Maybe WorldEventType,WorldEventQueue)
takeWQ i [] = (Nothing,[])
takeWQ i ((j,we):wes) | i >= j = (Just we,wes)
                      | otherwise = (Nothing,wes)

putWQ tick we wes = before ++ ((tick,we):after)
    where (before,after) = break ((>tick).fst) wes

putManyWQ [] wq = wq
putManyWQ ((tick,we):wes) wq = putWQ tick we (putManyWQ wes wq)

putWorldEvent tick we = 
    do weq <- getWQueue
       setWQueue $ putWQ tick we weq

checkBp bp sm =
    do  bpm <- getWorldBreakpointManager
        let (result,bpm',sm') = checkBreakpoint bp bpm sm
        setWorldBreakpointManager bpm'
        return (result,sm')
       
logAMessage s =
    do log <- getMsgLog
       tick <- getTick
       let message = LogMessage tick LogInfo "" s 
       setMsgLog (message:log)

logTrace source s =
    do log <- getMsgLog
       tick <- getTick
       let message = LogMessage tick LogTrace source s 
       setMsgLog (message:log)
              
updateObject f name = 
    do objects <- getObjects
       case M.lookup name objects of
           Nothing -> logAMessage ("object " ++ name ++ " not found")
           Just o -> do o' <- f o
                        setObjects (M.insert name o' objects)

updatePrim f key =
    do  prims <- getPrims
        case M.lookup key prims of
            Nothing -> logAMessage ("prim " ++ key ++ " not found")
            Just p -> do p' <- f p
                         setPrims (M.insert key p' prims)

queryObject oid f =
    do objects <- getObjects
       return $ case M.lookup oid objects of
           Nothing -> Nothing
           Just o -> case f o of
                         Nothing -> Nothing
                         Just v -> Just v

queryPrim k f =
    do prims <- getPrims
       return $ case M.lookup k prims of
           Nothing -> Nothing
           Just p -> Just (f p)
         
registerListener listener =
   do listeners <- getListeners
      id <- getNextListenerId
      let id' = id + 1
      setNextListenerId id'
      setListeners ((id,listener):listeners)
      return id

processEvents :: Monad m => WorldM m ()
processEvents =
    do suspenseInfo <- getWorldSuspended
       when (isNothing suspenseInfo) $ do
           tick <- getTick
           weq <- getWQueue
           case takeWQ tick weq of
               (Nothing,_) -> return ()
               (Just we,weq') -> 
                   do setWQueue weq'
                      w <- queryWorld id
                      processEvent we
                      processEvents
    where isNothing = maybe True (const False)
    
processEvent (CreatePrim name key) =
    do worldPrims <- getPrims
       objects <- getObjects
       let prim = emptyPrim name key
       setPrims (M.insert key prim worldPrims)
       setObjects (M.insert key (LSLObject [key]) objects)
processEvent (AddScript (name,script) key active) =
       do scripts <- getWScripts
          case lookup script scripts of
              Nothing -> logAMessage ("no such script: " ++ script)
              Just (Invalid s) -> do
                  updatePrim (\ p -> return $ addScript p name) key
                  scripts <- getWorldScripts
                  setWorldScripts (M.insert (key,name) (Invalid s,[]) scripts)
              Just (Valid code) -> do
                  updatePrim (\ p -> return $ addScript p name) key
                  let sstate = initLSLScript code
                  scripts <- getWorldScripts
                  setWorldScripts (M.insert (key,name) (Valid sstate,[Event "state_entry" [] []]) scripts)
processEvent chat@(Chat chan name key msg) =
    do listeners <- getListeners    
       let listeners'= filter (matchListener chat) $ map snd listeners
       -- event goes to all UNIQUE addresses in list
       let addresses = nub $ map listenAddress listeners'
       mapM (\ (key,sid) -> pushEvent (Event "listen" [IVal chan, SVal name, KVal key, SVal msg] []) key sid) addresses
       return ()
processEvent (WorldSimEvent name args) = 
    case M.lookup name eventDescriptors of
        Nothing -> logAMessage ("event " ++ name ++ " not understood")
        Just def -> handleSimInputEvent def args
processEvent _ = error "not implemented"

matchListener (Chat chan' sender' key' msg') (Listener pkey sid chan sender key msg) =
    chan == chan' &&
    (key' /= pkey) &&
    (sender == "" || sender == sender') &&
    (key == nullKey || key == key') &&
    (msg == "" || msg == msg')

listenAddress l = (listenerPrimKey l, listenerScriptName l)

runScripts :: Monad m => WorldM m ()
runScripts =
    do scripts <- getWorldScripts
       mapM_ findAndRunScript (M.keys scripts)
       
findAndRunScript scriptKey =
    do scripts <- getWorldScripts
       case M.lookup scriptKey scripts of
          Nothing -> return ()
          Just script -> checkAndRunScript scriptKey script
checkAndRunScript _ (Invalid _,_) = return ()
checkAndRunScript k@(pkey,sname) (Valid img, q) =
    do  pInfo <- getPrimInfo pkey
        suspenseInfo <- getWorldSuspended
        case suspenseInfo of
            Nothing -> run pInfo
            Just (suspKey,suspName) | pkey == suspKey && suspName == sname -> run pInfo
                                    | otherwise -> return ()
   where run pInfo =
             case pInfo of
                 Nothing -> logAMessage ("script " ++ (show k) ++ ": prim not found!")
                 Just (parent, index, primName) -> runScript parent index pkey sname primName img q
runScript parent index pkey sname primName img q =
    do slice <- getSliceSize
       tick <- getTick
       let img' = img { scriptImageName = sname ++ "/" ++ primName ++ "/" ++ pkey }
       let log = logTrace (pkey ++ ":" ++ sname)
       --let checkBp _ sm = return (False,sm)
       result <- executeLsl img' parent index sname pkey doPredef log getTick setTick checkBp q (tick + slice)
       case result of
           Left s -> logAMessage ("execution error in script " ++ "(" ++ pkey ++ "," ++ sname ++ ") ->" ++ s)
           Right (img'',q') -> do
               scripts <- getWorldScripts
               setWorldScripts (M.insert (pkey,sname) (Valid img'',q') scripts)
               case executionState img'' of
                   Suspended bp -> setWorldSuspended (Just (pkey,sname))
                   _ -> setWorldSuspended Nothing

simulate :: Monad m => WorldM m ()
simulate =
    do processEvents
       runScripts
       t <- getTick
       setTick (t + 1)
       pauseTime <- getNextPause
       suspendInfo <- getWorldSuspended
       if t + 1 >= pauseTime || isSuspended suspendInfo then return () else simulate
    where isSuspended Nothing = False
          isSuspended _ = True

linkSet = -1::Int
linkAllOthers = -2::Int
linkAllChildren = -3::Int
linkThis = -4::Int
linkRoot = 1::Int

etrace :: Monad m => String -> m ()
etrace val = trace val $ return ()
trace1 s val = trace (s ++ ": " ++ show val) val

-- ***************************************************************************

data SimCommand = SimContinue { simCmdBreakpoints :: [Breakpoint], simCmdEvents :: [SimEvent] }
                | SimStep { simCmdBreakpoints :: [Breakpoint], simCmdEvents :: [SimEvent] }
                | SimStepOver { simCmdBreakpoints :: [Breakpoint], simCmdEvents :: [SimEvent] }
                | SimStepOut { simCmdBreakpoints :: [Breakpoint], simCmdEvents :: [SimEvent] }

data WorldDef = WorldDef { worldDefScript :: String }

data SimStatus = SimEnded { simStatusMessage :: String, simStatusLog :: [LogMessage], simStatusState :: SimStateInfo } | 
                 SimInfo { simStatusEvents :: [SimEvent], simStatusLog :: [LogMessage], simStatusState :: SimStateInfo } |
                 SimSuspended { simStatusEvents :: [SimEvent], 
                                simStatusSuspendInfo :: ExecutionInfo,
                                simStatusLog :: [LogMessage],
                                simStatusState :: SimStateInfo }

data SimStateInfo = SimStateInfo {
        simStateInfoPrims :: [(String,String)],
        simStateInfoAvatars :: [(String,String)]
    }
nullSimState = SimStateInfo [] []

stateInfoFromWorld world =
    SimStateInfo {
        simStateInfoPrims = map (\ p -> (primKey p, primName p)) (M.elems $ wprims world),
        simStateInfoAvatars = map (\ (_,a) -> (avatarKey a, avatarName a)) (worldAvatars world)
    }
    
data SimInputEventDefinition m = SimInputEventDefinition { 
    simInputEventName :: String,
    simInputEventDescription :: String,
    simInputEventParameters :: [SimParam],
    simInputEventHandler :: String -> [LSLValue] -> WorldM m () }

data SimParam = SimParam { simParamName :: String, simParamDescription :: String,
                           simParamType :: SimParamType }
data SimParamType = SimParamPrim | SimParamAvatar | SimParamLSLValue LSLType | SimParamRootPrim | SimParamKey |
                    SimParamScript
                    
data SimEvent = SimEvent { simEventName :: String, simEventArgs :: [SimEventArg], simEventDelay :: Int }
    deriving (Show)
data SimEventArg = SimEventArg { simEventArgName :: String, simEventArgValue :: String }
    deriving (Show)
    
-- initWorld def scripts lib = 
--     newWorld' 1000 1000000 iq lib scripts [(avKey, defaultAvatar avKey)]
--     where avKey = nextKey nullKey
--           primKey = nextKey avKey
--           iq = [(1,CreatePrim "object" primKey),
--                 (2,AddScript ("script1",worldDefScript def) primKey True)]

initWorld def scripts lib = worldFromFullWorldDef newWorld'' def lib scripts

simStep init@(Left (worldDef, scripts, lib)) command =
--     let world = initWorld worldDef scripts lib in simStep (Right world) command
    case initWorld worldDef scripts lib of
        Left s -> -- initialization failed
            (SimEnded ("Error initializing: " ++ s) [] nullSimState, init)
        Right world -> simStep (Right (logInitialProblems world)) command
simStep (Right world) command =
    let t = tick world
        events = simCmdEvents command
        newBreakpointManager = replaceBreakpoints (simCmdBreakpoints command) (worldBreakpointManager world)
        slice = sliceSize world
        newScripts = case worldSuspended world of
             Nothing -> worldScripts world
             Just (k,s) ->
                 let (Just (Valid img,eq)) = M.lookup (k,s) (worldScripts world) -- TODO: non-exhaustive but MUST succeed
                     stepMgr = stepManager img
                     img' = case command of
                                SimStep _ _ -> img { stepManager = setStepBreakpoint stepMgr }
                                SimStepOver _ _ -> img { stepManager = setStepOverBreakpoint stepMgr }
                                SimStepOut _ _ -> img { stepManager = setStepOutBreakpoint stepMgr }
                                _ -> img
                 in M.insert (k,s) (Valid img',eq) (worldScripts world)
        simEventToWorldEvent (SimEvent name args delay) = (t + delay, WorldSimEvent name args)
        wq' = putManyWQ (map simEventToWorldEvent events) (wqueue world)
        world' = trace ("breakpoints: " ++ show newBreakpointManager) $ world { wqueue = wq', 
                         nextPause = t + slice,
                         worldBreakpointManager = newBreakpointManager,
                         worldScripts = newScripts }
        (_,world'') = runIdentity $ runStateT simulate world'
        log = msglog world''
        world''' = world'' { msglog = [] } in
        if trace1 "tick" (tick world''') < trace1 "maxTick" (maxTick world''') 
            then case worldSuspended world''' of
                Nothing ->
                     (SimInfo { simStatusEvents = [], 
                                simStatusLog = reverse log,
                                simStatusState = stateInfoFromWorld world''' }, Right world''')
                Just (k,s) ->
                     let (Just (Valid img,eq)) = M.lookup (k,s) (worldScripts world''') -- TODO: non-exhaustive but MUST succeed
                         (Suspended bp) = executionState img -- TODO: non-exhaustive but MUST succeed
                         executionInfo = ExecutionInfo (breakpointFile bp) (breakpointLine bp) (frameInfo img)
                     in
                     (SimSuspended { simStatusEvents = [], 
                                     simStatusSuspendInfo = executionInfo,
                                     simStatusLog = reverse log,
                                     simStatusState = stateInfoFromWorld world''' }, Right world''')
            else (SimEnded { simStatusMessage = "ended", 
                             simStatusLog = reverse log,
                             simStatusState = stateInfoFromWorld world''' }, Right world''')
        
-- Event Descriptions and Handlers -------------------------------------------
------------------------------------------------------------------------------
--checkArgs def args = do
checkEventArgs def args = 
    do
        when (length params /= length args) $ fail "wrong number of parameters"
        mapM (uncurry checkEventArg) argList
    where params = simInputEventParameters def
          argList = map (\ p -> ( p , find (\ a -> simParamName p == simEventArgName a) args)) params
          
checkEventArg (SimParam _ _ SimParamPrim) (Just arg) = return $ KVal (simEventArgValue arg)
checkEventArg (SimParam _ _ SimParamRootPrim) (Just arg) = return $ KVal (simEventArgValue arg)
checkEventArg (SimParam _ _ SimParamScript) (Just arg) = return $ SVal (simEventArgValue arg)
checkEventArg (SimParam name _ SimParamAvatar) (Just arg) = return $ KVal (simEventArgValue arg)
checkEventArg (SimParam name _ SimParamKey) (Just arg) = return $ KVal (simEventArgValue arg)
checkEventArg (SimParam name _ (SimParamLSLValue t)) (Just arg) = evaluateExpression t (simEventArgValue arg)
checkEventArg (SimParam name _ _) Nothing = fail ("event argument " ++ name ++ " not found")

validSimInputEvent simEvent =
   case M.lookup (worldSimEventName simEvent) (eventDescriptors :: Map [Char] (SimInputEventDefinition Identity)) of
       Left s -> fail s
       Right def ->
           case checkEventArgs def (worldSimEventArgs simEvent) of
               Left s -> fail s
               Right _ -> return ()
               
handleSimInputEvent def args = 
  case checkEventArgs def args of
      Left s -> logAMessage s
      Right argValues -> (simInputEventHandler def) (simInputEventName def) argValues

mkTouchStartEvent pk nd ak = WorldSimEvent "touch_start" [SimEventArg "Prim Key" pk, SimEventArg "num_detected" nd, SimEventArg "Avatar key" ak]
mkTouchEvent pk nd ak = WorldSimEvent "touch" [SimEventArg "Prim Key" pk, SimEventArg "num_detected" nd, SimEventArg "Avatar key" ak, SimEventArg "Grab vector" "<0.0,0.0,0.0>"]
mkTouchEndEvent pk nd ak = WorldSimEvent "touch_end" [SimEventArg "Prim Key" pk, SimEventArg "num_detected" nd, SimEventArg "Avatar key" ak]

userTouchEventDef :: Monad m => SimInputEventDefinition m
userTouchEventDef =
    SimInputEventDefinition {
        simInputEventName = "Touch Prim",
        simInputEventDescription = "Avatar touches a prim for a duraction",
        simInputEventParameters = [
            SimParam "Prim" "The prim the avatar should touch" SimParamPrim,
            SimParam "Avatar" "The avatar that should do the touching" SimParamAvatar,
            SimParam "Duration" "The duration of the touch (in seconds)" (SimParamLSLValue LLFloat)],
        simInputEventHandler = 
            let f _ [KVal pk, KVal ak, FVal duration] = do
                    t <- getTick
                    putWorldEvent t (mkTouchStartEvent pk "1" ak)
                    let tdur = durationToTicks duration
                    let tticks = [(t + 1),(t + 1 + 500)..(t + tdur)]
                    mapM_ ((flip putWorldEvent) (mkTouchEvent pk "1" ak)) tticks
                    putWorldEvent (t + tdur) (mkTouchEndEvent pk "1" ak)
                f name _ = logAMessage ("invalid event activation: " ++ name)
            in f
    }
        
eventDescriptors :: Monad m => Map [Char] (SimInputEventDefinition m)
eventDescriptors = M.fromList $ mkEventDefList ([userTouchEventDef] ++ rawLslEventDescriptors)

mkEventDefList eventDefs = map (\ e -> (simInputEventName e, e)) eventDefs

rawLslEventDescriptors :: Monad m => [SimInputEventDefinition m]
rawLslEventDescriptors = map lslEventDescriptorToSimInputDef lslEventDescriptors

lslEventDescriptorToSimInputDef (name, params, delivery, additionalData, description) =
    SimInputEventDefinition {
        simInputEventName = name,
        simInputEventDescription = "This is a raw LSL event: " ++ description,
        simInputEventParameters =
            (case delivery of
               EventDeliveryScript -> [SimParam "Prim Key" "key of prim to deliver to" SimParamPrim,
                                       SimParam "Script" "name of script to deliver to" SimParamScript]
               EventDeliveryObject -> [SimParam "Object Key" "key of object to deliver to" SimParamRootPrim]
               EventDeliveryRoot -> [SimParam "Object Key" "key of object to deliver to" SimParamRootPrim]
               EventDeliveryPrim -> [SimParam "Prim Key" "key of prim to deliver to" SimParamPrim]
               ) ++
            (flip map additionalData (\ d -> case d of
                EventAdditionalKeys name description -> SimParam name description SimParamKey
                EventAdditionalAvatarKeys name description -> SimParam name description SimParamAvatar
                EventAdditionalVectors name description -> SimParam name description (SimParamLSLValue LLVector))) ++
            map ( \ (t,name) -> SimParam name "" (SimParamLSLValue t)) params,
        simInputEventHandler =
            let f _ list = 
                    let lenAddl = length additionalData
                        (df,rest) = case delivery of
                               EventDeliveryScript -> 
                                   let (KVal pk:SVal sn:vals) = list in ((\ e -> pushEvent e pk sn),vals)
                               EventDeliveryObject ->
                                   let (KVal key:vals) = list in ((\ e -> pushEventToObject e key),vals)
                               _ -> let (KVal key:vals) = list in ((\ e -> pushEventToPrim e key), vals)
                    in df (Event name (drop lenAddl rest) (zipWith mkInfo additionalData (take lenAddl rest)) )
                       where mkInfo (EventAdditionalVectors _ _) val = ("vector",val)
                             mkInfo _                            val = ("key",val)
            in f
    }

----- MISC ----
durationToTicks dur = floor (1000.0 * dur)

logInitialProblems world =
    let log = msglog world
        newLog = [LogMessage 0 LogWarn "init" ("script \"" ++ name ++ "\" in prim " ++ prim ++ " failed to activate because of error: " ++ s) |
                          ((prim,name),(Invalid (_,s),_)) <- M.toList (worldScripts world) ] ++ log
    in world { msglog = newLog }