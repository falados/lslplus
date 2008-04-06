module Lsl.World1 where

import Lsl.Avatar
import Lsl.Breakpoint
import Lsl.Builder
import Lsl.Evaluation
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
import Control.Monad
import Control.Monad.State hiding (State,get)
import Control.Monad.Identity
import Control.Monad.Error
import Data.List
import Data.Bits
import Data.Map(Map)
import qualified Data.Map as M
import Debug.Trace
import System.Random

-- a data type that defines the state of the 'world'
data World m = World {
                    sliceSize :: Int,
                    maxTick :: Int,
                    wqueue :: WorldEventQueue,
                    wlisteners :: [(Int,Listener)],
                    nextListenerId :: Int,
                    wobjects :: Map String LSLObject,
                    wprims :: Map String Prim,
                    inventory :: [(String,LSLObject)],
                    tick :: Int,
                    msglog :: [LogMessage],
                    predefs :: [PredefFunc m],
                    randGen :: StdGen,
                    wlibrary :: [(String,Validity LModule)],
                    wscripts :: [(String,Validity CompiledLSLScript)],
                    worldDB :: ValueDB,
                    worldAvatars :: [(String,Avatar)]
                } deriving (Show)

-- a state monad for the World
type WorldM m = StateT (World m) m

worldM f = StateT (\ s -> return (f s))

-- execute a predefined ('ll') function
doPredef :: Monad m => String -> ScriptInfo -> [LSLValue] -> WorldM m (EvalResult,LSLValue)
doPredef name info@(oid, pid, sid, pkey) args =
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
getTick :: Monad m => WorldM m Int
getTick = queryWorld tick
getNextListenerId :: Monad m => WorldM m Int
getNextListenerId = queryWorld nextListenerId
getObjects :: Monad m => WorldM m (Map String LSLObject)
getObjects = (queryWorld wobjects)
getPrims :: Monad m => WorldM m (Map String Prim)
getPrims = queryWorld wprims
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

getNextEvent key sname =
    do (q::Maybe [Event]) <- (getEventQueue key sname)
       case q of
          Just (e:es) -> 
              do setEventQueue key sname es
                 return $ Just e
          _ -> return Nothing

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
llMessageLinked (oid,pid,sid,pkey) [IVal link,IVal val,SVal msg,KVal key] =
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
                   do mapM (flip (pushEvents oid) (Event "link_message" [IVal pid,IVal val,SVal msg, KVal key])) targetList
                      return (EvalIncomplete,VoidVal)

-- the key to 'sleep' is that instead of returning 'EvalIncomplete' as its EvalResult, it returns
-- 'YieldTil <some-tick>', which puts the script into a sleep state.
-- other functions which have a built in delay should use this mechanism as well.
llSleep (oid,pid,sid,pkey) [FVal f] =
    do tick <- getTick
       return $ (YieldTil (tick + floor (f * 1000.0)),VoidVal)
llSay info@(oid,pid,sid,pkey) [IVal chan, SVal message] =
    do logFromScript info $ concat ["chan = ", show chan, ", message = ", message]
       tick <- getTick
       Just name <- queryPrim pkey primName
       putWorldEvent tick $ Chat chan name pkey message
       return (EvalIncomplete,VoidVal)
llListen (oid,pid,sid,pkey) [IVal chan, SVal sender, KVal key, SVal msg] =
    do lid <- registerListener $ Listener pkey sid chan sender key msg
       return (EvalIncomplete,IVal lid)

llFrand (oid,pid,sid,pkey) [FVal maxval] =
    do r <- wrand
       continueWith $ FVal (maxval * r)

llSetPos (oid,pid,sid,pkey) [val] =
    do insertWorldDB ["prim", pkey, "pos" ] val
       continueWith VoidVal

llGetPos (oid,pid,sid,pkey) [] =
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
logFromScript (_,_,_,pkey) msg = logAMessage (pkey ++ ": " ++ msg)

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
    
data LSLObject = LSLObject { primKeys :: [String] } deriving (Show)

data Prim = Prim {
                    primName :: String,
                    primKey :: String,
                    primScripts :: [(String,(Validity ScriptImage,[Event]))],
                    notecards :: [(String,[String])],
                    animations :: [String],
                    textures :: [String],
                    sounds :: [String],
                    inventoryObjects :: [LSLObject] } deriving (Show)

emptyPrim name key =
    Prim { primName = name,
           primKey = key,
           primScripts = [],
           notecards = [],
           animations = [],
           textures = [],
           sounds = [],
           inventoryObjects = [] }
addScript prim name vimage = prim { primScripts = (name,(vimage,[Event "state_entry" []])):(primScripts prim) }
         
type RezzedObject = (String,LSLObject)
type RezzedObjects = [RezzedObject]

data Listener = Listener {
                       listenerPrimKey :: String,
                       listenerScriptName :: String,
                       listenerChannel :: Int,
                       listenerName :: String,
                       listenerKey :: String,
                       listenerMsg :: String }
    deriving (Show)
    

getEventQueue key scriptName =
    do prims <- getPrims
       case M.lookup key prims of
           Nothing -> do logAMessage ("prim " ++ key ++ " not found")
                         return Nothing
           Just p ->
               case lookup scriptName (primScripts p) of
                   Nothing -> do logAMessage ("script " ++ scriptName ++ " not found in prim " ++ key)
                                 return Nothing
                   Just (_,q) ->  return (Just q)

-- getEventQueue' :: [Prim] -> String -> String -> Maybe [Event]
-- getEventQueue' prims key scriptName =
--     do p <- find (\ p -> primKey p == key) prims
--        (_,q) <- lookup scriptName (primScripts p)
--        return q
       
setEventQueue key scriptName q =
    do updatePrim (\ p -> updateScript p scriptName (\ _ (image, qold) -> return (image, q))) key

pushEvents oid pid e =
    do objects <- getObjects
       case M.lookup oid objects of
           Nothing -> return ()
           Just o ->
               case lookupByIndex pid (primKeys o) of
                   Nothing -> return ()
                   Just key ->
                       do updatePrim (\ p -> let scripts' = map (\ (n,(si,eq)) -> (n,(si,eq ++ [e]))) (primScripts p) in
                                             return $ p { primScripts = scripts' }) key
       
pushEvent e key sid =
    updatePrim (\ p -> updateScript p sid (pushEvent' e)) key
    
pushEvent' e _ (image, es) = return (image,e:es)

getObjectNames :: (Monad m) => WorldM m [String]
getObjectNames = getObjects >>= (return . M.keys)
getListenerIds :: (Monad m) => WorldM m [Int]
getListenerIds = liftM (map fst) getListeners
getObject name = liftM (M.lookup name) getObjects

newWorld slice maxt iq = World {
               sliceSize = slice,
               maxTick = maxt,
               wqueue = iq,
               wlisteners = [],
               nextListenerId = 0,
               wobjects = M.empty,
               wprims = M.empty,
               inventory = [],
               tick = 0,
               msglog = [],
               predefs = defaultPredefs,
               randGen = mkStdGen 1,
               wlibrary = [],
               wscripts = [],
               worldDB = emptyDB,
               worldAvatars = []
           }
newWorld' slice maxt iq lib scripts avatars = 
    (newWorld slice maxt iq) { 
        wscripts = scripts, 
        wlibrary = lib,
        worldAvatars = avatars }

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
       
logAMessage s =
    do log <- getMsgLog
       tick <- getTick
       let message = LogMessage tick LogInfo "" s 
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

updateScript prim sid f =
    do scripts' <- modifyLookupM sid f $ primScripts prim
       return $ prim { primScripts = scripts' }

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
         
onRezScript param sname (Valid scriptImage) =
     (sname, (Valid (softReset scriptImage), [Event "on_rez" [IVal param]]))
onRezScript param sname (Invalid s) =
     (sname, (Invalid s, []))
onRezPrim param prim =
   let names = map fst $ primScripts prim
       states = map (fst.snd) $ primScripts prim in
       prim { primScripts = zipWith (onRezScript param) names states }

registerListener listener =
   do listeners <- getListeners
      id <- getNextListenerId
      let id' = id + 1
      setNextListenerId id'
      setListeners ((id,listener):listeners)
      return id

processEvents :: Monad m => WorldM m ()
processEvents =
    do tick <- getTick
       weq <- getWQueue
       case takeWQ tick weq of
           (Nothing,_) -> return ()
           (Just we,weq') -> 
               do setWQueue weq'
                  w <- queryWorld id
                  processEvent we
                  processEvents

sampleModule = LModule
    [llString "msg0" <<- slit "Message0",
     llString "msg1" <<- slit "Message1",
     GF (Func (FuncDec (nullCtx "sample") LLVoid []) 
          [llString "msg1" <<- slit "Message2",
           call "llSay" [ilit 0, (get "msg0")],
           call "llSay" [ilit 0, (get "msg1")]
          ])
    ] []
library = [("sample",sampleModule)]
              
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
              Just (Invalid s) -> updatePrim (\ p -> return $ addScript p name (Invalid s)) key
              Just (Valid code) -> do
                  let sstate = initLSLScript code
                  updatePrim (\ p -> return $ addScript p name (Valid sstate)) key
processEvent chat@(Chat chan name key msg) =
    do listeners <- getListeners    
       let listeners'= filter (matchListener chat) $ map snd listeners
       -- event goes to all UNIQUE addresses in list
       let addresses = nub $ map listenAddress listeners'
       mapM (\ (key,sid) -> pushEvent (Event "listen" [IVal chan, SVal name, KVal key, SVal msg]) key sid) addresses
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
    do objects <- getObjects
       objects' <- mapM processObject (M.toList objects)
       setObjects (M.fromList objects')

processObject :: Monad m => (String,LSLObject) -> WorldM m (String,LSLObject)
processObject (name,o) = 
    do let pairs = zip (primKeys o) [0..]
       mapM process pairs
       return (name, o)
--        prims <- getPrims
--        prims' <- modifyByPredM (\ p -> primKey p `elem` map fst pairs) (\p -> processPrim name (lookup (primKey p) pairs) p) prims
--        setPrims prims'
--        return (name,o)
    where process (k,index) = do
              prims <- getPrims
              case M.lookup k prims of
                  Nothing -> logAMessage ("prim " ++ k ++ " not found")
                  Just p -> do p' <- processPrim name index p
                               setPrims (M.insert k p' prims)
       
processPrim :: Monad m => String -> Int -> Prim -> WorldM m Prim
--processPrim name Nothing p = error ("can't determine index for prim: " ++ (primKey p))
processPrim name index p =
      do scripts' <- mapM (processScript name index (primKey p)) (primScripts p)
         return $ p { primScripts = scripts' }

processScript :: Monad m => String -> Int -> String -> (String,(Validity ScriptImage,[Event])) -> WorldM m (String,(Validity ScriptImage,[Event]))
processScript oid pid key (sid,(Valid image, _)) =
    do --(w::World) <- queryWorld id
       slice <- getSliceSize
       tick <- getTick
       let chkBp _ sm = return (False,sm)
       result <- executeLsl image oid pid sid key doPredef logAMessage getTick setTick chkBp getNextEvent (tick + slice)
       Just q <- getEventQueue key sid
       case result of
           Left s -> do logAMessage ("execution error in script " ++ key ++ "/" ++ sid ++ ":" ++ s)
                        return (sid,(Valid image,q))
           Right image' -> return (sid,(Valid image',q))
processScript oid pid key (sid,(Invalid s,q)) = return (sid,(Invalid s,q))

simulate :: Monad m => WorldM m ()
simulate =
    do w <- queryWorld id
       --etrace (show w)
       processEvents
       runScripts
       t <- getTick
       maxt <- getMaxTick
       setTick (t + 1)
       if t > maxt then return () else simulate

runSim :: Monad m => Int -> Int -> WorldEventQueue -> [(String,Validity CompiledLSLScript)] -> [(String,Validity LModule)] -> m ((),World m)
runSim slice maxt iq scripts library =
    runStateT (simulate) $ newWorld' slice maxt iq library scripts [(avatarKey defaultAvatar, defaultAvatar)]
           

testScript = LSLScript 
    [
        mimport "sample" [] "foo",
        llInteger "l" <<- ilit 5
    ]
    [
        state "default" [
            handler "state_entry" [] [
                llInteger "i" <<- ilit 1,
                llInteger "j" <<- ilit 1,
                llInteger "k" <<- ilit 1,
                "i" <=> postinc "j",
                "k" <=> preinc "j",
                call "llSay" [ilit 0, slit "i = " !+ castString (get "i") !+
                              slit ", j = " !+ castString (get "j") !+
                              slit ", k = " !+ castString (get "k") !+
                              slit ", l = " !+ castString (get "l")],
                call "llSay" [ilit 0, slit "length = " !+ 
                                      castString (call "llStringLength" [slit "01234"])],
                call "llSay" [ilit 0, slit "new String = " !+ 
                                      (call "llGetSubString" [slit "01234", ilit 0, neg $ ilit 1])],
                call "llSay" [ilit 0, castString (call "llGetPos" [])],
                call "foosample" []
            ]
        ]
    ]

lslExecSimple script scripts library =
    let scripts' = zip (map fst scripts) $ map (validLSLScript library . snd) scripts in
        do result <- runSim 1000 1000 [(1,CreatePrim "object" "00000000-0000-0000-0000-000000000001"),
             (2,AddScript ("script1",script) "00000000-0000-0000-0000-000000000001" True)]
             scripts' library
           return $ niceShowWorld (snd result)
           

-- lslExecSimpleFromFile file =
--     do script <- parseFile lslParser file
--        s <- return $ lslExecSimple script
--        print s

simulate' :: Monad m => World m -> m (World m)
simulate' world = 
    do (_,world') <- runStateT (simulate) world
       return world'
    
-- simulate a world built with the given library and the 
-- set of scripts
lslExec iq library scripts slice1 slice2 maxt action =
    untilM ((>= maxt) . tick) initialWorld $ \ w ->
        do w' <- simulate' w
           let newmax = tick w' + slice2
           action w'
           let w'' = w' { maxTick = newmax, msglog = []}
           return w''
    where untilM p v f = if p v then return v else f v
          initialWorld :: Monad m => World m
          initialWorld = newWorld' slice1 slice2 iq library scripts []
       
linkSet = -1::Int
linkAllOthers = -2::Int
linkAllChildren = -3::Int
linkThis = -4::Int
linkRoot = 1::Int

etrace :: Monad m => String -> m ()
etrace val = trace val $ return ()

-- ***************************************************************************

data SimCommand = SimContinue [Breakpoint] [SimEvent]

data WorldDef = WorldDef { worldDefScript :: String }

data SimStatus = SimEnded { simStatusMessage :: String, simStatusLog :: [LogMessage] } | 
                 SimInfo { simStatusEvents :: [SimEvent], simStatusLog :: [LogMessage] } |
                 SimSuspended { simStatusEvents :: [SimEvent], 
                                simStatusSuspendInfo :: ExecutionInfo,
                                simStatusLog :: [LogMessage] }

data SimInputEventDefinition m = SimInputEventDefinition { 
    simInputEventName :: String,
    simInputEventDescription :: String,
    simInputEventParameters :: [SimParam],
    simInputEventHandler :: String -> [LSLValue] -> WorldM m () }
    
data SimParam = SimParam { simParamName :: String, simParamDescription :: String,
                           simParamType :: SimParamType }
data SimParamType = SimParamPrim | SimParamAvatar | SimParamLSLValue LSLType
                    
data SimEvent = SimEvent { simEventName :: String, simEventArgs :: [SimEventArg], simEventDelay :: Int }
    deriving (Show)
data SimEventArg = SimEventArg { simEventArgName :: String, simEventArgValue :: String }
    deriving (Show)
    
initWorld def scripts lib = 
    newWorld' 1000 100000 iq lib scripts [(avatarKey defaultAvatar, defaultAvatar)]
    where iq = [(1,CreatePrim "object" "00000000-0000-0000-0000-000000000001"),
                (2,AddScript ("script1",worldDefScript def) "00000000-0000-0000-0000-000000000001" True)]

simStep init@(Left (worldDef, scripts, lib)) command =
    let world = initWorld worldDef scripts lib in simStep (Right world) command
simStep (Right world) (SimContinue _ events) =
    let t = tick world
        simEventToWorldEvent (SimEvent name args delay) = (t + delay, WorldSimEvent name args)
        wq' = putManyWQ (map simEventToWorldEvent events) (wqueue world)
        world' = world { wqueue = wq' }
        (_,world'') = runIdentity $ runStateT simulate world'
        log = msglog world''
        world''' = world'' { msglog = [] } in
        (SimInfo { simStatusEvents = [], simStatusLog = reverse log }, Right world''')
        
        
-- Event Descriptions and Handlers -------------------------------------------
------------------------------------------------------------------------------
--checkArgs def args = do
checkEventArgs def args = 
    do
        when (length params /= length args) $ fail "wrong number of parameters"
        mapM (uncurry checkEventArg) argList
    where params = simInputEventParameters def
          argList = map (\ p -> ( p , find (\ a -> simParamName p == simEventArgName a) args)) params
          
checkEventArg (SimParam name _ SimParamPrim) (Just arg) = return $ KVal (simEventArgValue arg)
checkEventArg (SimParam name _ SimParamAvatar) (Just arg) = return $ KVal (simEventArgValue arg)
checkEventArg (SimParam name _ (SimParamLSLValue t)) (Just arg) = evaluateExpression t (simEventArgValue arg)
checkEventArg (SimParam name _ _) Nothing = fail ("event argument " ++ name ++ " not found")

handleSimInputEvent def args = 
  case checkEventArgs def args of
      Left s -> logAMessage s
      Right argValues -> (simInputEventHandler def) (simInputEventName def) argValues
      
touchEventDef :: Monad m => SimInputEventDefinition m
touchEventDef =
    SimInputEventDefinition {
        simInputEventName = "Touch Prim",
        simInputEventDescription = "Avatar touches a prim",
        simInputEventParameters = [
            SimParam "Prim" "The prim the avatar should touch" SimParamPrim,
            SimParam "Avatar" "The avatar that should do the touching" SimParamAvatar,
            SimParam "Duration" "The duration of the touch" (SimParamLSLValue LLInteger)],
        simInputEventHandler = undefined }
        
eventDescriptors :: Monad m => Map [Char] (SimInputEventDefinition m)
eventDescriptors = M.fromList [
        ("Touch Prim", touchEventDef)
    ]