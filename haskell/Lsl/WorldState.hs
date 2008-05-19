module Lsl.WorldState where

import Control.Monad
import Control.Monad.State hiding (State,get)
import Control.Monad.Identity
import Control.Monad.Error
import Data.List
import Data.Bits
import Data.Map(Map)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace

--import Lsl.Avatar
import Lsl.Breakpoint
import Lsl.Builder
import Lsl.CodeHelper
import Lsl.Constants
import Lsl.Evaluation
import Lsl.EventSigs
import Lsl.Exec hiding (scriptImage)
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
                    wlisteners :: [(Int,(Listener,Bool))],
                    nextListenerId :: Int,
                    wobjects :: Map String LSLObject,
                    wprims :: Map String Prim,
                    worldScripts :: Map (String,String) Script, 
                    inventory :: [(String,LSLObject)],
                    tick :: Int,
                    msglog :: [LogMessage],
                    predefs :: [PredefFunc m],
                    randGen :: StdGen,
                    wlibrary :: [(String,Validity LModule)],
                    wscripts :: [(String,Validity CompiledLSLScript)],
                    worldDB :: ValueDB,
                    worldEventHandler :: Maybe (String, [(String,LSLValue)]),
                    worldAvatars :: Map String Avatar,
                    worldBreakpointManager :: BreakpointManager,
                    worldSuspended :: Maybe (String,String), -- prim-key, script-name, image
                    worldRegions :: Map (Int,Int) Region,
                    worldZeroTime :: Int,
                    worldKeyIndex :: Integer,
                    worldWebHandling :: WebHandling,
                    worldOutputQueue :: [SimEvent],
                    worldPendingHTTPRequests :: [String],
                    worldOpenDataChannels :: (Map String (String,String),Map (String,String) String),
                    worldXMLRequestRegistry :: Map String XMLRequestSourceType
                } deriving (Show)

-- a state monad for the World
type WorldM m = StateT (World m) m

worldM f = StateT (\ s -> return (f s))

-- extracting/updating the world state -----------------------------------------------------                   
queryWorld q = worldM (\w -> (q w, w))
updateWorld u = worldM (\w -> ((), u w))

getWorldDB :: Monad m => WorldM m ValueDB
getWorldDB = queryWorld worldDB
setWorldDB db = updateWorld (\ w -> w { worldDB = db })

getSliceSize :: Monad m => WorldM m Int
getSliceSize = queryWorld sliceSize
getListeners :: Monad m => WorldM m [(Int,(Listener,Bool))]
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
getWorldScripts :: Monad m => WorldM m (Map (String,String) Script)
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
getWLibrary :: Monad m => WorldM m  [(String,Validity LModule)]
getWLibrary = queryWorld wlibrary
getWorldAvatars :: Monad m => WorldM m (Map String Avatar)
getWorldAvatars = queryWorld worldAvatars
getWorldBreakpointManager :: Monad m => WorldM m BreakpointManager
getWorldBreakpointManager = queryWorld worldBreakpointManager
getWorldSuspended :: Monad m => WorldM m (Maybe (String,String))
getWorldSuspended = queryWorld worldSuspended
getWorldRegions :: Monad m => WorldM m (Map (Int,Int) Region)
getWorldRegions = queryWorld worldRegions
getWorldZeroTime :: Monad m => WorldM m Int
getWorldZeroTime = queryWorld worldZeroTime
getWorldKeyIndex :: Monad m => WorldM m Integer
getWorldKeyIndex = queryWorld worldKeyIndex
getWorldWebHandling :: Monad m => WorldM m WebHandling
getWorldWebHandling = queryWorld worldWebHandling
getWorldOutputQueue :: Monad m => WorldM m [SimEvent]
getWorldOutputQueue = queryWorld worldOutputQueue
getWorldPendingHTTPRequests :: Monad m => WorldM m [String]
getWorldPendingHTTPRequests = queryWorld worldPendingHTTPRequests
getWorldOpenDataChannels :: Monad m => WorldM m (Map String (String,String), Map (String,String) String)
getWorldOpenDataChannels = queryWorld worldOpenDataChannels
getWorldEventHandler :: Monad m => WorldM m (Maybe (String,[(String,LSLValue)]))
getWorldEventHandler = queryWorld worldEventHandler
getWorldXMLRequestRegistry :: Monad m => WorldM m (Map String XMLRequestSourceType)
getWorldXMLRequestRegistry = queryWorld worldXMLRequestRegistry

getRegion index = (lift getWorldRegions >>= M.lookup index)

getPrim k = (lift getPrims >>= M.lookup k)
getPrimVal k f = (lift getPrims >>= M.lookup k >>= return . f)
getPrimName k = getPrimVal k primName
getPrimPosition k = getPrimVal k primPosition
getPrimParent k = getPrimVal k primParent
getPrimDescription k = getPrimVal k primDescription
getPrimOwner k = getPrimVal k primOwner
getPrimGroup k = getPrimVal k primGroup
getPrimCreator k = getPrimVal k primCreator
getPrimRotation k = getPrimVal k primRotation
getPrimScale k = getPrimVal k primScale
getPrimFaces k = getPrimVal k primFaces
getPrimFlexibility k = getPrimVal k primFlexibility
getPrimMaterial k = getPrimVal k primMaterial
getPrimStatus k = getPrimVal k primStatus
getPrimVehicleFlags k = getPrimVal k primVehicleFlags
getPrimLight k = getPrimVal k primLight
getPrimTempOnRez k = getPrimVal k primTempOnRez
getPrimTypeInfo k = getPrimVal k primTypeInfo
getPrimPermissions k = getPrimVal k primPermissions
getPrimSitTarget k = getPrimVal k primSitTarget
getPrimSittingAvatar k = getPrimVal k primSittingAvatar
getPrimAttachment k = getPrimVal k primAttachment
getPrimPassTouches k = getPrimVal k primPassTouches
getPrimPassCollisions k = getPrimVal k primPassCollisions
getPrimPayInfo k = getPrimVal k primPayInfo
getPrimPendingEmails k = getPrimVal k primPendingEmails
getPrimRemoteScriptAccessPin k = getPrimVal k primRemoteScriptAccessPin

getPrimInventory k = getPrimVal k primInventory

getPrimNotecards k = getPrimInventory k >>= return . filter isInvNotecardItem
getPrimLandmarks k =getPrimInventory k >>= return . filter isInvLandmarkItem
getPrimClothing k = getPrimInventory k >>= return . filter isInvClothingItem
getPrimBodyParts k = getPrimInventory k >>= return . filter isInvBodyPartItem
getPrimObjects k = getPrimInventory k >>= return . filter isInvObjectItem
getPrimGestures k = getPrimInventory k >>= return . filter isInvGestureItem
getPrimSounds k = getPrimInventory k >>= return . filter isInvSoundItem
getPrimAnimations k = getPrimInventory k >>= return . filter isInvAnimationItem
getPrimTextures k = getPrimInventory k >>= return . filter isInvTextureItem
getPrimScripts k = getPrimInventory k >>= return . filter isInvScriptItem

getRootPrim pk =  (getPrimParent pk) >>= return . maybe pk id
 
-- TODO: temp until introduce region into Prim definition
getPrimRegion _ = (lift (return (0 :: Int,0 :: Int)))
primRegion :: a -> (Int,Int)
primRegion _ = (0,0)

runErrFace k i defaultVal = runAndLogIfErr 
    ("face " ++ (show i) ++ " or prim " ++ k ++ " not found") defaultVal

getPrimFace k i = getPrimFaces k >>= (lookupByIndex i)
getPrimFaceAlpha k i = getPrimFace k i >>= return . faceAlpha
getPrimFaceColor k i = getPrimFace k i >>= return . faceColor
getPrimFaceTextureInfo k i = getPrimFace k i >>= return . faceTextureInfo

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
setWorldRegions r = updateWorld (\ w -> w { worldRegions = r })
setWorldKeyIndex i = updateWorld (\ w -> w { worldKeyIndex = i })
setWorldOutputQueue i = updateWorld (\ w -> w { worldOutputQueue = i })
setWorldPendingHTTPRequests p = updateWorld (\ w -> w { worldPendingHTTPRequests = p })
setWorldOpenDataChannels o = updateWorld (\ w -> w { worldOpenDataChannels = o })
setWorldEventHandler e = updateWorld (\ w -> w { worldEventHandler = e })
setWorldXMLRequestRegistry r = updateWorld (\ w -> w { worldXMLRequestRegistry = r })

setPrim k p = (getPrims >>= return . (M.insert k p) >>= setPrims)
updatePrimVal k f = runErrPrim k () $ (lift getPrims >>= M.lookup k >>= return . f >>= lift . (setPrim k))

runErrPrim k defaultVal = runAndLogIfErr ("prim " ++ k ++ " not found") defaultVal
setPrimPosition k v = updatePrimVal k (\ p -> p { primPosition = v } )
setPrimRotation k v =  updatePrimVal k (\ p -> p { primRotation = v } )
setPrimScale k v =  updatePrimVal k (\ p -> p { primScale = v } )
setPrimDescription k v =  updatePrimVal k (\ p -> p { primDescription = v } )
setPrimName k v =  updatePrimVal k (\ p -> p { primName = v } )
setPrimParent k v =  updatePrimVal k (\ p -> p { primParent = v } )
setPrimFaces k v =  updatePrimVal k (\ p -> p { primFaces = v } )
setPrimFlexibility k v =  updatePrimVal k (\ p -> p { primFlexibility = v } )
setPrimMaterial k v =  updatePrimVal k (\ p -> p { primMaterial = v } )
setPrimOwner k v =  updatePrimVal k (\ p -> p { primOwner = v } )
setPrimGroup k v =  updatePrimVal k (\ p -> p { primGroup = v } )
setPrimStatus k v =  updatePrimVal k (\ p -> p { primStatus = v } )
setPrimVehicleFlags k v =  updatePrimVal k (\ p -> p { primVehicleFlags = v } )
setPrimLight k v =  updatePrimVal k (\ p -> p { primLight = v } )
setPrimTempOnRez k v =  updatePrimVal k (\ p -> p { primTempOnRez = v } )
setPrimTypeInfo k v =  updatePrimVal k (\ p -> p { primTypeInfo = v } )
setPrimPermissions k v =  updatePrimVal k (\ p -> p { primPermissions = v } )
setPrimSitTarget k v =  updatePrimVal k (\ p -> p { primSitTarget = v })
setPrimSittingAvatar k v =  updatePrimVal k (\ p -> p { primSittingAvatar = v } )
setPrimPassTouches k v  =  updatePrimVal k (\ p -> p { primPassTouches = v } )
setPrimPassCollisions k v  = updatePrimVal k (\ p -> p { primPassCollisions = v } )
setPrimPayInfo k v = updatePrimVal k (\ p -> p { primPayInfo = v } )
setPrimAttachment k v =  updatePrimVal k (\ p -> p { primAttachment = v } )
setPrimInventory k v =  updatePrimVal k (\ p -> p { primInventory = v } )
setPrimPendingEmails k v =  updatePrimVal k (\ p -> p { primPendingEmails = v })
setPrimRemoteScriptAccessPin k v = updatePrimVal k (\ p -> p { primRemoteScriptAccessPin = v })
setRegion regionIndex region = getWorldRegions >>= return . (M.insert regionIndex region) >>= setWorldRegions

updatePrimFace k i f = do
    faces <- getPrimFaces k
    let faces' = zipWith (\ face index -> if (index == i) then f face else face) faces [0..]
    lift $ setPrimFaces k faces'
    
setPrimFaceAlpha k i v = runErrFace k i () $ updatePrimFace k i (\ f -> f { faceAlpha = v })
setPrimFaceColor k i v = runErrFace k i () $ updatePrimFace k i (\ f -> f { faceColor = v })

insertWorldDB path value = do
    wdb <- getWorldDB
    setWorldDB (insertDB path value wdb)

lookupWorldDB path = do
    wdb <- getWorldDB
    return $ lookupDB path wdb
    
data WorldEventType = CreatePrim { wePrimName :: String, wePrimKey :: String }
                    | AddScript (String,String) String Bool -- script, prim key, activate
                    | ResetScript String String -- prim key, script name
                    | ResetScripts String -- object name
                    | WorldSimEvent { worldSimEventName :: String, worldSimEventArgs :: [SimEventArg] }
                    | DeferredScriptEvent { deferredScriptEvent :: Event, deferredScriptEventTarget :: DeferredScriptEventTarget }
                    | Chat { chatChannel :: Int, chatterName :: String, chatterKey :: String, chatMessage :: String,
                             chatLocation :: ((Int,Int),(Float,Float,Float)),
                             chatRange :: Maybe Float }
                    | TimerEvent { timerEventInterval :: Float,
                                   timerAddress :: (String,String) }
                    | PermissionRequestEvent { permissionRequestPrim :: String,
                                               permissionRequestScript :: String,
                                               permissionRequestAgent :: String,
                                               permissionRequestMask :: Int }
                    | SensorEvent { sensorAddress :: (String,String),
                                    sensorSenseName :: String,
                                    sensorSenseKey :: String,
                                    sensorSenseType :: Int,
                                    sensorSenseRange :: Float,
                                    sensorSenseArc :: Float,
                                    sensorRepeat :: Maybe Float }
                    | XMLRequestEvent {  xmlRequestSource :: XMLRequestSourceType,
                                         xmlRequestChannel :: String,
                                         xmlRequestIData :: Int,
                                         xmlRequestSData :: String }
                    | HTTPRequestEvent { httpRequestSource :: (String,String),
                                         httpRequestKey :: String,
                                         httpRequestURL :: String,
                                         httpRequestMethod :: String,
                                         httpRequestMimetype :: String,
                                         httpRequestBodyMaxlength :: Int,
                                         httpRequestVerifyCert :: Int,
                                         httpRequestBody :: String }
                    | XMLReplyEvent { xmlRequestKey :: String,
                                      xmlRequestChannel :: String,
                                      xmlRequestMessageId :: String,
                                      xmlRequestSData :: String,
                                      xmlRequestIData :: Int }
                    | DialogEvent { dialogAgent :: String,
                                    dialogMessage :: String,
                                    dialogButtons :: [String],
                                    dialogChannel :: Int,
                                    dialogSourceObject :: String }
                    | RezObjectEvent { rezObjectLinkSet :: [Prim],
                                       rezObjectPos :: (Float,Float,Float),
                                       rezObjectVel :: (Float,Float,Float),
                                       rezObjectRot :: (Float,Float,Float,Float),
                                       rezObjectStartParam :: Int,
                                       rezObjectRezzer :: String,
                                       rezObjectCopy :: Bool,
                                       rezObjectAtRoot :: Bool }
                    | ResetScriptEvent { resetScriptPrimKey :: String,
                                         resetScriptScriptName :: String }
                    | DetachCompleteEvent { detachObject :: String, detachAvatar :: String }
                    | GiveAvatarInventoryEvent { giveAvatarInventoryKey :: String, giveAvatarInventoryItem :: InventoryItem }
    deriving (Show)

data XMLRequestSourceType = XMLRequestInternal { xmlRequestTag :: String }
                          | XMLRequestExternal { xmlRequestTag :: String }
    deriving (Show)

data DeferredScriptEventTarget = DeferredScriptEventScriptTarget (String,String)
                               | DeferredScriptEventPrimTarget String -- pushes to all scripts in prim
                               | DeferredScriptEventObjectTarget String -- pushes to all scripts in all prims in object
    deriving (Show)
    
isSensorEvent (SensorEvent _ _ _ _ _ _ _) = True
isSensorEvent _ = False
    
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

data SimEvent = SimEvent { simEventName :: String, simEventArgs :: [SimEventArg], simEventDelay :: Int }
    deriving (Show)
data SimEventArg = SimEventArg { simEventArgName :: String, simEventArgValue :: String }
    deriving (Show)
    
data Listener = Listener {
                       listenerPrimKey :: String,
                       listenerScriptName :: String,
                       listenerChannel :: Int,
                       listenerName :: String,
                       listenerKey :: String,
                       listenerMsg :: String }
    deriving (Show)
    
data PredefFunc m = PredefFunc { predefFuncName :: String, 
                               predefAllowedKey :: Maybe String,
                               predefAllowedSID :: Maybe String,
                               predefExpectedArgs :: [Maybe LSLValue],
                               predef :: ScriptInfo -> [LSLValue] -> WorldM m (EvalResult,LSLValue) }
    deriving (Show)

instance Monad m => Show (ScriptInfo -> [LSLValue] -> WorldM m (EvalResult,LSLValue)) where
    showsPrec _ _ = showString "(function :: ScriptInfo -> [LSLValue] -> WorldM m (EvalResult,LSLValue))"

evalErrorT :: ErrorT String m v -> m (Either String v)
evalErrorT = runErrorT

fromErrorT def val = evalErrorT val >>= (return . either (const def) id)
    
fromErrorTWithErrAction action def val = do
    result <- evalErrorT val
    case result of 
        Left s -> action s >> return def
        Right v -> return v

runAndLogIfErr msg def val = do
    result <- evalErrorT val
    case result of
        Left s -> logAMessage LogWarn "sim" (msg ++ " (" ++ s ++ ")") >> return def
        Right v -> return v

logAMessage logLevel source s =
    do log <- getMsgLog
       tick <- getTick
       let message = LogMessage tick logLevel source s 
       setMsgLog (message:log)

logTrace source s =
    do log <- getMsgLog
       tick <- getTick
       let message = LogMessage tick LogTrace source s 
       setMsgLog (message:log)
              
newKey :: Monad m => WorldM m  String
newKey = do
    i <- getWorldKeyIndex
    setWorldKeyIndex (i + 1)
    return $ mkKey i
    
findAsset _ = return Nothing

isSoundAsset _ = False
isTextureAsset _ = False
isAnimationAsset _ = False

findTextureAsset "" = mzero
findTextureAsset _ = return $ InventoryItem (InventoryItemIdentification ("","")) undefined undefined

primHasActiveHandler pk handler =
    do scripts <- getPrimScripts pk
       images <- lift $ imagesForScripts scripts
       return $ foldl (\ x y -> x || hasActiveHandler y handler) False images
    where imagesForScripts scriptItems = do
              let scriptIndices = map (((,)pk) . fst . inventoryItemNameKey . inventoryItemIdentification) scriptItems
              allScripts <- getWorldScripts
              let scripts = map (flip M.lookup allScripts) scriptIndices
              return [ image | Just (Script (Valid image) _ _ _ _ _ _ _) <- scripts ]
scriptHasActiveHandler pk sn handler =
    do script <- lift getWorldScripts >>= M.lookup (pk,sn)
       Valid image <- return $ scriptImage script
       return $ hasActiveHandler image handler
             
lookupDataChannel scriptAddr = lift getWorldOpenDataChannels >>= M.lookup scriptAddr . snd
lookupScriptFromChan chan = lift getWorldOpenDataChannels >>= M.lookup chan . fst

insertScriptChannelPair script chan = lift $ do
    (c2s,s2c) <- getWorldOpenDataChannels
    setWorldOpenDataChannels (M.insert chan script c2s,M.insert script chan s2c)
    

----- MISC ----
durationToTicks dur = floor (1000.0 * dur)
ticksToDuration ticks = fromInt ticks / 1000.0

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