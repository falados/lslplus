{-# LANGUAGE FlexibleInstances,
             NoMonomorphismRestriction,
             GeneralizedNewtypeDeriving,
             MultiParamTypeClasses,
             TypeSynonymInstances
  #-}
{-# OPTIONS_GHC -fwarn-unused-binds -fwarn-unused-imports #-}
module Language.Lsl.Internal.WorldState(
    DeferredScriptEventTarget(..),
    Listener(..),
    PredefFunc(..),
    SimEvent(..),
    SimEventArg(..),
    Touch(..),
    WorldEventType(..),
    World(..),
    WorldE,
    XMLRequestSourceType(..),
    durationToTicks,
    evalWorldE,
    findAsset,
    findTextureAsset,
    findTexture,
    fromWorldE,
    getActualPrimScripts,
    getListeners,
    getNextListenerId,
    getNextPause,
    getObjects,
    getObjectDynamics,
    setObjectDynamics,
    getObjectPosition,
    getObjectRotation,
    getObjectVelocity,
    getObject,
    getParcelByPosition,
    getPos,
    getPredefFuncs,
    getPrim,
    getPrimAnimations,
    getPrimAttachment,
    getPrimBodyParts,
    getPrimClothing,
    getPrimCreator,
    getPrimDescription,
    getPrimFaceAlpha,
    getPrimFaceColor,
    getPrimFaceTextureInfo,
    getPrimFaces,
    getPrimFlexibility,
    getPrimGestures,
    getPrimGroup,
    getPrimInventory,
    getPrimLandmarks,
    getPrimLight,
    getPrimLinkNum,
    getPrimName,
    getPrimNotecards,
    getPrimMaterial,
    getPrimObjects,
    getPrimOwner,
    getPrimParcel,
    getPrimParent,
    getPrimPassCollisions,
    getPrimPermissions,
    getPrimRegion,
    getPrimPassTouches,
    getPrimPendingEmails,
    getPrimPosition,
    getPrimRotation,
    getPrimScale,
    getPrimScripts,
    getPrimSitTarget,
    getPrimSittingAvatar,
    getPrimSounds,
    getPrimStatus,
    getPrimTempOnRez,
    getPrimTextures,
    getPrimTypeInfo,
    getPrimVehicleFlags,
    getPrims,
    getRegion,
    getRootPrim,
    getSliceSize,
    getTick,
    getWLibrary,
    getWQueue,
    getWScripts,
    getWorldAvatar,
    getWorldAvatars,
    getWorldBreakpointManager,
    getWorldCollisions,
    getWorldEventHandler,
    getWorldLandCollisions,
    getWorldLastPositions,
    getWorldOutputQueue,
    getWorldPendingHTTPRequests,
    getWorldPhysicsTime,
    getWorldRegions,
    getWorldScripts,
    getWorldSuspended,
    getWorldTouchCheckTime,
    getWorldTouches,
    getWorldTargetCheckTime,
    getWorldWebHandling,
    getWorldXMLRequestRegistry,
    getWorldZeroTime,
    insertScriptChannelPair,
    isSensorEvent,
    isSoundAsset,
    logAMessage,
    logTrace,
    lookupDataChannel,
    lookupScriptFromChan,
    newKey,
    primHasActiveHandler,
    pushDeferredScriptEvent,
    pushDeferredScriptEventToPrim,
    pushDeferredScriptEventToObject,
    pushChangedEventToObject,
    pushAttachEvent,
    putHTTPTimeoutEvent,
    putHTTPResponseEvent,
    putParcel,
    putManyWQ,
    putWQ,
    putWorldEvent,
    runAndLogIfErr,
    runErrFace,
    runErrPrim,
    scriptHasActiveHandler,
    setListeners,
    setNextListenerId,
    setObjects,
    setPrim,
    setPrimAttachment,
    setPrimDescription,
    setPrimFaceAlpha,
    setPrimFaceColor,
    setPrimFaces,
    setPrimFlexibility,
    setPrimInventory,
    setPrimLight,
    setPrimMaterial,
    setPrimName,
    setPrimOwner,
    setPrimParent,
    setPrimPassCollisions,
    setPrimPassTouches,
    setPrimPayInfo,
    setPrimPendingEmails,
    setPrimPosition,
    setPrimRemoteScriptAccessPin,
    setPrimRotation,
    setPrimScale,
    setPrimSitTarget,
    setPrimSittingAvatar,
    setPrimStatus,
    setPrimTempOnRez,
    setPrimVehicleFlags,
    setPrims,
    setRegion,
    setTexture,
    setTick,
    setWQueue,
    setWorldAvatar,
    setWorldAvatars,
    setWorldBreakpointManager,
    setWorldCollisions,
    setWorldEventHandler,
    setWorldLandCollisions,
    setWorldLastPositions,
    setWorldOutputQueue,
    setWorldPendingHTTPRequests,
    setWorldPhysicsTime,
    setWorldScripts,
    setWorldSuspended,
    setWorldTouchCheckTime,
    setWorldTouches,
    setWorldXMLRequestRegistry,
    takeWQ,
    ticksToDuration,
    updatePrimFace,
    wrand,
    getScript,
    setScript,
    delScript,
    sortByInvName
    ) where

import Control.Applicative
import Control.Monad(MonadPlus(..),liftM,ap)
import Control.Monad.State(MonadState(..),StateT(..))
import Control.Monad.Error(ErrorT(..),MonadError(..))
import Data.List(elemIndex)
import Data.Map(Map)
import Data.Maybe(fromMaybe)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Set as S

import qualified Language.Lsl.Internal.AvEvents as AvEvent
import Language.Lsl.Internal.Breakpoint(BreakpointManager(..))
import Language.Lsl.Internal.Evaluation(Event(..),ScriptInfo(..),EvalResult(..))
import Language.Lsl.Internal.Exec(hasActiveHandler)
import Language.Lsl.Internal.Key(mkKey)
import Language.Lsl.Internal.Log(LogMessage(..),LogLevel(..))
import Language.Lsl.Syntax(Validity,LModule(..),CompiledLSLScript(..))
import Language.Lsl.Internal.Type(LSLValue(..),LSLType(..),vec2VVal)
import Language.Lsl.Internal.Util(mlookup,lookupByIndex,elemAt)
import Language.Lsl.WorldDef(Prim(..),PrimFace(..),InventoryItem(..),InventoryItemIdentification(..),
                    LSLObject(..),Script(..),Avatar(..),Region(..),ObjectDynamics(..),Parcel(..),
                    WebHandling(..),isInvNotecardItem,isInvLandmarkItem,isInvClothingItem,
                    isInvBodyPartItem,isInvGestureItem,isInvSoundItem,isInvAnimationItem,
                    isInvTextureItem,isInvScriptItem,isInvObjectItem,sortByInvName,findByInvName,
                    textureKey)

import System.Random(StdGen(..),Random(..))

-- a data type that defines the state of the 'world'
data World m = World {
        sliceSize :: !Int,
        maxTick :: !Int,
        nextPause :: !Int,
        wqueue :: !WorldEventQueue,
        wlisteners :: !(IM.IntMap (Listener,Bool)),
        nextListenerId :: !Int,
        wobjects :: !(Map String LSLObject),
        wprims :: !(Map String Prim),
        worldScripts :: !(Map (String,String) Script), 
        inventory :: ![(String,LSLObject)],
        tick :: !Int,
        msglog :: ![LogMessage],
        predefs :: !(Map String (PredefFunc m)),
        randGen :: !StdGen,
        wlibrary :: ![(String,Validity LModule)],
        wscripts :: ![(String,Validity CompiledLSLScript)],
        worldEventHandler :: !(Maybe (String, [(String,LSLValue Float)])),
        worldAvatars :: !(Map String Avatar),
        worldBreakpointManager :: !BreakpointManager,
        worldSuspended :: !(Maybe (String,String)), -- prim-key, script-name, image
        worldRegions :: !(Map (Int,Int) Region),
        worldZeroTime :: !Int,
        worldKeyIndex :: !Integer,
        worldWebHandling :: !WebHandling,
        worldOutputQueue :: ![SimEvent],
        worldPendingHTTPRequests :: ![String],
        worldOpenDataChannels :: !(Map String (String,String),Map (String,String) String),
        worldXMLRequestRegistry :: !(Map String XMLRequestSourceType),
        worldPhysicsTime :: !Int,
        worldTargetCheckTime :: !Int,
        worldLastPositions :: !(Map String (Bool,(Float,Float,Float))),
        worldCollisions :: !(S.Set (String,String)),
        worldLandCollisions :: !(S.Set String),
        worldTouches :: !(Map String [Touch]),
        worldTouchCheckTime :: !Int
    } deriving (Show)

-- an ErrorT/StateT/m Monad stack for the World.  The type is parameterized by
-- the innermost monad...
newtype WorldE m a = WorldE { unWorldE :: ErrorT String ((StateT (World m) m)) a }
    deriving (Monad)

instance Monad m => MonadState (World m) (WorldE m) where
   get = WorldE { unWorldE = get }
   put v = WorldE { unWorldE = put v }
   
instance Monad m => MonadError String (WorldE m) where
    throwError e = WorldE { unWorldE = throwError e }
    catchError v f = WorldE { unWorldE = catchError (unWorldE v) (unWorldE . f) }

instance Monad m => Functor (WorldE m) where
    fmap = liftM
    
instance Monad m => Applicative (WorldE m) where
   pure  = return
   (<*>) = ap

-- extracting/updating the world state -----------------------------------------------------                   
updateWorld u = put =<< liftM u get

--getSliceSize :: Monad m => WorldM m Int
getSliceSize = sliceSize <$> get

getListeners :: Monad m => WorldE m (IM.IntMap (Listener,Bool))
getListeners = wlisteners <$> get
getNextPause :: Monad m => WorldE m Int
getNextPause = nextPause <$> get
getTick :: Monad m => WorldE m Int
getTick = tick <$> get
getNextListenerId :: Monad m => WorldE m Int
getNextListenerId = nextListenerId <$> get
getObjects :: Monad m => WorldE m (Map String LSLObject)
getObjects = wobjects <$> get
getPrims :: Monad m => WorldE m (Map String Prim)
getPrims = wprims <$> get
getWorldScripts :: Monad m => WorldE m (Map (String,String) Script)
getWorldScripts = worldScripts <$> get
getMsgLog :: Monad m => WorldE m [LogMessage]
getMsgLog = msglog <$> get
getWQueue :: Monad m => WorldE m WorldEventQueue
getWQueue = wqueue <$> get
getPredefFuncs :: Monad m => WorldE m (Map String (PredefFunc m))
getPredefFuncs = predefs <$> get
getRandGen :: Monad m => WorldE m StdGen
getRandGen = randGen <$> get
getWScripts :: Monad m => WorldE m  [(String,Validity CompiledLSLScript)]
getWScripts = wscripts <$> get
getWLibrary :: Monad m => WorldE m  [(String,Validity LModule)]
getWLibrary = wlibrary <$> get
getWorldAvatars :: Monad m => WorldE m (Map String Avatar)
getWorldAvatars = worldAvatars <$> get
getWorldBreakpointManager :: Monad m => WorldE m BreakpointManager
getWorldBreakpointManager = worldBreakpointManager <$> get
getWorldSuspended :: Monad m => WorldE m (Maybe (String,String))
getWorldSuspended = worldSuspended <$> get
getWorldRegions :: Monad m => WorldE m (Map (Int,Int) Region)
getWorldRegions = worldRegions <$> get
getWorldZeroTime :: Monad m => WorldE m Int
getWorldZeroTime = worldZeroTime <$> get
getWorldKeyIndex :: Monad m => WorldE m Integer
getWorldKeyIndex = worldKeyIndex <$> get
getWorldWebHandling :: Monad m => WorldE m WebHandling
getWorldWebHandling = worldWebHandling <$> get
getWorldOutputQueue :: Monad m => WorldE m [SimEvent]
getWorldOutputQueue = worldOutputQueue <$> get
getWorldPendingHTTPRequests :: Monad m => WorldE m [String]
getWorldPendingHTTPRequests = worldPendingHTTPRequests <$> get
getWorldOpenDataChannels :: Monad m => WorldE m (Map String (String,String), Map (String,String) String)
getWorldOpenDataChannels = worldOpenDataChannels <$> get
getWorldEventHandler :: Monad m => WorldE m (Maybe (String,[(String,LSLValue Float)]))
getWorldEventHandler = worldEventHandler <$> get
getWorldXMLRequestRegistry :: Monad m => WorldE m (Map String XMLRequestSourceType)
getWorldXMLRequestRegistry = worldXMLRequestRegistry <$> get
getWorldPhysicsTime :: Monad m => WorldE m Int
getWorldPhysicsTime = worldPhysicsTime <$> get
getWorldTargetCheckTime :: Monad m => WorldE m Int
getWorldTargetCheckTime = worldTargetCheckTime <$> get
getWorldLastPositions :: Monad m => WorldE m (Map String (Bool,(Float,Float,Float)))
getWorldLastPositions = worldLastPositions <$> get
getWorldCollisions :: Monad m => WorldE m (S.Set (String,String))
getWorldCollisions = worldCollisions <$> get
getWorldLandCollisions :: Monad m => WorldE m (S.Set (String))
getWorldLandCollisions = worldLandCollisions <$> get
getWorldTouches :: Monad m => WorldE m (Map String [Touch])
getWorldTouches = worldTouches <$> get
getWorldTouchCheckTime :: Monad m => WorldE m Int
getWorldTouchCheckTime = worldTouchCheckTime <$> get

getWorldAvatar k = getWorldAvatars >>= (\ m -> case M.lookup k m of
    Nothing -> throwError ("no such avatar/agent: " ++ k)
    Just av -> return av)
    
getRegion index = (getWorldRegions >>= mlookup index)

getPrim k = (getPrims >>= (\ m -> case M.lookup k m of
    Nothing -> throwError ("no such prim: " ++ k)
    Just prim -> return prim))
    
--getPrimVal k f = (lift getPrims >>= M.lookup k >>= return . f)
getPrimVal k f = getPrim k >>= return . f
getPrimName k = getPrimVal k primName
getPrimPosition k = getPrimVal k primPosition
getPrimParent k = getPrimVal k primParent
getRootPrim k = getPrimVal k primParent >>= \ val -> return $ fromMaybe k val
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
getPrimPendingEmails k = getPrimVal k primPendingEmails

getPrimInventory k = getPrimVal k primInventory

getPrimNotecards k = filter isInvNotecardItem <$> getPrimInventory k
getPrimLandmarks k = filter isInvLandmarkItem <$> getPrimInventory k
getPrimClothing k = filter isInvClothingItem <$> getPrimInventory k
getPrimBodyParts k = filter isInvBodyPartItem <$> getPrimInventory k
getPrimObjects k = filter isInvObjectItem <$> getPrimInventory k
getPrimGestures k = filter isInvGestureItem <$> getPrimInventory k
getPrimSounds k = filter isInvSoundItem <$> getPrimInventory k
getPrimAnimations k = filter isInvAnimationItem <$> getPrimInventory k
getPrimTextures k = filter isInvTextureItem <$> getPrimInventory k
getPrimScripts k = filter isInvScriptItem <$> getPrimInventory k

getActualPrimScripts k = do
    scriptNames <- map (fst . inventoryItemNameKey . inventoryItemIdentification) <$> getPrimScripts k
    allScripts <- M.toList <$> getWorldScripts
    return [ s | s@((pk,sn),_) <- allScripts, pk == k && sn `elem` scriptNames ]
    
getPrimLinkNum pk = do
    mp <- getPrimParent pk
    case mp of
        Nothing -> do  -- this is the root prim
            links <- getObjects >>= mlookup pk >>= return . primKeys
            return (if null links then 0 else 1)
        Just ok -> do
            links <- getObjects >>= mlookup ok >>= return . primKeys
            case elemIndex pk links of
                Nothing -> throwError "internal error, can't find prim in link list of parent object"
                Just i -> return (i + 1)
-- TODO: temp until introduce region into Prim definition
getPrimRegion _ = return (0 :: Int, 0 :: Int)

getPos pkey = runErrPrim pkey
                  (VVal 0.0 0.0 0.0)
                  (vec2VVal <$> (getRootPrim pkey >>= getObjectPosition))

runErrFace k i defaultVal = runAndLogIfErr 
    ("face " ++ (show i) ++ " or prim " ++ k ++ " not found") defaultVal

getPrimFace k i = getPrimFaces k >>= (elemAt i)
getPrimFaceAlpha k i = faceAlpha <$> getPrimFace k i
getPrimFaceColor k i = faceColor <$> getPrimFace k i
getPrimFaceTextureInfo k i = faceTextureInfo <$> getPrimFace k i

setListeners l = updateWorld (\w -> w { wlisteners = l })   
setNextListenerId i = updateWorld (\w -> w { nextListenerId = i })
setObjects os = updateWorld (\w -> w { wobjects = os })
setPrims ps = updateWorld (\w -> w { wprims = ps })
setTick t = t `seq` updateWorld (\w -> w { tick = t })
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
setWorldPhysicsTime t = updateWorld (\ w -> w { worldPhysicsTime = t })
setWorldLastPositions p = updateWorld (\ w -> w { worldLastPositions = p })
setWorldCollisions s = updateWorld (\ w -> w { worldCollisions = s })
setWorldLandCollisions s = updateWorld (\ w -> w { worldLandCollisions = s })
setWorldTouches ts = updateWorld (\ w -> w { worldTouches = ts })
setWorldTouchCheckTime t = updateWorld (\ w -> w { worldTouchCheckTime = t })

setWorldAvatar k av = getWorldAvatars >>= return . M.insert k av >>= setWorldAvatars

setPrim k p = (getPrims >>= return . (M.insert k p) >>= setPrims)

updatePrimVal k f = runErrPrim k () $ (getPrims >>= mlookup k >>= return . f >>= (setPrim k))

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
setPrimStatus k v =  updatePrimVal k (\ p -> p { primStatus = v } )
setPrimVehicleFlags k v =  updatePrimVal k (\ p -> p { primVehicleFlags = v } )
setPrimLight k v =  updatePrimVal k (\ p -> p { primLight = v } )
setPrimTempOnRez k v =  updatePrimVal k (\ p -> p { primTempOnRez = v } )
setPrimSitTarget k v =  updatePrimVal k (\ p -> p { primSitTarget = v })
setPrimSittingAvatar k v =  updatePrimVal k (\ p -> p { primSittingAvatar = v } )
setPrimPassTouches k v  =  updatePrimVal k (\ p -> p { primPassTouches = v } )
setPrimPassCollisions k v  = updatePrimVal k (\ p -> p { primPassCollisions = v } )
setPrimPayInfo k v = updatePrimVal k (\ p -> p { primPayInfo = v } )
setPrimAttachment k v =  updatePrimVal k (\ p -> p { primAttachment = v } )
setPrimInventory k v =  updatePrimVal k (\ p -> p { primInventory = v } )
setPrimPendingEmails k v =  updatePrimVal k (\ p -> p { primPendingEmails = v })
setPrimRemoteScriptAccessPin k v = updatePrimVal k (\ p -> p { primRemoteScriptAccessPin = v })

setRegion regionIndex region = setWorldRegions =<< M.insert regionIndex region <$> getWorldRegions

updatePrimFace k i f = do
    faces <- getPrimFaces k
    let faces' = zipWith (\ face index -> if (index == i) then f face else face) faces [0..]
    setPrimFaces k faces'
    
setPrimFaceAlpha k i v = runErrFace k i () $ updatePrimFace k i (\ f -> f { faceAlpha = v })
setPrimFaceColor k i v = runErrFace k i () $ updatePrimFace k i (\ f -> f { faceColor = v })

data WorldEventType = 
          CreatePrim { wePrimName :: String, wePrimKey :: String }
        | AddScript (String,String) String Bool -- script, prim key, activate
        | ResetScript String String -- prim key, script name
        | ResetScripts String -- object name
        | WorldSimEvent {
            worldSimEventName :: String,
            worldSimEventArgs :: [SimEventArg] }
        | DeferredScriptEvent { 
            deferredScriptEvent :: Event Float,
            deferredScriptEventTarget :: DeferredScriptEventTarget }
        | Chat { 
            chatChannel :: Int,
            chatterName :: String,
            chatterKey :: String,
            chatMessage :: String,
            chatLocation :: ((Int,Int),(Float,Float,Float)),
            chatRange :: Maybe Float }
        | TimerEvent { 
            timerEventInterval :: Float,
            timerAddress :: (String,String) }
        | PermissionRequestEvent {
            permissionRequestPrim :: String,
            permissionRequestScript :: String,
            permissionRequestAgent :: String,
            permissionRequestMask :: Int }
        | SensorEvent { 
            sensorAddress :: (String,String),
            sensorSenseName :: String,
            sensorSenseKey :: String,
            sensorSenseType :: Int,
            sensorSenseRange :: Float,
            sensorSenseArc :: Float,
            sensorRepeat :: Maybe Float }
        | XMLRequestEvent {
            xmlRequestSource :: XMLRequestSourceType,
            xmlRequestChannel :: String,
            xmlRequestIData :: Int,
            xmlRequestSData :: String }
        | HTTPRequestEvent {
            httpRequestSource :: (String,String),
            httpRequestKey :: String,
            httpRequestURL :: String,
            httpRequestMethod :: String,
            httpRequestMimetype :: String,
            httpRequestBodyMaxlength :: Int,
            httpRequestVerifyCert :: Int,
            httpRequestBody :: String }
        | XMLReplyEvent {
            xmlRequestKey :: String,
            xmlRequestChannel :: String,
            xmlRequestMessageId :: String,
            xmlRequestSData :: String,
            xmlRequestIData :: Int }
        | DialogEvent {
            dialogAgent :: String,
            dialogMessage :: String,
            dialogButtons :: [String],
            dialogChannel :: Int,
            dialogSourceObject :: String }
        | RezObjectEvent {
            rezObjectLinkSet :: [Prim],
            rezObjectPos :: (Float,Float,Float),
            rezObjectVel :: (Float,Float,Float),
            rezObjectRot :: (Float,Float,Float,Float),
            rezObjectStartParam :: Int,
            rezObjectRezzer :: String,
            rezObjectCopy :: Bool,
            rezObjectAtRoot :: Bool }
        | ResetScriptEvent {
            resetScriptPrimKey :: String,
            resetScriptScriptName :: String }
        | DetachCompleteEvent {
            detachObject :: String,
            detachAvatar :: String }
        | GiveAvatarInventoryEvent { 
            giveAvatarInventoryKey :: String,
            giveAvatarInventoryItem :: InventoryItem }
        | AvatarOutputEvent {
            avatarOutputEventKey :: String,
            avatarOutputEventVal :: AvEvent.AvatarOutputEvent }
        | AvatarInputEvent {
            avatarInputEventKey :: String,
            avatarInputEventVal :: AvEvent.AvatarInputEvent }
    deriving (Show)

data XMLRequestSourceType = XMLRequestInternal { xmlRequestTag :: String }
                          | XMLRequestExternal { xmlRequestTag :: String }
    deriving (Show)

data DeferredScriptEventTarget = 
      -- pushes to a specific script in a prim
      DeferredScriptEventScriptTarget (String,String)
      -- pushes to all scripts in prim
    | DeferredScriptEventPrimTarget String
      -- pushes to all scripts in all prims in object
    | DeferredScriptEventObjectTarget String
    deriving (Show)
    

data Touch = Touch {
    touchAvatarKey :: String , 
    touchPrimKey :: String, 
    touchFace :: Int, 
    touchST :: (Float,Float), 
    touchStartTick :: Int, 
    touchEndTick :: Int  }
    deriving (Show)
    
isSensorEvent (SensorEvent {}) = True
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

putWorldEvent delay we = 
    do weq <- getWQueue
       t <- getTick
       setWQueue $ putWQ (t + max (durationToTicks delay) 1) we weq

pushDeferredScriptEvent event pk sn delay = 
    putWorldEvent delay (DeferredScriptEvent event (DeferredScriptEventScriptTarget (pk,sn)))
pushDeferredScriptEventToPrim event pk delay =
    putWorldEvent delay (DeferredScriptEvent event (DeferredScriptEventPrimTarget pk))
pushDeferredScriptEventToObject event oid delay =
    putWorldEvent delay (DeferredScriptEvent event (DeferredScriptEventObjectTarget oid))

pushChangedEventToObject oid val = pushDeferredScriptEventToObject (Event "changed" [IVal val] M.empty) oid 0
pushAttachEvent pk k = pushDeferredScriptEventToPrim (Event "attach" [KVal k] M.empty) pk 0
  
putHTTPTimeoutEvent pk sn key = 
    pushDeferredScriptEvent (Event "http_response" [KVal key, IVal 499, LVal [], SVal ""] M.empty) pk sn 
putHTTPResponseEvent pk sn key status metadata body =
    pushDeferredScriptEvent (Event "http_response" [KVal key, IVal status, LVal metadata, body] M.empty) pk sn


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
    
type Predef m = ScriptInfo Float -> [LSLValue Float] -> WorldE m (EvalResult,LSLValue Float)
data PredefFunc m = PredefFunc { predefFuncName :: String, 
                                 predefFuncResultType :: LSLType, 
                                 predef :: Predef m }
     deriving (Show)

instance Monad m => Show (Predef m) where
    showsPrec _ _ = showString "(function :: ScriptInfo -> [LSLValue] -> WorldE m (EvalResult,LSLValue))"

evalWorldE :: Monad m => WorldE m v -> StateT (World m) m (Either String v)
evalWorldE = runErrorT . unWorldE

fromWorldE def val =
    val `catchError` const (return def)

runAndLogIfErr msg def val = 
    val `catchError` (\ s -> 
        logAMessage LogWarn "sim" (msg ++ " (" ++ s ++ ")") >> return def)

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
              
newKey :: Monad m => WorldE m  String
newKey = do
    i <- getWorldKeyIndex
    setWorldKeyIndex (i + 1)
    return $ mkKey i
    
findAsset _ = return Nothing

isSoundAsset _ = False

findTextureAsset "" = mzero
findTextureAsset _ = return $ InventoryItem (InventoryItemIdentification ("","")) undefined undefined

primHasActiveHandler pk handler =
    do scripts <- getPrimScripts pk
       images <- imagesForScripts scripts
       return $ foldl (\ x y -> x || hasActiveHandler y handler) False images
    where imagesForScripts scriptItems = do
              scripts <- runErrPrim pk [] $ getActualPrimScripts pk
              return [ image | (Script { scriptImage = image } ) <- map snd scripts ]
              
scriptHasActiveHandler pk sn handler =
    do script <- getWorldScripts >>= mlookup (pk,sn)
       return $ hasActiveHandler (scriptImage script) handler
             
lookupDataChannel scriptAddr = getWorldOpenDataChannels >>= mlookup scriptAddr . snd
lookupScriptFromChan chan = getWorldOpenDataChannels >>= mlookup chan . fst

insertScriptChannelPair script chan = do
    (c2s,s2c) <- getWorldOpenDataChannels
    setWorldOpenDataChannels (M.insert chan script c2s,M.insert script chan s2c)
    

----- MISC ----
durationToTicks dur = floor (1000.0 * dur)
ticksToDuration ticks = fromIntegral ticks / 1000.0

wrand :: (Monad m, Random a) => WorldE m a
wrand = do g <- getRandGen
           let (v,g') = random g
           setRandGen g'
           return v

setObject oid obj = getObjects >>= setObjects . M.insert oid obj

getObject name = getObjects >>= mlookup name

getObjectDynamics = liftM objectDynamics . getObject
setObjectDynamics k d = getObject k >>= \ o -> setObject k o { objectDynamics = d }
getObjectPosition = liftM objectPosition . getObjectDynamics
getObjectRotation = liftM objectRotation . getObjectDynamics
getObjectVelocity = liftM objectVelocity . getObjectDynamics

getScript = curry ((getWorldScripts >>=) . mlookup)
setScript pk sn s = liftM (M.insert (pk,sn) s) getWorldScripts >>= setWorldScripts
delScript pk sn = getWorldScripts >>= setWorldScripts . M.delete (pk,sn)

getParcelByPosition regionIndex (x,y,_) = 
    getRegion regionIndex >>= findParcel 0 . regionParcels
    where findParcel _ [] = throwError "parcel not found" -- mzero
          findParcel i (p:ps) =
              let (south,north,west,east) = parcelBoundaries p
                  (xc,yc) = (floor x, floor y) in
                  if xc < east && xc >= west && yc < north && yc >= south
                      then return (i,p) else findParcel (i + 1) ps

getPrimParcel pk = do
    regionIndex <- getPrimRegion pk
    pos <- getObjectPosition =<< getRootPrim pk
    (index,parcel) <- getParcelByPosition regionIndex pos
    return (regionIndex,index,parcel)
    
putParcel regionIndex index parcel = do
    region <- getRegion regionIndex
    let (before,after) = splitAt index (regionParcels region)
    let parcels' = if null after then parcel : before else before ++ (parcel : tail after)
    setRegion regionIndex $ region { regionParcels = parcels' }

findTexture pk id =
    do  textures <- getPrimTextures pk
        case findByInvName id textures of
            Just item -> return $ snd $ inventoryItemNameKey $ inventoryItemIdentification item
            Nothing -> do
                result <- return $ findTextureAsset id
                case result of
                    Nothing -> throwError ("cannot find texture " ++ id)
                    Just item -> return $ snd $ inventoryItemNameKey $ inventoryItemIdentification item
                    
setTexture tk face pkey =
    if face == -1
        then do faces <- getPrimFaces pkey
                let faces' = map (\ face -> 
                        let info = faceTextureInfo face in 
                            face { faceTextureInfo = info { textureKey = tk } }) faces
                setPrimFaces pkey faces'
        else do faces <- getPrimFaces pkey
                f <- lookupByIndex face faces
                let tInfo = faceTextureInfo f
                updatePrimFace pkey face (\ f -> f { faceTextureInfo = tInfo { textureKey = tk } })
