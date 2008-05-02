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

import Lsl.Avatar
import Lsl.Breakpoint
import Lsl.Builder
import Lsl.CodeHelper
import Lsl.Constants
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
                    worldAvatars :: Map String Avatar,
                    worldBreakpointManager :: BreakpointManager,
                    worldSuspended :: Maybe (String,String), -- prim-key, script-name, image
                    worldRegions :: Map (Int,Int) Region,
                    worldZeroTime :: Int,
                    worldKeyIndex :: Integer
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
getRegion index = (lift getWorldRegions >>= M.lookup index)

getPrim k = (lift getPrims >>= M.lookup k)
getPrimVal k f = (lift getPrims >>= M.lookup k >>= return . f)
getPrimName k = getPrimVal k primName
getPrimPosition k = getPrimVal k primPosition
getPrimParent k = getPrimVal k primParent
getPrimDescription k = getPrimVal k primDescription
getPrimOwner k = getPrimVal k primOwner
getPrimCreator k = getPrimVal k primCreator
getPrimRotation k = getPrimVal k primRotation
getPrimScale k = getPrimVal k primScale
getPrimFaces k = getPrimVal k primFaces
getPrimFlexibility k = getPrimVal k primFlexibility
getPrimMaterial k = getPrimVal k primMaterial
getPrimStatus k = getPrimVal k primStatus
getPrimLight k = getPrimVal k primLight
getPrimTempOnRez k = getPrimVal k primTempOnRez
getPrimTypeInfo k = getPrimVal k primTypeInfo
getPrimPermissions k = getPrimVal k primPermissions
getPrimSitTarget k = getPrimVal k primSitTarget
getPrimSittingAvatar k = getPrimVal k primSittingAvatar
getPrimAttachment k = getPrimVal k primAttachment

getPrimNotecards k = getPrimVal k notecards
getPrimLandmarks k = getPrimVal k primLandmarks
getPrimClothing k = getPrimVal k primClothing
getPrimGestures k = getPrimVal k primGestures
getPrimAnimations k = getPrimVal k animations
getPrimTextures k = getPrimVal k textures
getPrimSounds k = getPrimVal k sounds
getPrimBodyParts k = getPrimVal k primBodyParts
getPrimObjects k = getPrimVal k inventoryObjects
getPrimScripts k = getPrimVal k primScripts

getInventoryInfo f k = (f k >>= return . inventoryItemInfoMap)
getPrimNotecardInfo k = getInventoryInfo getPrimNotecards k
getPrimLandmarkInfo k = getInventoryInfo getPrimLandmarks k
getPrimClothingInfo k = getInventoryInfo getPrimClothing k
getPrimBodyPartInfo k = getInventoryInfo getPrimBodyParts k
getPrimObjectInfo k = getInventoryInfo getPrimObjects k
getPrimGestureInfo k = getInventoryInfo getPrimGestures k
getPrimSoundInfo k = getInventoryInfo getPrimSounds k
getPrimAnimationInfo k = getInventoryInfo getPrimAnimations k
getPrimTextureInfo k = getInventoryInfo getPrimTextures k
getPrimScriptInfo k = getInventoryInfo getPrimScripts k

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

setPrim k p = (getPrims >>= return . (M.insert k p) >>= setPrims)
updatePrimVal k f = (lift getPrims >>= M.lookup k >>= return . f >>= lift . (setPrim k))

runErrPrim k defaultVal = runAndLogIfErr ("prim " ++ k ++ " not found") defaultVal
setPrimPosition k v = runErrPrim k () $ updatePrimVal k (\ p -> p { primPosition = v } )
setPrimRotation k v = runErrPrim k () $ updatePrimVal k (\ p -> p { primRotation = v } )
setPrimScale k v = runErrPrim k () $ updatePrimVal k (\ p -> p { primScale = v } )
setPrimDescription k v = runErrPrim k () $ updatePrimVal k (\ p -> p { primDescription = v } )
setPrimName k v = runErrPrim k () $ updatePrimVal k (\ p -> p { primName = v } )
setPrimParent k v = runErrPrim k () $ updatePrimVal k (\ p -> p { primParent = v } )
setPrimFaces k v = runErrPrim k () $ updatePrimVal k (\ p -> p { primFaces = v } )
setPrimFlexibility k v = runErrPrim k () $ updatePrimVal k (\ p -> p { primFlexibility = v } )
setPrimMaterial k v = runErrPrim k () $ updatePrimVal k (\ p -> p { primMaterial = v } )
setPrimOwner k v = runErrPrim k () $ updatePrimVal k (\ p -> p { primOwner = v } )
setPrimStatus k v = runErrPrim k () $ updatePrimVal k (\ p -> p { primStatus = v } )
setPrimLight k v = runErrPrim k () $ updatePrimVal k (\ p -> p { primLight = v } )
setPrimTempOnRez k v = runErrPrim k () $ updatePrimVal k (\ p -> p { primTempOnRez = v } )
setPrimTypeInfo k v = runErrPrim k () $ updatePrimVal k (\ p -> p { primTypeInfo = v } )
setPrimPermissions k v = runErrPrim k () $ updatePrimVal k (\ p -> p { primPermissions = v } )
setPrimSitTarget k v = runErrPrim k () $ updatePrimVal k (\ p -> p { primSitTarget = v })
setPrimSittingAvatar k v = runErrPrim k () $ updatePrimVal k (\ p -> p { primSittingAvatar = v } )
setPrimAttachment k v = runErrPrim k () $ updatePrimVal k (\ p -> p { primAttachment = v } )

setRegion regionIndex region = getWorldRegions >>= return . (M.insert regionIndex region) >>= setWorldRegions

updatePrimFace k i f = do
    faces <- getPrimFaces k
    let faces' = zipWith (\ face index -> if (index == i) then f face else face) faces [0..]
    lift $ setPrimFaces k faces'
    
setPrimFaceAlpha k i v = runErrFace k i () $ updatePrimFace k i (\ f -> f { faceAlpha = v })
setPrimFaceColor k i v = runErrFace k i () $ updatePrimFace k i (\ f -> f { faceColor = v })

insertPrimData key path value = do
    prims <- getPrims
    case M.lookup key prims of
        Nothing -> logAMessage LogWarn "sim" ("key " ++ key ++ " not found")
        Just prim -> 
            setPrims $ M.insert key (prim { primData = (insertDB path value (primData prim)) }) prims

lookupPrimData key path = do
    prims <- getPrims
    case M.lookup key prims of
        Nothing -> return Nothing
        Just prim -> return $ lookupDB path (primData prim)
 
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
                    | Chat { chatChannel :: Int, chatterName :: String, chatterKey :: String, chatMessage :: String,
                             chatLocation :: ((Int,Int),(Float,Float,Float)),
                             chatRange :: Maybe Float }
                    | TimerEvent { timerEventInterval :: Float,
                                   timerAddress :: (String,String) }
                    | PermissionRequestEvent { permissionRequestPrim :: String,
                                               permissionRequestScript :: String,
                                               permissionRequestAgent :: String,
                                               permissionRequestMask :: Int }
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