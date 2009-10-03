{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances,
             MultiParamTypeClasses, GeneralizedNewtypeDeriving,
             NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fwarn-unused-binds -fwarn-unused-imports #-}
module Language.Lsl.WorldDef(
    Avatar(..),
    AvatarControlListener(..),
    Prim(..),
    PrimFace(..),
    TextureInfo(..),
    Flexibility(..),
    LightInfo(..),
    LSLObject(..),
    ObjectDynamics(..),
    PositionTarget(..),
    RotationTarget(..),
    PrimType(..),
    FullWorldDef(..),
    Region(..),
    Parcel(..),
    InventoryItemIdentification(..),
    InventoryItem(..),
    InventoryInfo(..),
    InventoryItemData(..),
    Script(..),
    ScriptId,
    Attachment(..),
    CameraParams(..),
    Email(..),
    WebHandling(..),
    -- imported types
    LSLValue(..),
    Event(..),
    ScriptImage,
    defaultAvatar,
    defaultCamera,
    defaultDynamics,
    isInvScriptItem,
    isInvBodyPartItem,
    isInvGestureItem,
    isInvClothingItem,
    isInvTextureItem,
    isInvSoundItem,
    isInvAnimationItem,
    isInvLandmarkItem,
    isInvNotecardItem,
    isInvObjectItem,
    inventoryInfoPermValue,
    defaultInventoryPermissions,
    inventoryItemName,
    inventoryItemNames,
    scriptInventoryItem,
    findByInvName,
    findByInvKey,
    emptyPrim,
    primPhantomBit,
    primPhysicsBit,
    sortByInvName,
    worldFromFullWorldDef,
    defaultRegions,
    mkScript,
    world,
    AcceptState(..),
    AcceptContext(..),
    evalAcceptContext,
    newAcceptState) where

import Control.Applicative
import Control.Monad(when,foldM,ap,liftM)
import Control.Monad.Error(MonadError(..),ErrorT(..))
import Control.Monad.Writer(tell,lift,runWriterT)

import qualified Control.Monad.State as SM(MonadState(..),State,runState,evalState)
import Data.List(find,sortBy)
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Maybe(isNothing)

import Language.Lsl.Internal.DOMProcessing(req,opt,def,val,text,elist,
    liste,choicet,Element(..),Posn,ElementAcceptContext(..))
import Language.Lsl.Internal.Evaluation(Event(..))
import Language.Lsl.Internal.Exec(ScriptImage,initLSLScript)
import Language.Lsl.Internal.Key(mkKey,nullKey)
import Language.Lsl.Internal.Type(LSLValue(..))
import Language.Lsl.Internal.Util(
    mlookup,Permutation3(..),rotationsToQuaternion)

type ScriptId = (String,String)

data FullWorldDef = FullWorldDef {
    fullWorldDefMaxTime :: Int,
    fullWorldDefSliceSize :: Int,
    fullWorldDefWebHandling :: WebHandling,
    fullWorldDefEventHandler :: Maybe String,
    fullWorldDefObjects :: [LSLObject],
    fullWorldDefPrims :: [Prim],
    fullWorldDefAvatars :: [Avatar],
    fullWorldDefRegions :: [((Int,Int),Region)],
    fullWorldDefInitialKeyIndex :: Integer } deriving (Show)

data WebHandling = 
     WebHandlingByFunction 
   | WebHandlingByDoingNothing
   | WebHandlingByInternet { webHandlingTimeout :: Float } deriving (Show)
                             
data LSLObject = LSLObject { 
    primKeys :: [String], 
    objectDynamics :: !ObjectDynamics } deriving (Show)

data ObjectDynamics = ObjectDynamics {
    objectPosition :: (Float,Float,Float),
    objectRotation :: (Float,Float,Float,Float),
    objectVelocity :: (Float,Float,Float),
    objectForce :: ((Float,Float,Float),Bool),
    objectBuoyancy :: Float,
    objectImpulse :: (((Float,Float,Float),Bool),Int),
    objectTorque :: ((Float,Float,Float),Bool),
    objectRotationalImpulse :: (((Float,Float,Float),Bool),Int),
    objectOmega :: (Float,Float,Float),
    objectPositionTarget :: !(Maybe PositionTarget),
    objectRotationTarget :: !(Maybe RotationTarget),
    objectVolumeDetect :: Bool
    } deriving (Show)
                      
defaultDynamics = ObjectDynamics { 
    objectPosition = (0,0,0),
    objectRotation = (0,0,0,1),
    objectVelocity = (0,0,0), 
    objectForce = ((0,0,0),False),
    objectBuoyancy = 0,
    objectImpulse = (((0,0,0),False),0),
    objectTorque = ((0,0,0),False),
    objectRotationalImpulse = (((0,0,0),False),0),
    objectOmega = (0,0,0),
    objectPositionTarget = Nothing,
    objectRotationTarget = Nothing,
    objectVolumeDetect = False }
data PositionTarget = 
      Repel { 
        positionTargetTau :: Float, 
        positionTargetOverWater :: Bool, 
        positionTargetHeight :: Float }
    | Hover { 
        positionTargetTau :: Float, 
        positionTargetOverWater :: Bool, 
        positionTargetHeight :: Float }
    | PositionTarget { 
        positionTargetTau :: Float, 
        positionTargetLocation :: (Float,Float,Float), 
        positionTargetSetBy :: ScriptId }
    deriving (Show)
    
data RotationTarget = RotationTarget {
    rotationTarget :: (Float,Float,Float,Float),
    rotationTargetStrength :: Float,
    rotationTargetTau :: Float } deriving (Show)
                                       
data Avatar = Avatar { 
    avatarKey :: String,
    avatarName :: String,
    avatarActiveGroup :: Maybe String,
    avatarRegion :: (Int,Int),
    avatarPosition :: (Float,Float,Float),
    avatarRotation :: (Float,Float,Float,Float),
    avatarHeight :: Float,
    avatarState :: Int,
    avatarInventory :: [InventoryItem],
    avatarCameraPosition :: (Float,Float,Float),
    avatarCameraRotation :: (Float,Float,Float,Float),
    avatarCameraControlParams :: CameraParams,
    avatarActiveAnimations :: [(Maybe Int,String)],
    avatarAttachments :: IM.IntMap String,
    avatarEventHandler :: !(Maybe (String,[(String,LSLValue Float)])),
    avatarControls :: !Int,
    avatarControlListener :: !(Maybe AvatarControlListener) } deriving (Show)

data AvatarControlListener = AvatarControlListener { 
    avatarControlListenerMask :: !Int,
    avatarControlListenerScript :: !(String,String) } deriving (Show)

data CameraParams = CameraParams { 
    cameraActive :: Bool,
    cameraBehindednessAngle :: Float,
    cameraBehindednessLag :: Float,
    cameraDistance :: Float,
    cameraFocus :: Maybe (Float,Float,Float),
    cameraFocusLag :: Float,
    cameraFocusLocked :: Bool,
    cameraFocusOffset :: (Float,Float,Float),
    cameraFocusThreshold :: Float,
    cameraPitch :: Float,
    cameraPosition :: Maybe (Float,Float,Float),
    cameraPositionLag :: Float,
    cameraPositionLocked :: Bool,
    cameraPositionThreshold :: Float } deriving Show
defaultCamera = CameraParams {
    cameraActive = False,
    cameraBehindednessAngle = 10.0,
    cameraBehindednessLag = 0,
    cameraDistance = 3.0,
    cameraFocus = Nothing,
    cameraFocusLag = 0,
    cameraFocusLocked = False,
    cameraFocusOffset = (0,0,0),
    cameraFocusThreshold = 1.0,
    cameraPitch = 0.0,
    cameraPosition = Nothing,
    cameraPositionLag = 0.1,
    cameraPositionLocked = False,
    cameraPositionThreshold = 1 }
                               
defaultAvatar key = Avatar {
        avatarKey = key,
        avatarName = "Default Avatar",
        avatarActiveGroup = Nothing,
        avatarRegion = (0,0),
        avatarPosition = (128.0,128.0,0.0),
        avatarRotation = (0.0, 0.0, 0.0, 1.0),
        avatarHeight = 2,
        avatarState = 0,
        avatarInventory = [],
        avatarCameraPosition = (128.0,128.0,0.0),
        avatarCameraRotation = (0,0,0,1),
        avatarCameraControlParams = defaultCamera,
        avatarActiveAnimations = [],
        avatarAttachments = IM.empty,
        avatarEventHandler = Nothing,
        avatarControls = 0,
        avatarControlListener = Nothing }
                       
-- these are bit INDEXES not MASKS (0 == least significant bit)
primPhantomBit :: Int
primPhantomBit = 4
primPhysicsBit :: Int
primPhysicsBit = 0

data InventoryItemData = 
      InvScript { invScriptLibId :: String, invScriptState :: Maybe (ScriptImage Float) }
    | InvBodyPart
    | InvGesture
    | InvClothing
    | InvTexture
    | InvSound { invSoundDuration :: Float }
    | InvAnimation { invAnimationDuration :: Maybe Float }
    | InvLandmark { invLandmarkLocation :: ((Int,Int),(Float,Float,Float)) }
    | InvNotecard { invNotecardLines :: [String] }
    | InvObject { invObjectPrims :: [Prim] } deriving (Show)
    
isInvScript (InvScript _ _) = True
isInvScript _ = False
isInvBodyPart InvBodyPart = True
isInvBodyPart _ = False
isInvGesture InvGesture = True
isInvGesture _ = False
isInvClothing InvClothing = True
isInvClothing _ = False
isInvTexture InvTexture = True
isInvTexture _ = False
isInvSound (InvSound _) = True
isInvSound _ = False
isInvAnimation (InvAnimation _) = True
isInvAnimation _ = False
isInvLandmark (InvLandmark _) = True
isInvLandmark _ = False
isInvNotecard (InvNotecard _) = True
isInvNotecard _ = False
isInvObject (InvObject _) = True
isInvObject _ = False

isInvScriptItem = isInvScript . inventoryItemData
isInvBodyPartItem = isInvBodyPart . inventoryItemData
isInvGestureItem = isInvGesture . inventoryItemData
isInvClothingItem = isInvClothing . inventoryItemData
isInvTextureItem = isInvTexture . inventoryItemData
isInvSoundItem = isInvSound . inventoryItemData
isInvAnimationItem = isInvAnimation . inventoryItemData
isInvLandmarkItem = isInvLandmark . inventoryItemData
isInvNotecardItem = isInvNotecard . inventoryItemData
isInvObjectItem = isInvObject . inventoryItemData
                       
newtype InventoryItemIdentification = InventoryItemIdentification {
    inventoryItemNameKey :: (String,String) } deriving (Show)
data InventoryInfo  = InventoryInfo {
    inventoryInfoCreator :: String,
    inventoryInfoPerms :: (Int,Int,Int,Int,Int) } deriving (Show)
data InventoryItem = InventoryItem {
    inventoryItemIdentification :: InventoryItemIdentification,
    inventoryItemInfo :: InventoryInfo,
    inventoryItemData :: InventoryItemData } deriving (Show)
                                     
inventoryItemName = fst . inventoryItemNameKey . inventoryItemIdentification
invnetoryItemKey = snd . inventoryItemNameKey . inventoryItemIdentification
inventoryItemNames = map inventoryItemName
scriptInventoryItem s k id = 
    InventoryItem (InventoryItemIdentification (s,k)) 
        (InventoryInfo "" defaultInventoryPermissions) (InvScript id Nothing)
findByInvName name = find ((== name) . inventoryItemName)
findByInvKey key = find ((== key) . invnetoryItemKey)

sortByInvName = sortBy (\ i i' -> compare (inventoryItemName i) (inventoryItemName i'))

inventoryInfoPermValue 0 (i,_,_,_,_) = return i
inventoryInfoPermValue 1 (_,i,_,_,_) = return i
inventoryInfoPermValue 2 (_,_,i,_,_) = return i
inventoryInfoPermValue 3 (_,_,_,i,_) = return i
inventoryInfoPermValue 4 (_,_,_,_,i) = return i
inventoryInfoPermValue i _ = throwError ("no such perm mask - " ++ show i)

defaultInventoryPermissions :: (Int,Int,Int,Int,Int)
defaultInventoryPermissions = (0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff)

data Prim = Prim {
    primName :: String,
    primKey :: String,
    primParent :: Maybe String,
    primDescription :: String,
    primInventory :: [InventoryItem],
    primOwner :: String,
    primGroup :: Maybe String,
    primCreator :: String,
    primPosition :: (Float, Float, Float),
    primRotation :: (Float, Float, Float, Float),
    primScale :: (Float, Float, Float),
    primFaces :: [PrimFace],
    primFlexibility :: Maybe Flexibility,
    primMaterial :: Int,
    primStatus :: Int,
    primVehicleFlags :: Int,
    primLight :: Maybe LightInfo,
    primTempOnRez :: Bool,
    primTypeInfo :: PrimType,
    primPermissions :: [Int],
    primAllowInventoryDrop :: Bool,
    primSitTarget :: Maybe ((Float,Float,Float),(Float,Float,Float,Float)),
    primSittingAvatar :: Maybe String,
    primPendingEmails :: [Email],
    primPassTouches :: Bool,
    primPassCollisions :: Bool,
    primPayInfo :: (Int,Int,Int,Int,Int),
    primAttachment :: Maybe Attachment,
    primRemoteScriptAccessPin :: Int } deriving (Show)

data PrimType = 
      PrimTypeUnknown
    | PrimType {
       primVersion :: Int, -- 1 or 9
       primTypeCode :: Int,
       primHoleshape :: Int,
       primCut :: (Float,Float,Float),
       primTwist :: (Float,Float,Float),
       primHolesize :: (Float,Float,Float),
       primTopshear :: (Float, Float, Float),
       primHollow :: Float,
       primTaper :: (Float,Float,Float),
       primAdvancedCut :: (Float,Float,Float),
       primRadiusOffset :: Float,
       primRevolutions :: Float,
       primSkew :: Float,
       primSculptTexture :: Maybe String,
       primSculptType :: Int
    } deriving (Show)
                   
basicBox = PrimType 9 0 0 (0,1,0) (0,0,0) (0,0,0) (0,0,0) 0 (0,0,0) (0,1,0) 0 0 0 Nothing 0

data Attachment = Attachment { 
    attachmentKey :: String, 
    attachmentPoint :: Int } deriving (Show)

data LightInfo = LightInfo {
    lightColor :: (Float,Float,Float),
    lightIntensity :: Float,
    lightRadius :: Float,
    lightFalloff :: Float
    } deriving (Show)
    
data Flexibility = Flexibility {
    flexSoftness :: Int,
    flexGravity :: Float,
    flexFriction :: Float,
    flexWind :: Float,
    flexTension :: Float,
    flexForce :: (Float,Float,Float)
    } deriving (Show)
    
data PrimFace = PrimFace {
    faceAlpha :: Float,
    faceColor :: (Float,Float,Float),
    faceShininess :: Int,
    faceBumpiness :: Int,
    faceFullbright :: Bool,
    faceTextureMode :: Int,
    faceTextureInfo :: TextureInfo 
    } deriving (Show)

defaultFace = PrimFace 1.0 (1.0,1.0,1.0) 0 0 False 0 defaultTextureInfo
               
data TextureInfo = TextureInfo {
    textureKey :: String,
    textureRepeats :: (Float,Float,Float),
    textureOffsets :: (Float,Float,Float),
    textureRotation :: Float
    } deriving (Show)

defaultTextureInfo = TextureInfo "" (1.0,1.0,0.0) (0.0,0.0,0.0) 0.0
                   
data Email = Email {
    emailSubject :: String,
    emailAddress :: String, -- sender address
    emailMessage :: String,
    emailTime :: Int } deriving (Show)
                                      
emptyPrim name key = Prim { 
    primName = name,
    primKey = key,
    primParent = Nothing,
    primDescription = "",
    primInventory = [],
    primOwner = "",
    primGroup = Nothing,
    primCreator = "",
    primPosition = (0.0,0.0,0.0),
    primRotation = (0.0,0.0,0.0,1.0),
    primScale = (1.0,1.0,1.0),
    primFaces = replicate 6 defaultFace,
    primFlexibility = Nothing,
    primMaterial = 0,
    primStatus = 0x0e,
    primVehicleFlags = 0,
    primLight = Nothing,
    primTempOnRez = False,
    primTypeInfo = basicBox,
    primPermissions = [0],            
    primAllowInventoryDrop = False,
    primSitTarget = Nothing,
    primSittingAvatar = Nothing,
    primPendingEmails = [],
    primPassTouches = False,
    primPassCollisions = False,
    primPayInfo = ( -2, -2, -2, -2, -2),
    primAttachment = Nothing,
    primRemoteScriptAccessPin = 0 }

data Region = Region {
    regionName :: String,
    regionFlags :: Int,
    regionParcels :: [Parcel]
    } deriving (Show)
    
data Parcel = Parcel {
    parcelName :: String,
    parcelDescription :: String,
    parcelBoundaries :: (Int,Int,Int,Int), -- bottom, top, left, right aka south,north,west,east
    parcelOwner :: String,
    parcelFlags :: Int,
    parcelBanList :: [(String,Maybe Int)],
    parcelPassList :: [(String,Maybe Int)]
    } deriving (Show)

defaultRegions :: String -> [((Int,Int),Region)]
defaultRegions owner =
    [(
        (0,0), 
        Region { 
            regionName = "Region_0_0", regionFlags = 0,
            regionParcels = 
                [Parcel "parcel_0" "default parcel" (0,256,0,256) owner
                    0xffffffff [] []] }
    )]

data Script = Script { 
    scriptImage :: !(ScriptImage Float),
    scriptActive :: Bool,
    scriptPermissions :: M.Map String Int,
    scriptLastPerm :: Maybe String,
    scriptStartTick :: Int,
    scriptLastResetTick :: Int,
    scriptEventQueue :: [Event Float],
    scriptStartParameter :: Int,
    scriptCollisionFilter :: !(String,String,Bool),
    scriptTargetIndex :: !Int,
    scriptPositionTargets :: !(IM.IntMap ((Float,Float,Float), Float)),
    scriptRotationTargets :: !(IM.IntMap ((Float,Float,Float,Float), Float)),
    scriptControls :: ![String] } deriving (Show)

mkScript img = Script { 
    scriptImage = img,
    scriptActive = True,
    scriptPermissions = M.empty,
    scriptLastPerm = Nothing,
    scriptStartTick = 0,
    scriptLastResetTick = 0,
    scriptEventQueue = [Event "state_entry" [] M.empty],
    scriptStartParameter = 0,
    scriptCollisionFilter = ("",nullKey,True),
    scriptTargetIndex = 0,
    scriptPositionTargets = IM.empty,
    scriptRotationTargets = IM.empty,
    scriptControls = [] }
                        
worldFromFullWorldDef worldBuilder fwd lib scripts = do
    let primMap = mkPrimMap (fullWorldDefPrims fwd)
    -- prove all the prims in all the objects exists
    primMap' <- checkObjects primMap (fullWorldDefObjects fwd)
    let filt prim = filter isInvScriptItem $ primInventory prim
    let item2Script prim item = ((primKey prim, inventoryItemName item),
            invScriptLibId $ inventoryItemData item)
    let activeScripts = concatMap 
            (\ prim -> map (item2Script prim) $ filt prim) $ M.elems primMap'
    (activatedScripts,log) <- 
        runWriterT (activateScripts activeScripts scripts primMap')
    return $ worldBuilder
        (fullWorldDefSliceSize fwd)
        (fullWorldDefMaxTime fwd)
        [] lib scripts 
        (M.fromList $ mkAvatarLookup (fullWorldDefAvatars fwd))
        (mkObjectMap (fullWorldDefObjects fwd))
        primMap'
        (M.fromList activatedScripts)
        (M.fromList (fullWorldDefRegions fwd))
        (fullWorldDefInitialKeyIndex fwd)
        (fullWorldDefWebHandling fwd)
        (fullWorldDefEventHandler fwd) 
        log

fctx :: MonadError String m => String -> Either String a -> m a
fctx s (Left s') = throwError s
fctx _ (Right v) = return v

mkPrimMap prims = M.fromList [(primKey p, p) | p <- prims]

mkObjectMap objects = 
    M.fromList [ (p, o) | o@(LSLObject { primKeys = (p:_) }) <- objects ]
mkAvatarLookup avatars = [ (avatarKey a,a) | a <- avatars]

checkObject primMap o = 
    foldM checkPrim primMap (primKeys o)
    where 
        root = head (primKeys o)
        checkPrim m k = 
            case M.lookup k m of
                Nothing -> fail ("prim " ++ k ++ " not found in definition")
                Just prim -> 
                    return (if (k == root) 
                        then m
                        else M.insert k (prim { primParent = Just root }) m)

checkObjects primMap os = foldM checkObject primMap os

activateScripts scriptIdInfo compiledScripts primMap = 
    mapM (activateScript compiledScripts primMap) scriptIdInfo >>=
        (\ ms -> return [ s | Just s <- ms ])

activateScript scripts primMap (k@(primKey,invName),(scriptID)) = do
    let script = case lookup scriptID scripts of
             Nothing -> fail "script not found"
             Just v -> v
    prim <- (lift . fctx ("looking up prim " ++ primKey ++ " failed")) 
        (mlookup primKey primMap)
    when (isNothing (findByInvName invName (primInventory prim))) $ 
        fail (invName ++ " doesn't exist in prim " ++ primKey)
    case script of
        Left ((_,s):_) -> 
            tell [("script \"" ++ invName ++ "\" in prim " ++ primKey ++ 
                  " failed to activate because of error: " ++ s)] 
            >> return Nothing
        Right code -> return $ Just (k,mkScript $ initLSLScript code)

world = do 
    handler <- opt "simEventHandler" text
    avs <- req "avatars" avatars
    ps <- req "prims" prims
    let webHandling = maybe WebHandlingByDoingNothing 
            (const WebHandlingByFunction) handler
    FullWorldDef <$> req "maxTime" val <*> req "sliceSize" val
        <*> pure webHandling <*> pure handler <*> req "objects" objects
        <*> pure ps <*> pure avs 
        <*> pure (defaultRegions "") <*> (asKeyIndex <$> SM.get)

objects = liste "object" object

object = do
    keys <- mapM findRealKey' =<< req "primKeys" (liste "string" text)
    position <- dvec0 "position"
    LSLObject <$> pure keys <*> pure defaultDynamics { objectPosition = position }
    
vec = (,,) <$> req "x" val <*> req "y" val <*> req "z" val
region = (,) <$> req "x" val <*> req "y" val

prims = liste "prim" prim

prim = do
    key <- req "key" text
    scripts <- req "scripts" (liste "script" script)
    inventory <- def "inventory" [] (elist invItem)
    owner <- findRealKey' =<< def "owner" "" text
    rotation <- rotationsToQuaternion P123 <$> dvec0 "rotation"
    Prim <$> req "name" text <*> newKey' (Just key) <*> pure Nothing
        <*> def "description" "" text <*> pure (scripts ++ inventory)
        <*> pure owner <*> pure Nothing <*> pure owner 
        <*> def "position" (128,128,0) vec <*> pure rotation <*> dvec1 "scale"
        <*> def "faces" (replicate 6 defaultFace) (liste "face" primFace)
        <*> opt "flexibility" flexibility <*> dval "material" 0
        <*> dval "status" 0x0e <*> pure 0 <*> opt "light" lightInfo 
        <*> dval "tempOnRez" False <*> def "typeInfo" basicBox primType
        <*> def "permissions" [0] (liste "int" val)
        <*> dval "dropAllowed" False <*> pure Nothing <*> pure Nothing 
        <*> pure [] <*> pure False <*> pure False <*> pure (-2,-2,-2,-2,-2)
        <*> pure Nothing <*> pure 0
        
    
    
invItem = choicet $ map (fmap inventoryItem)
     [("notecardItem",invNotecard),("textureItem", invTexture),
      ("bodyPartItem", invBodyPart), ("clothingItem", invClothing),
      ("gestureItem", invGesture), ("soundItem", invSound),
      ("animationItem", invAnimation), ("inventoryObjectItem", invObject),
      ("landmarkItem", invLandmark)]

invNotecard = InvNotecard <$> req "lines" (liste "string" text)
    
invTexture = return InvTexture
invBodyPart = return InvBodyPart
invClothing = return InvClothing
invGesture = return InvGesture
invSound = InvSound <$> req "duration" val
invAnimation = InvAnimation <$> req "duration" 
    (val >>= \ v -> return $ if v == 0 then Nothing else Just v)
    
invObject = InvObject <$> req "prims" prims
invLandmark = curry InvLandmark <$> req "region" region <*> req "position" vec

inventoryItem f = do
    id <- curry InventoryItemIdentification <$> req "name" text 
        <*> newKey' Nothing
    info <- InventoryInfo <$> req "creator" text 
        <*> pure defaultInventoryPermissions
    findRealKey' $ inventoryInfoCreator info
    InventoryItem id info <$> f

script = scriptInventoryItem <$> req "scriptName" text <*> newKey' Nothing
    <*> req "scriptId" text
    
primFace = PrimFace <$> dval "alpha" 0 <*> dvec1 "color"
    <*> dval "shininess" 0 <*> dval "bumpiness" 0 <*> dval "fullbright" False
    <*> dval "textureMode" 0 
    <*> def "textureInfo" defaultTextureInfo textureInfo
    
textureInfo = TextureInfo <$> def "name" "" text <*> dvec1 "repeats"
    <*> dvec0 "offsets" <*> dval "rotation" 0
    
flexibility = Flexibility <$> dval "softness" 0 <*> dval "gravity" 1
    <*> dval "friction" 0 <*> dval "wind" 0 <*> dval "tension" 1.0
    <*> dvec0 "force"
    
lightInfo = LightInfo <$> dvec1 "color" <*> dval "intensity" 1.0 
    <*> dval "radius" 10.0 <*> dval "falloff" 1.0
    
primType = 
    PrimType <$> dval "version" 9 <*> dval "typeCode" 0
        <*> def "holeshape" 0 val <*> dvec0 "cut" <*> dvec0 "twist"
        <*> dvec0 "holesize" <*> dvec0 "topshear" <*> dval "hollow" 0
        <*> dvec0 "taper" <*> dvec0 "advancedCut" <*> dval "radiusOffset" 0
        <*> dval "revolutions" 0 <*> dval "skew" 0 <*> opt "sculptTexture" text
        <*> dval "scupltType" 0

dvec0 s = def s (0,0,0) vec
dvec1 s = def s (1,1,1) vec
dval s v = def s v val

avatars = liste "avatar" avatar

avatar = do
    (name,x,y,z,handlerName) <- (,,,,) <$> req "name" text 
        <*> req "xPos" val <*> req "yPos" val <*> req "zPos" val
        <*> opt "avatarEventHandler" text
    key <- newKey' (Just name)
    return $ (defaultAvatar key) {
        avatarName = name, 
        avatarPosition = (x,y,z),
        avatarCameraPosition = (x,y,z), 
        avatarEventHandler = fmap (flip (,) []) handlerName }
    
data AcceptState = AcceptState {
    asKeys :: M.Map String String,
    asKeyIndex :: Integer,
    asElement :: Element Posn }

newKey' :: Maybe String -> AcceptContext String
newKey' xref = do
    s <- SM.get
    let k = mkKey (asKeyIndex s)
    let m' = case xref of
                 Nothing -> asKeys s
                 Just v -> M.insert v k (asKeys s)
    SM.put s { asKeys = m', asKeyIndex = 1 + asKeyIndex s }
    return k

findRealKey' k = asKeys <$> SM.get >>= mlookup k

newAcceptState e = AcceptState M.empty 1 e
 
newtype AcceptContext a = AcceptContext { unAcceptContext :: ErrorT String (SM.State AcceptState) a }
    deriving (Monad)

instance SM.MonadState AcceptState AcceptContext where
   get = AcceptContext { unAcceptContext = SM.get }
   put v = AcceptContext { unAcceptContext = SM.put v }
   
instance MonadError String AcceptContext where
    throwError e = AcceptContext { unAcceptContext = throwError e }
    catchError v f = AcceptContext { unAcceptContext = catchError (unAcceptContext v) (unAcceptContext . f) }

instance Functor AcceptContext where
    fmap = liftM
    
instance Applicative AcceptContext where
   pure  = return
   (<*>) = ap

instance ElementAcceptContext AcceptContext where
   getContext = asElement <$> SM.get
   setContext c = SM.get >>= \ s -> SM.put s { asElement = c }
   withContext c a = SM.get >>= \ s -> case runAcceptContext a s { asElement = c } of
       (Left s, _) -> throwError s
       (Right v, s') -> SM.get >>= 
           \ s -> SM.put s { asKeys = asKeys s', asKeyIndex = asKeyIndex s' } >>
           return v
   
runAcceptContext = (SM.runState . runErrorT . unAcceptContext)
evalAcceptContext = (SM.evalState . runErrorT . unAcceptContext)
   