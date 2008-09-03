{-# OPTIONS_GHC -XFlexibleContexts #-}
module Lsl.WorldDef(Avatar(..),
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
                    worldFromFullWorldDef,
                    worldElement,
                    defaultRegions,
                    mkScript
                   ) where

import Control.Monad
import Control.Monad.Error
import qualified Control.Monad.State as SM
import Data.Int
import Data.List
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Maybe
import Debug.Trace

-- import Lsl.Avatar
import Lsl.DOMProcessing hiding (find)
import Lsl.Evaluation
import Lsl.Exec(ScriptImage,initLSLScript)
import Lsl.Key(mkKey,nullKey)
import Lsl.Structure(Validity(..),State(..))
import Lsl.Type
import Lsl.Util(readM,Permutation3(..),rotationsToQuaternion)

type KeyManagerM = ErrorT String (SM.State (M.Map String String,Integer))

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

data WebHandling = WebHandlingByFunction 
                 | WebHandlingByDoingNothing
                 | WebHandlingByInternet { webHandlingTimeout :: Float } deriving (Show)
                             
data LSLObject = LSLObject { primKeys :: [String], objectDynamics :: !ObjectDynamics } deriving (Show)

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
                      
defaultDynamics = ObjectDynamics { objectPosition = (0,0,0),
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
data PositionTarget = Repel { positionTargetTau :: Float, 
                              positionTargetOverWater :: Bool, 
                              positionTargetHeight :: Float }
                    | Hover { positionTargetTau :: Float, 
                              positionTargetOverWater :: Bool, 
                              positionTargetHeight :: Float }
                    | PositionTarget { positionTargetTau :: Float, 
                                       positionTargetLocation :: (Float,Float,Float), 
                                       positionTargetSetBy :: ScriptId }
    deriving (Show)
    
data RotationTarget = RotationTarget { rotationTarget :: (Float,Float,Float,Float),
                                       rotationTargetStrength :: Float,
                                       rotationTargetTau :: Float } deriving (Show)
                                       
data Avatar = Avatar { avatarKey :: String,
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
                       avatarAttachments :: IM.IntMap String } deriving (Show)

data CameraParams = CameraParams { cameraActive :: Bool,
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
defaultCamera = CameraParams { cameraActive = False,
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
                               
defaultAvatar key = 
    Avatar { avatarKey = key,
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
             avatarAttachments = IM.empty }
                       
-- these are bit INDEXES not MASKS (0 == least significant bit)
primPhantomBit :: Int
primPhantomBit = 4
primPhysicsBit :: Int
primPhysicsBit = 0

data InventoryItemData = InvScript { invScriptLibId :: String, invScriptState :: Maybe ScriptImage }
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
                       
newtype InventoryItemIdentification = InventoryItemIdentification { inventoryItemNameKey :: (String,String) } deriving (Show)
-- data InventoryItem a = InventoryItem { inventoryItemIdentification :: InventoryItemIdentification, 
--                                        inventoryItemInfo :: InventoryInfo,
--                                        inventoryItemData :: a } deriving (Show)
data InventoryInfo  = InventoryInfo {  inventoryInfoCreator :: String,
                                       --inventoryInfoOwner :: String,
                                       inventoryInfoPerms :: (Int,Int,Int,Int,Int) } deriving (Show)
data InventoryItem = InventoryItem { inventoryItemIdentification :: InventoryItemIdentification,
                                     inventoryItemInfo :: InventoryInfo,
                                     inventoryItemData :: InventoryItemData } deriving (Show)
                                     
--inventoryItemInfoMap = map ( \ item -> (inventoryItemIdentification item, inventoryItemInfo item))
inventoryItemName = fst . inventoryItemNameKey . inventoryItemIdentification
inventoryItemNames = map inventoryItemName
scriptInventoryItem s k id = InventoryItem (InventoryItemIdentification (s,k)) (InventoryInfo "" defaultInventoryPermissions) (InvScript id Nothing)
inventoryItemIdentifications = map inventoryItemIdentification
findByInvName name = find ((== name) . (fst . inventoryItemNameKey . inventoryItemIdentification))
findByInvKey key = find ((== key) . (snd . inventoryItemNameKey . inventoryItemIdentification))

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

data PrimType = PrimTypeUnknown
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

data Attachment = Attachment { attachmentKey :: String, attachmentPoint :: Int } deriving (Show)

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
                                      
emptyPrim name key =
    Prim { primName = name,
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
defaultRegions owner = [
    ((0,0), Region { regionName = "Region_0_0", regionFlags = 0,
                     regionParcels = [Parcel "parcel_0" "default parcel" (0,256,0,256) owner 0xffffffff [] []] })
    ]

data Script = Script { scriptImage :: !(Validity ScriptImage),
                       scriptActive :: Bool,
                       scriptPermissions :: M.Map String Int,
                       scriptLastPerm :: Maybe String,
                       scriptStartTick :: Int,
                       scriptLastResetTick :: Int,
                       scriptEventQueue :: [Event],
                       scriptStartParameter :: Int,
                       scriptCollisionFilter :: !(String,String,Bool),
                       scriptTargetIndex :: !Int,
                       scriptPositionTargets :: !(IM.IntMap ((Float,Float,Float), Float)),
                       scriptRotationTargets :: !(IM.IntMap ((Float,Float,Float,Float), Float)) } deriving (Show)

mkScript vimg = Script { scriptImage = vimg,
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
                         scriptRotationTargets = IM.empty }
                         
worldFromFullWorldDef worldBuilder fwd lib scripts =
    do let primMap = mkPrimMap (fullWorldDefPrims fwd)
       primMap' <- checkObjects primMap (fullWorldDefObjects fwd) -- prove all the prims in all the objects exists
       let activeScripts = 
               concat (map (\ prim -> map 
                   (\ item -> ((primKey prim, fst $ inventoryItemNameKey $ inventoryItemIdentification item),
                               invScriptLibId $ inventoryItemData item)) (filter isInvScriptItem $ primInventory prim)) (M.elems primMap'))
       activatedScripts <- activateScripts activeScripts scripts primMap'
       return $ worldBuilder (fullWorldDefSliceSize fwd)
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

fctx :: MonadError String m => String -> Either String a -> m a
fctx s (Left s') = fail s
fctx _ (Right v) = return v

maybe2Either Nothing = Left "failed"
maybe2Either (Just v) = Right v

mkPrimMap prims = M.fromList [(primKey p, p) | p <- prims]
    --let tuples = map (\ p -> (primKey p, p)) prims in M.fromList tuples
mkObjectMap objects = M.fromList [ (p, o) | o@(LSLObject { primKeys = (p:_) }) <- objects ]
mkAvatarLookup avatars = [ (avatarKey a,a) | a <- avatars]

checkObject primMap o = 
    foldM checkPrim primMap (primKeys o)
    where root = head (primKeys o)
          checkPrim m k = 
              case M.lookup k m of
                  Nothing -> fail ("prim " ++ k ++ " not found in definition")
                  Just prim -> 
                      return (if (k == root) then m
                                             else M.insert k (prim { primParent = Just root }) m)

checkObjects primMap os = foldM checkObject primMap os

activateScripts scriptIdInfo compiledScripts primMap = mapM (activateScript compiledScripts primMap) scriptIdInfo

activateScript scripts primMap (k@(primKey,invName),(scriptID)) =
    do  --script <- fctx ("looking up script " ++ scriptID ++ " failed") $ maybe2Either $ lookup scriptID scripts
        let script = case lookup scriptID scripts of
             Nothing -> fail "script not found"
             Just v -> v
        prim <- fctx ("looking up prim " ++ primKey ++ " failed") (M.lookup primKey primMap)
        when (isNothing (findByInvName invName (primInventory prim))) $ fail (invName ++ " doesn't exist in prim " ++ primKey)
        case script of
            Invalid s -> return (k, (mkScript (Invalid s)) { scriptEventQueue = []})
            Valid code -> return (k,mkScript (Valid $ initLSLScript code))

newKey xref = do
    (m,i) <- lift SM.get
    let k = mkKey i
    let m' = case xref of
                 Nothing -> m
                 Just v -> M.insert v k m
    lift $ SM.put (m',i+1)            
    return k
    
findRealKey k = lift SM.get >>= M.lookup k . fst

--worldElement :: ElemAcceptor KeyManagerM FullWorldDef
worldElement =
    let f (Elem _ _ contents) = do
        (maxTime, c1) <- findValue "maxTime" (elementsOnly contents)
        (handler, c2) <- findOptionalElement (simpleElement "simEventHandler") c1
        (sliceSizeStr, c3) <- findSimple "sliceSize" c2
        (avatars,c4) <- findElement avatarsElement c3
        (prims,c5) <- findElement primsElement c4
        (objects,[]) <- findElement objectsElement c5
        sliceSize <- readM sliceSizeStr
        (m,keyIndex) <- lift SM.get
        let webHandling = if isNothing handler then WebHandlingByDoingNothing else WebHandlingByFunction
        return $ (FullWorldDef maxTime sliceSize webHandling handler objects prims avatars (defaultRegions "") keyIndex)
    in ElemAcceptor "world-def" f
    
activeScriptsElement :: ElemAcceptor KeyManagerM [((String,String),String)]
activeScriptsElement = elementList "scripts" activeScriptElement

activeScriptElement :: ElemAcceptor KeyManagerM ((String,String),String)
activeScriptElement = ElemAcceptor "script" $
    \ (Elem _ _ contents) -> do
            (primKey,c1) <- findSimple "primKey" (elementsOnly contents)
            realPrimKey <- findRealKey primKey
            (scriptName, c2) <- findSimple "scriptName" c1
            (scriptId, []) <- findSimple "scriptId" c2
            return $ ((realPrimKey,scriptName), scriptId)
            
objectsElement :: ElemAcceptor KeyManagerM [LSLObject]
objectsElement = elementList "objects" objectElement

objectElement :: ElemAcceptor KeyManagerM LSLObject
objectElement = ElemAcceptor "object" $
    \ (Elem _ _ contents) -> do
           (position,c) <- findOrDefault (0,0,0) (vecAcceptor "position") (elementsOnly contents)
           (primKeys,[]) <- findElement (elementList "primKeys" (simpleElement "string")) c
           --(m,i) <- lift SM.get
           realPrimKeys <- mapM findRealKey primKeys
           return $ LSLObject realPrimKeys (defaultDynamics { objectPosition = position })
           
primsElement :: ElemAcceptor KeyManagerM [Prim]
primsElement = elementList "prims" primElement

primElement :: ElemAcceptor KeyManagerM Prim
primElement = ElemAcceptor "prim" $
    \ (Elem _ _ contents) -> do
            (name,c1) <- findSimple "name" (elementsOnly contents)
            (key,c2) <- findSimple "key" c1
            realKey <- newKey (Just key)
            (description, c3) <- findSimpleOrDefault "" "description" c2
            (scripts,c4) <- findElement (elementList "scripts" (scriptAcceptor "script")) c3
            (owner,c5) <- findSimpleOrDefault "" "owner" c4
            realOwner <- findRealKey owner
            (position,c6) <- findOrDefault (128.0,128.0,0.0) (vecAcceptor "position") c5
            (eulerRotation,c7) <- findOrDefault (0.0,0.0,0.0) (vecAcceptor "rotation") c6
            (scale,c8) <- findOrDefault (1.0,1.0,1.0) (vecAcceptor "scale") c7
            (faces,c9) <- findOrDefault (replicate 6 defaultFace) (elementList "faces" (faceAcceptor "face")) c8
            (flexibility,c10) <- findOptionalElement (flexibilityAcceptor "flexibility") c9
            (material,c11) <- findValueOrDefault 0 "material" c10
            (status,c12) <- findValueOrDefault 0x0e "status" c11
            (light,c13) <- findOptionalElement (lightAcceptor "light") c12
            (tempOnRez, c14) <- findValueOrDefault False "tempOnRez" c13
            (typeInfo, c15) <- findOrDefault basicBox (primTypeAcceptor "typeInfo") c14
            (permissions, c16) <- findOrDefault [0] (elementList "permissions" (valueAcceptor "int")) c15
            (dropAllowed,c17) <- findValueOrDefault False "dropAllowed" c16
            (inventory,[]) <- findOrDefault [] (elementListWith "inventory" acceptInventoryItem) c17
            return $ Prim { primName = name, 
                            primKey = realKey, 
                            primParent = Nothing,
                            primDescription = description,
                            primInventory = scripts ++ inventory,
                            primOwner = realOwner,
                            primGroup = Nothing,
                            primCreator = realOwner,
                            primPosition = position,
                            primRotation = rotationsToQuaternion P123 eulerRotation,
                            primScale = scale,
                            primFaces = faces,
                            primFlexibility = flexibility,
                            primMaterial = material,
                            primStatus = status,
                            primVehicleFlags = 0,
                            primLight = light,
                            primTempOnRez = tempOnRez,
                            primTypeInfo = typeInfo,
                            primPermissions = permissions,
                            primAllowInventoryDrop = dropAllowed,
                            primSitTarget = Nothing ,
                            primSittingAvatar = Nothing,
                            primPendingEmails = [],
                            primPassTouches = False,
                            primPassCollisions = False,
                            primPayInfo = (-2, -2, -2, -2, -2),
                            primAttachment = Nothing,
                            primRemoteScriptAccessPin = 0 }

acceptInventoryItem e = matchChoice (map (uncurry mkInventoryItemAcceptor) 
    [("notecardItem",acceptNotecardData),
     ("textureItem", acceptTextureData),
     ("bodyPartItem", acceptBodyPartData),
     ("clothingItem", acceptClothingData),
     ("gestureItem", acceptGestureData),
     ("soundItem", acceptSoundData),
     ("animationItem", acceptAnimationData),
     ("inventoryObjectItem", acceptInventoryObjectData),
     ("landmarkItem", acceptLandmarkData)
    ]) e

acceptNotecardData contents = do
     (lines,c1) <- findOrDefault [] (elementList "lines" (simpleElement "string")) contents
     return (InvNotecard lines,c1)

acceptTextureData contents = return (InvTexture,contents)
acceptBodyPartData contents = return (InvBodyPart,contents)
acceptClothingData contents = return (InvClothing,contents)
acceptGestureData contents = return (InvGesture,contents)
acceptSoundData contents = do
    (duration,c1) <- findValue "duration" contents
    return (InvSound duration,c1)
acceptAnimationData contents = do
    (duration,c1) <- findValue "duration" contents
    return (InvAnimation (if duration == 0 then Nothing else Just duration),c1)
acceptInventoryObjectData contents = do
    (prims,c1) <- findElement primsElement contents
    return (InvObject prims,c1)
    
acceptLandmarkData contents = do
    (region,c1) <- findElement (regionAcceptor "region") contents
    (position, c2) <- findElement (vecAcceptor "position") c1
    return (InvLandmark (region,position), c2)
    
mkInventoryItemAcceptor s f = ElemAcceptor s $
   \ (Elem _ _ contents) -> do
       let c0 = elementsOnly contents
       (name,c1) <- findSimple "name" c0
       key <- newKey Nothing
       (creator,c2) <- findSimple "creator" c1
       realCreator <- findRealKey creator
       (itemData,[]) <- f c2
       return $ InventoryItem (InventoryItemIdentification (name, key)) (InventoryInfo creator defaultInventoryPermissions)
                       itemData
       
scriptAcceptor s = ElemAcceptor s $
   \ (Elem _ _ contents) -> do
       let c0 = elementsOnly contents
       (scriptName, c1) <- findSimple "scriptName" c0
       (scriptId, []) <- findSimple "scriptId" c1
       k <- newKey Nothing
       return $ scriptInventoryItem scriptName k scriptId
       
faceAcceptor s = ElemAcceptor s $
    \ (Elem _ _ contents) -> do
            let c0 = elementsOnly contents
            (alpha,c1) <- findValueOrDefault 0.0 "alpha" c0
            (color,c2) <- findOrDefault (1.0,1.0,1.0) (vecAcceptor "color") c1
            (shininess, c3) <- findValueOrDefault 0 "shininess" c2
            (bumpiness, c4) <- findValueOrDefault 0 "bumpiness" c3
            (fullbright, c5) <- findValueOrDefault False "fullbright" c4
            (textureMode, c6) <- findValueOrDefault 0 "textureMode" c5
            (textureInfo, []) <- findOrDefault defaultTextureInfo (textureInfoAcceptor "textureInfo") c6
            return $ PrimFace {
                    faceAlpha = alpha,
                    faceColor = color,
                    faceShininess = shininess,
                    faceBumpiness = bumpiness,
                    faceFullbright = fullbright,
                    faceTextureMode = textureMode,
                    faceTextureInfo = textureInfo
                }    
                
textureInfoAcceptor s = ElemAcceptor s $
    \ (Elem _ _ contents) -> do
        let c0 = elementsOnly contents
        (name,c1) <- findSimpleOrDefault "" "name" c0
        (repeats,c2) <- findOrDefault (1.0,1.0,1.0) (vecAcceptor "repeats") c1
        (offsets,c3) <- findOrDefault (0.0,0.0,0.0) (vecAcceptor "offsets") c2
        (rotation,[]) <- findValueOrDefault 0.0 "rotation" c3
        return $ TextureInfo {
                textureKey = name,
                textureRepeats = repeats,
                textureOffsets = offsets,
                textureRotation = rotation
            }
            
flexibilityAcceptor s = ElemAcceptor s $
    \ (Elem _ _ contents) -> do
        let c0 = elementsOnly contents
        (softness,c1) <- findValueOrDefault 0 "softness" c0
        (gravity,c2) <- findValueOrDefault 1.0 "gravity" c1
        (friction,c3) <- findValueOrDefault 0.0 "friction" c2
        (wind,c4) <- findValueOrDefault 0.0 "wind" c3
        (tension,c5) <- findValueOrDefault 1.0 "tension" c4
        (force, []) <- findOrDefault (0.0,0.0,0.0) (vecAcceptor "force") c5
        return $ Flexibility {
                flexSoftness =  softness,
                flexGravity = gravity,
                flexFriction = friction,
                flexWind = wind,
                flexTension = tension,
                flexForce = force
            }

lightAcceptor s = ElemAcceptor s $
    \ (Elem _ _ contents) -> do
        let c0 = elementsOnly contents
        (color,c1) <- findOrDefault (1.0,1.0,1.0) (vecAcceptor "color") c0
        (intensity,c2) <- findValueOrDefault 1.0 "intensity" c1
        (radius,c3) <- findValueOrDefault 10.0 "radius" c2
        (falloff,[]) <- findValueOrDefault 1.0 "falloff" c3
        return $ LightInfo {
                lightColor = color,
                lightIntensity = intensity,
                lightRadius = radius,
                lightFalloff = falloff
            }

primTypeAcceptor s = ElemAcceptor s $
    \ (Elem _ _ contents) -> do
        let c0 = elementsOnly contents
        (version, c1) <- findValueOrDefault 9 "version" c0
        (typeCode, c2) <- findValueOrDefault 0 "typeCode" c1
        (holeshape, c3) <- findValueOrDefault 0 "holeshape" c2
        (cut, c4) <- findOrDefault (0.0,0.0,0.0) (vecAcceptor "cut") c3
        (twist, c5) <- findOrDefault (0.0,0.0,0.0) (vecAcceptor "twist") c4
        (holesize, c6) <- findOrDefault (0.0,0.0,0.0) (vecAcceptor "holesize") c5
        (topshear, c7) <- findOrDefault (0.0,0.0,0.0) (vecAcceptor "topshear") c6
        (hollow, c8) <- findValueOrDefault 0.0 "hollow" c7
        (taper, c9) <- findOrDefault (0.0,0.0,0.0) (vecAcceptor "taper") c8
        (advancedCut, c10) <- findOrDefault (0.0,0.0,0.0) (vecAcceptor "advancedCut") c9
        (radiusOffset, c11) <- findValueOrDefault 0.0 "radiusOffset" c10
        (revolutions, c12) <- findValueOrDefault 0.0 "revolutions" c11
        (skew, c13) <- findValueOrDefault 0.0 "skew" c12
        (sculptTexture, c14) <- findOptionalElement (simpleElement "sculptTexture") c13
        (sculptType, []) <- findValueOrDefault 0 "sculptType" c14
        return $ PrimType {
                   primVersion = version, -- 1 or 9
                   primTypeCode = typeCode,
                   primHoleshape = holeshape,
                   primCut = cut,
                   primTwist = twist,
                   primHolesize = holesize,
                   primTopshear = topshear,
                   primHollow = hollow,
                   primTaper =  taper,
                   primAdvancedCut = advancedCut,
                   primRadiusOffset = radiusOffset,
                   primRevolutions = revolutions,
                   primSkew = skew,
                   primSculptTexture = sculptTexture,
                   primSculptType = sculptType
                }
                
avatarsElement :: ElemAcceptor KeyManagerM [Avatar]
avatarsElement = elementList "avatars" avatarElement

avatarElement :: ElemAcceptor KeyManagerM Avatar
avatarElement = ElemAcceptor "avatar" $
    \ (Elem _ _ contents) -> do
            (name,c2) <- findSimple "name" (elementsOnly contents)
            realKey <- newKey (Just name)
            (x,c3) <- findValue "xPos" c2
            (y,c4) <- findValue "yPos" c3
            (z,_) <- findValue "zPos" c4
            return $ (defaultAvatar realKey) { avatarName = name, avatarPosition = (x,y,z), avatarCameraPosition = (x,y,z) }

vecAcceptor s = ElemAcceptor s $
    \ (Elem _ _ contents) -> do
        (x,c1) <- findValue "x" (elementsOnly contents)
        (y,c2) <- findValue "y" c1
        (z,[]) <- findValue "z" c2
        return $ (x,y,z)
        
rotAcceptor s = ElemAcceptor s $
    \ (Elem _ _ contents) -> do
        (x,c1) <- findValue "x" (elementsOnly contents)
        (y,c2) <- findValue "y" c1
        (z,c3) <- findValue "z" c2
        (s,[]) <- findValue "s" c3
        return $ (x,y,z,s)
        
regionAcceptor s = ElemAcceptor s $
    \ (Elem _ _ contents) -> do
        (x,c1) <- findValue "x" (elementsOnly contents)
        (y,[]) <- findValue "y" (c1)
        return $ (x,y)