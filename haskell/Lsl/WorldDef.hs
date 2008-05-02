module Lsl.WorldDef(Prim(..),
                    PrimFace(..),
                    TextureInfo(..),
                    Flexibility(..),
                    LightInfo(..),
                    LSLObject(..),
                    PrimType(..),
                    FullWorldDef(..),
                    Region(..),
                    Parcel(..),
                    InventoryItemIdentification(..),
                    InventoryItem(..),
                    InventoryInfo(..),
                    Script(..),
                    Attachment(..),
                    inventoryItemInfoMap,
                    inventoryItemNames,
                    scriptInventoryItem,
                    findByInvName,
                    emptyPrim,
                    primPhantomBit,
                    primPhysicsBit,
                    worldFromFullWorldDef,
                    worldElement,
                    defaultRegions
                   ) where

import Control.Monad
import Control.Monad.Error
import qualified Control.Monad.State as SM
import Data.List
import qualified Data.Map as M
import Data.Maybe

import Lsl.Avatar
import Lsl.DOMProcessing hiding (find)
import Lsl.Evaluation
import Lsl.Exec(ScriptImage,initLSLScript)
import Lsl.Key(mkKey)
import Lsl.Structure(Validity(..),State(..))
import Lsl.Type
import Lsl.ValueDB
import Lsl.Util(readM,Permutation3(..),rotationsToQuaternion)

type KeyManagerM = ErrorT String (SM.State (M.Map String String,Integer))

data FullWorldDef = FullWorldDef {
                        fullWorldDefMaxTime :: Int,
                        fullWorldDefSliceSize :: Int,
                        fullWorldDefActiveScripts :: [((String,String),(String))], -- [((primKey,invName),(scriptID))]
                        fullWorldDefObjects :: [LSLObject],
                        fullWorldDefPrims :: [Prim],
                        fullWorldDefAvatars :: [Avatar],
                        fullWorldDefRegions :: [((Int,Int),Region)],
                        fullWorldDefInitialKeyIndex :: Integer } deriving (Show)
                        
data LSLObject = LSLObject { primKeys :: [String] } deriving (Show)

-- these are bit INDEXES not MASKS (0 == least significant bit)
primPhantomBit :: Int
primPhantomBit = 4
primPhysicsBit :: Int
primPhysicsBit = 0

newtype InventoryItemIdentification = InventoryItemIdentification { inventoryItemNameKey :: (String,String) } deriving (Show)
data InventoryItem a = InventoryItem { inventoryItemIdentification :: InventoryItemIdentification, 
                                       inventoryItemInfo :: InventoryInfo,
                                       inventoryItemData :: a } deriving (Show)
data InventoryInfo  = InventoryInfo {  inventoryInfoCreator :: String,
                                       inventoryInfoOwner :: String,
                                       inventoryInfoCopyPerm :: Bool,
                                       inventoryInfoModifyPerm :: Bool,
                                       inventoryInfoTransferPerm :: Bool } deriving (Show)

inventoryItemInfoMap = map ( \ item -> (inventoryItemIdentification item, inventoryItemInfo item))
inventoryItemNames = map (fst . inventoryItemNameKey . inventoryItemIdentification)
scriptInventoryItem s k = InventoryItem (InventoryItemIdentification (s,k)) (InventoryInfo "" "" True True True) ()
inventoryItemIdentifications = map inventoryItemIdentification
findByInvName name = find (\ (InventoryItemIdentification (n,_),_) -> n == name)

data Prim = Prim {
                    primName :: String,
                    primKey :: String,
                    primParent :: Maybe String,
                    primDescription :: String,
                    primScripts :: [InventoryItem ()],
                    notecards :: [InventoryItem [String]],
                    animations :: [InventoryItem ()],
                    textures :: [InventoryItem ()],
                    sounds :: [InventoryItem ()],
                    primBodyParts :: [InventoryItem ()],
                    primClothing :: [InventoryItem ()],
                    primGestures :: [InventoryItem ()],
                    primLandmarks :: [InventoryItem ((Int,Int),(Float,Float,Float))],
                    inventoryObjects :: [InventoryItem LSLObject],
                    primOwner :: String,
                    primCreator :: String,
                    primPosition :: (Float, Float, Float),
                    primRotation :: (Float, Float, Float, Float),
                    primScale :: (Float, Float, Float),
                    primFaces :: [PrimFace],
                    primFlexibility :: Maybe Flexibility,
                    primMaterial :: Int,
                    primStatus :: Int,
                    primLight :: Maybe LightInfo,
                    primTempOnRez :: Bool,
                    primTypeInfo :: PrimType,
                    primPermissions :: [Int],
                    primData :: ValueDB,
                    primAllowInventoryDrop :: Bool,
                    primSitTarget :: Maybe ((Float,Float,Float),(Float,Float,Float,Float)),
                    primSittingAvatar :: Maybe String,
                    primAttachment :: Maybe Attachment } deriving (Show)

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
                   
basicBox = PrimType 9 0 0 (0,0.0,0) (0,0,0) (0,0,0) (0,0,0) 0 (0,0,0) (0,0,0) 0 0 0 Nothing 0

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
        textureName :: String,
        textureRepeats :: (Float,Float,Float),
        textureOffsets :: (Float,Float,Float),
        textureRotation :: Float
    } deriving (Show)

defaultTextureInfo = TextureInfo "" (1.0,1.0,0.0) (0.0,0.0,0.0) 0.0
                   
                                      
emptyPrim name key =
    Prim { primName = name,
           primKey = key,
           primParent = Nothing,
           primDescription = "",
           primScripts = [],
           notecards = [],
           animations = [],
           textures = [],
           sounds = [],
           primBodyParts = [],
           primClothing = [],
           primGestures = [],
           primLandmarks = [],
           inventoryObjects = [],
           primOwner = "",
           primCreator = "",
           primPosition = (0.0,0.0,0.0),
           primRotation = (0.0,0.0,0.0,1.0),
           primScale = (1.0,1.0,1.0),
           primFaces = replicate 6 defaultFace,
           primFlexibility = Nothing,
           primMaterial = 0,
           primStatus = 0x0e,
           primLight = Nothing,
           primTempOnRez = False,
           primTypeInfo = basicBox,
           primPermissions = [0],            
           primData = emptyDB,
           primAllowInventoryDrop = False,
           primSitTarget = Nothing,
           primSittingAvatar = Nothing,
           primAttachment = Nothing }

data Region = Region {
        regionName :: String,
        regionParcels :: [Parcel]
    } deriving (Show)
    
data Parcel = Parcel {
        parcelName :: String,
        parcelBoundaries :: (Int,Int,Int,Int), -- bottom, top, left, right aka south,north,west,east
        parcelOwner :: String,
        parcelBanList :: [(String,Maybe Int)],
        parcelPassList :: [(String,Maybe Int)]
    } deriving (Show)

defaultRegions :: String -> [((Int,Int),Region)]
defaultRegions owner = [
    ((0,0), Region { regionName = "Region_0_0", regionParcels = [Parcel "parcel_0" (0,256,0,256) owner [] []] })
    ]

data Script = Script { scriptImage :: Validity ScriptImage,
                       scriptPermissions :: M.Map String Int,
                       scriptLastPerm :: Maybe String,
                       scriptStartTick :: Int,
                       scriptLastResetTick :: Int,
                       scriptEventQueue :: [Event] } deriving (Show)
                       
worldFromFullWorldDef worldBuilder fwd lib scripts =
    do let primMap = mkPrimMap (fullWorldDefPrims fwd)
       primMap' <- checkObjects primMap (fullWorldDefObjects fwd) -- prove all the prims in all the objects exists
       activatedScripts <- activateScripts (fullWorldDefActiveScripts fwd) scripts primMap'
       return $ worldBuilder (fullWorldDefSliceSize fwd)
                             (fullWorldDefMaxTime fwd)
                             [] lib scripts 
                             (M.fromList $ mkAvatarLookup (fullWorldDefAvatars fwd))
                             (mkObjectMap (fullWorldDefObjects fwd))
                             primMap'
                             (M.fromList activatedScripts)
                             emptyDB
                             (M.fromList (fullWorldDefRegions fwd))
                             (fullWorldDefInitialKeyIndex fwd)

fctx :: MonadError String m => String -> Either String a -> m a
fctx s (Left s') = fail s
fctx _ (Right v) = return v

maybe2Either Nothing = Left "failed"
maybe2Either (Just v) = Right v

mkPrimMap prims = M.fromList [(primKey p, p) | p <- prims]
    --let tuples = map (\ p -> (primKey p, p)) prims in M.fromList tuples
mkObjectMap objects = M.fromList [ (p,LSLObject (p:ps)) | LSLObject (p:ps) <- objects ]
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
    do  script <- fctx ("looking up script " ++ scriptID ++ " failed") $ maybe2Either $ lookup scriptID scripts
        prim <- fctx ("looking up prim " ++ primKey ++ " failed") (M.lookup primKey primMap)
        when (isNothing (findByInvName invName $ inventoryItemInfoMap (primScripts prim))) $ fail (invName ++ " doesn't exist in prim " ++ primKey)
        case script of
            Invalid s -> return (k, Script (Invalid s) M.empty Nothing 0 0  [])
            Valid code -> return (k,Script (Valid $ initLSLScript code) M.empty Nothing 0 0 [Event "state_entry" [] []])

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
        (sliceSizeStr, c2) <- findSimple "sliceSize" c1
        (avatars,c3) <- findElement avatarsElement c2
        (prims,c4) <- findElement primsElement c3
        (activeScripts, c5) <- findElement activeScriptsElement c4
        (objects,[]) <- findElement objectsElement c5
        --maxTime <- readM maxTimeStr
        sliceSize <- readM sliceSizeStr
        (m,keyIndex) <- lift SM.get
        return $ (FullWorldDef maxTime sliceSize activeScripts objects prims avatars (defaultRegions "") keyIndex)
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
           (primKeys,[]) <- findElement (elementList "primKeys" (simpleElement "string")) (elementsOnly contents)
           (m,i) <- lift SM.get
           realPrimKeys <- mapM findRealKey primKeys
           return $ LSLObject realPrimKeys
           
primsElement :: ElemAcceptor KeyManagerM [Prim]
primsElement = elementList "prims" primElement

primElement :: ElemAcceptor KeyManagerM Prim
primElement = ElemAcceptor "prim" $
    \ (Elem _ _ contents) -> do
            (name,c1) <- findSimple "name" (elementsOnly contents)
            (key,c2) <- findSimple "key" c1
            realKey <- newKey (Just key)
            (description, c3) <- findSimpleOrDefault "" "description" c2
            (scripts,c4) <- findElement (elementList "scripts" (simpleElement "string")) c3
            scriptKeys <- replicateM (length scripts) (newKey Nothing)
            let scripts' = zipWith scriptInventoryItem scripts scriptKeys
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
            (pData,c17) <- findElement (dbAcceptor "data") c16
            (dropAllowed,[]) <- findValueOrDefault False "dropAllowed" c17
            return $ Prim { primName = name, 
                            primKey = realKey, 
                            primParent = Nothing,
                            primDescription = description,
                            primScripts = scripts',
                            notecards = [], 
                            animations = [],
                            textures = [],
                            sounds = [],
                            primBodyParts = [],
                            primClothing = [],
                            primGestures = [],
                            primLandmarks = [],
                            inventoryObjects = [],
                            primOwner = realOwner,
                            primCreator = realOwner,
                            primPosition = position,
                            primRotation = rotationsToQuaternion P123 eulerRotation,
                            primScale = scale,
                            primFaces = faces,
                            primFlexibility = flexibility,
                            primMaterial = material,
                            primStatus = status,
                            primLight = light,
                            primTempOnRez = tempOnRez,
                            primTypeInfo = typeInfo,
                            primPermissions = permissions,
                            primData = pData,
                            primAllowInventoryDrop = dropAllowed,
                            primSitTarget = Nothing ,
                            primSittingAvatar = Nothing,
                            primAttachment = Nothing }

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
                textureName = name,
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
            (key,c1) <- findSimple "key" (elementsOnly contents)
            realKey <- newKey (Just key)
            (name,c2) <- findSimple "name" c1
            (x,c3) <- findValue "xPos" c2
            (y,c4) <- findValue "yPos" c3
            (z,_) <- findValue "zPos" c4
            return $ (defaultAvatar realKey) { avatarName = name, avatarPosition = (x,y,z) }

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
        
