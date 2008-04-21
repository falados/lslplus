module Lsl.WorldDef(Prim(..),
                    PrimFace(..),
                    LSLObject(..),
                    FullWorldDef(..),
                    emptyPrim,
                    worldFromFullWorldDef,
                    worldElement
                   ) where

import Control.Monad
import Control.Monad.Error
import Data.List
import qualified Data.Map as M

import Lsl.Avatar
import Lsl.DOMProcessing
import Lsl.Evaluation
import Lsl.Exec
import Lsl.Structure
import Lsl.Type
import Lsl.ValueDB
import Lsl.Util

import Text.XML.HaXml hiding (when)

data FullWorldDef = FullWorldDef {
                        fullWorldDefMaxTime :: Int,
                        fullWorldDefSliceSize :: Int,
                        fullWorldDefActiveScripts :: [((String,String),(String))], -- [((primKey,invName),(scriptID))]
                        fullWorldDefObjects :: [LSLObject],
                        fullWorldDefPrims :: [Prim],
                        fullWorldDefAvatars :: [Avatar] } deriving (Show)
                        
data LSLObject = LSLObject { primKeys :: [String] } deriving (Show)

data Prim = Prim {
                    primName :: String,
                    primKey :: String,
                    primParent :: Maybe String,
                    primDescription :: String,
                    primScripts :: [String],
                    notecards :: [(String,[String])],
                    animations :: [String],
                    textures :: [String],
                    sounds :: [String],
                    inventoryObjects :: [LSLObject],
                    primOwner :: String,
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
                    primData :: ValueDB } deriving (Show)

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
                   primRadiusOffset :: Float,
                   primRevolutions :: Float,
                   primSkew :: Float,
                   primSculptTexture :: Maybe String 
                } deriving (Show)
                   
basicBox = PrimType 9 0 0 (0,0,0) (0,0,0) (0,0,0) (0,0,0) 0 (0,0,0) 0 0 0 Nothing

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
           inventoryObjects = [],
           primOwner = "",
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
           primData = emptyDB }
                    
worldFromFullWorldDef worldBuilder fwd lib scripts =
    do let primMap = mkPrimMap (fullWorldDefPrims fwd)
       primMap' <- checkObjects primMap (fullWorldDefObjects fwd) -- prove all the prims in all the objects exists
       activatedScripts <- activateScripts (fullWorldDefActiveScripts fwd) scripts primMap'
       return $ worldBuilder (fullWorldDefSliceSize fwd)
                             (fullWorldDefMaxTime fwd)
                             [] lib scripts 
                             (mkAvatarLookup (fullWorldDefAvatars fwd))
                             (mkObjectMap (fullWorldDefObjects fwd))
                             primMap'
                             (M.fromList activatedScripts)
                             emptyDB

fctx :: Monad m => String -> Either String a -> m a
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
        when (not (invName `elem` primScripts prim)) $ fail (invName ++ " doesn't exist in prim " ++ primKey)
        case script of
            Invalid s -> return (k,(Invalid s,[]))
            Valid code -> return (k,(Valid $ initLSLScript code,[Event "state_entry" [] []]))
            
worldElement :: Monad m => ElemAcceptor m FullWorldDef
worldElement =
    let f (Elem _ _ contents) = do
        (maxTime, c1) <- findValue "maxTime" (elementsOnly contents)
        (sliceSizeStr, c2) <- findSimple "sliceSize" c1
        (activeScripts, c3) <- findElement activeScriptsElement c2
        (objects,c4) <- findElement objectsElement c3
        (prims,c5) <- findElement primsElement c4
        (avatars,[]) <- findElement avatarsElement c5
        --maxTime <- readM maxTimeStr
        sliceSize <- readM sliceSizeStr
        return $ (FullWorldDef maxTime sliceSize activeScripts objects prims avatars)
    in ElemAcceptor "world-def" f
    
activeScriptsElement :: Monad m => ElemAcceptor m [((String,String),String)]
activeScriptsElement = elementList "scripts" activeScriptElement

activeScriptElement :: Monad m => ElemAcceptor m ((String,String),String)
activeScriptElement = ElemAcceptor "script" $
    \ (Elem _ _ contents) -> do
            (primKey,c1) <- findSimple "primKey" (elementsOnly contents)
            (scriptName, c2) <- findSimple "scriptName" c1
            (scriptId, []) <- findSimple "scriptId" c2
            return $ ((primKey,scriptName), scriptId)
            
objectsElement :: Monad m => ElemAcceptor m [LSLObject]
objectsElement = elementList "objects" objectElement

objectElement :: Monad m => ElemAcceptor m LSLObject
objectElement = ElemAcceptor "object" $
    \ (Elem _ _ contents) -> do
           (primKeys,[]) <- findElement (elementList "primKeys" (simpleElement "string")) (elementsOnly contents)
           return $ LSLObject primKeys
           
primsElement :: Monad m => ElemAcceptor m [Prim]
primsElement = elementList "prims" primElement

primElement :: Monad m => ElemAcceptor m Prim
primElement = ElemAcceptor "prim" $
    \ (Elem _ _ contents) -> do
            (name,c1) <- findSimple "name" (elementsOnly contents)
            (key,c2) <- findSimple "key" c1
            (description, c3) <- findSimpleOrDefault "" "description" c2
            (scripts,c4) <- findElement (elementList "scripts" (simpleElement "string")) c3
            (owner,c5) <- findSimpleOrDefault "" "owner" c4
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
            (pData,[]) <- findElement (dbAcceptor "data") c16
            return $ Prim { primName = name, 
                            primKey = key, 
                            primParent = Nothing,
                            primDescription = description,
                            primScripts = scripts,
                            notecards = [], 
                            animations = [],
                            textures = [],
                            sounds = [],
                            inventoryObjects = [],
                            primOwner = owner,
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
                            primData = pData }

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
        (radiusOffset, c10) <- findValueOrDefault 0.0 "radiusOffset" c9
        (revolutions, c11) <- findValueOrDefault 0.0 "revolutions" c10
        (skew, c12) <- findValueOrDefault 0.0 "skew" c11
        (sculptTexture, []) <- findOptionalElement (simpleElement "sculptTexture") c12
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
                   primRadiusOffset = radiusOffset,
                   primRevolutions = revolutions,
                   primSkew = skew,
                   primSculptTexture = sculptTexture 
                }
                
avatarsElement :: Monad m => ElemAcceptor m [Avatar]
avatarsElement = elementList "avatars" avatarElement

avatarElement :: Monad m => ElemAcceptor m Avatar
avatarElement = ElemAcceptor "avatar" $
    \ (Elem _ _ contents) -> do
            (key,c1) <- findSimple "key" (elementsOnly contents)
            (name,c2) <- findSimple "name" c1
            (x,c3) <- findValue "xPos" c2
            (y,c4) <- findValue "yPos" c3
            (z,_) <- findValue "zPos" c4
            return $ (defaultAvatar key) { avatarName = name, avatarPosition = (x,y,z) }

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
        
