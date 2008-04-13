module Lsl.WorldDef(Prim(..),
                    LSLObject(..),
                    FullWorldDef(..),
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

import Text.XML.HaXml hiding (when)

data FullWorldDef = FullWorldDef {
                        fullWorldDefMaxTime :: Int,
                        fullWorldDefSliceSize :: Int,
                        fullWorldDefActiveScripts :: [((String,String),(String))], -- [((primKey,invName),(scriptID))]
                        fullWorldDefObjects :: [LSLObject],
                        fullWorldDefPrims :: [Prim],
                        fullWorldDefAvatars :: [Avatar] }
                        
data LSLObject = LSLObject { primKeys :: [String] } deriving (Show)

data Prim = Prim {
                    primName :: String,
                    primKey :: String,
                    primParent :: Maybe String,
                    primScripts :: [String],
                    notecards :: [(String,[String])],
                    animations :: [String],
                    textures :: [String],
                    sounds :: [String],
                    inventoryObjects :: [LSLObject] } deriving (Show)
                    
worldFromFullWorldDef worldBuilder fwd lib scripts =
    do let primMap = mkPrimMap (fullWorldDefPrims fwd)
       checkObjects primMap (fullWorldDefObjects fwd) -- prove all the prims in all the objects exists
       activatedScripts <- activateScripts (fullWorldDefActiveScripts fwd) scripts primMap
       return $ worldBuilder (fullWorldDefSliceSize fwd)
                             (fullWorldDefMaxTime fwd)
                             [] lib scripts 
                             (mkAvatarLookup (fullWorldDefAvatars fwd))
                             (mkObjectMap (fullWorldDefObjects fwd))
                             primMap
                             (M.fromList activatedScripts)

fctx :: Monad m => String -> Either String a -> m a
fctx s (Left s') = fail s
fctx _ (Right v) = return v

maybe2Either Nothing = Left "failed"
maybe2Either (Just v) = Right v

mkPrimMap prims = M.fromList [(primKey p, p) | p <- prims]
    --let tuples = map (\ p -> (primKey p, p)) prims in M.fromList tuples
mkObjectMap objects = M.fromList [ (p,LSLObject (p:ps)) | LSLObject (p:ps) <- objects ]
mkAvatarLookup avatars = [ (avatarKey a,a) | a <- avatars]
checkObject primMap o = mapM_ (flip M.lookup primMap) (primKeys o)
checkObjects primMap os = mapM_ (checkObject primMap) os

activateScripts scriptIdInfo compiledScripts primMap = mapM (activateScript compiledScripts primMap) scriptIdInfo

activateScript scripts primMap (k@(primKey,invName),(scriptID)) =
    do  script <- fctx ("looking up script " ++ scriptID ++ " failed") $ maybe2Either $ lookup scriptID scripts
        prim <- fctx ("looking up prim " ++ primKey ++ " failed") (M.lookup primKey primMap)
        when (not (invName `elem` primScripts prim)) $ fail (invName ++ " doesn't exist in prim " ++ primKey)
        case script of
            Invalid s -> return (k,(Invalid s,[]))
            Valid code -> return (k,(Valid $ initLSLScript code,[Event "state_entry" [] []]))
            
readM s = case reads s of
             [] -> fail ("unable to parse " ++ s)
             ((v,_):_) -> return v
             
worldElement :: Monad m => ElemAcceptor m FullWorldDef
worldElement =
    let f (Elem _ _ contents) = do
        (maxTimeStr, c1) <- findSimple "maxTime" (elementsOnly contents)
        (sliceSizeStr, c2) <- findSimple "sliceSize" c1
        (activeScripts, c3) <- findElement activeScriptsElement c2
        (objects,c4) <- findElement objectsElement c3
        (prims,c5) <- findElement primsElement c4
        (avatars,[]) <- findElement avatarsElement c5
        maxTime <- readM maxTimeStr
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
            (scripts,[]) <- findElement (elementList "scripts" (simpleElement "string")) c2
            return $ Prim { primName = name, primKey = key, primParent = Nothing, primScripts = scripts,
                            notecards = [], animations = [], textures = [], sounds = [],
                            inventoryObjects = [] }

avatarsElement :: Monad m => ElemAcceptor m [Avatar]
avatarsElement = elementList "avatars" avatarElement

avatarElement :: Monad m => ElemAcceptor m Avatar
avatarElement = ElemAcceptor "avatar" $
    \ (Elem _ _ contents) -> do
            (key,c1) <- findSimple "key" (elementsOnly contents)
            (name,[]) <- findSimple "name" c1
            return $ (defaultAvatar key) { avatarName = name }