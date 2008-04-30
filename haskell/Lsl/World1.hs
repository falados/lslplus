module Lsl.World1(module Lsl.World1,module Lsl.WorldState) where

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
import qualified Data.IntMap as IM
import Debug.Trace

import Lsl.Avatar
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
import Lsl.WorldState

import System.Random
import System.Time
import Test.QuickCheck
import Text.Printf

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
               logAMessage LogDebug (pkey ++ ":" ++ sid) ("unimplemented predefined function called: " ++ renderCall name args)
               return (EvalIncomplete,case rettype of
                      LLVoid -> VoidVal
                      LLInteger -> IVal 0
                      LLFloat -> FVal 0.0
                      LLString -> SVal ""
                      LLKey -> KVal nullKey
                      LLList -> LVal []
                      LLVector -> VVal 0.0 0.0 0.0
                      LLRot -> RVal 0.0 0.0 0.0 1.0)

    
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

getParcelByPosition regionIndex (x,y,_) = 
    do
        region <- getRegion regionIndex
        findParcel 0 (regionParcels region)
    where findParcel _ [] = mzero
          findParcel i (p:ps) =
              let (south,north,west,east) = parcelBoundaries p
                  (xc,yc) = (floor x, floor y) in
                  if xc < east && xc >= west && yc < north && yc >= south
                      then return (i,p) else findParcel (i + 1) ps

getPrimParcel pk = do
    regionIndex <- lift $ getPrimRegion pk
    pos <- getPrimPosition pk
    (index,parcel) <- getParcelByPosition regionIndex pos
    return (regionIndex,index,parcel)
    
putParcel regionIndex index parcel = do
    region <- getRegion regionIndex
    let (before,after) = splitAt index (regionParcels region)
    let parcels' = if null after then parcel : before else before ++ (parcel : tail after)
    lift $ setRegion regionIndex $ region { regionParcels = parcels' }
                
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

--- INVENTORY related functions ----------------------------------------------
sortByInvName = sortBy (\ (InventoryItemIdentification (n,_),_) (InventoryItemIdentification (n',_),_) -> compare n n')

getAllInventoryInfo k = 
    sequence (map ($k) invGetters) >>= return . sortByInvName . concat
    where invGetters = [getPrimAnimationInfo,getPrimBodyPartInfo,getPrimClothingInfo,getPrimGestureInfo,
                        getPrimNotecardInfo,getPrimObjectInfo,getPrimSoundInfo,getPrimTextureInfo,getPrimScriptInfo]

getAnInventory f k = f k >>= return . sortByInvName

getPrimInventoryInfo info@(ScriptInfo _ _ _ pk _) invType =
    case invType of
        i | i == llcInventoryAll -> getAllInventoryInfo pk
          | i == llcInventoryAnimation -> getAnInventory getPrimAnimationInfo pk
          | i == llcInventoryBodyPart -> getAnInventory getPrimBodyPartInfo pk
          | i == llcInventoryClothing -> getAnInventory getPrimClothingInfo pk
          | i == llcInventoryGesture -> getAnInventory getPrimGestureInfo pk
          | i == llcInventoryNotecard -> getAnInventory getPrimNotecardInfo pk
          | i == llcInventoryObject -> getAnInventory getPrimObjectInfo pk
          | i == llcInventoryScript -> getAnInventory getPrimScriptInfo pk
          | i == llcInventorySound -> getAnInventory getPrimSoundInfo pk
          | i == llcInventoryTexture -> getAnInventory getPrimTextureInfo pk
          | otherwise -> do lift $ logFromScript info ("invalid inventory type: " ++ lslValString invType)
                            return []

llGetInventoryNumber info@(ScriptInfo _ _ _ pk _) [invType@(IVal _)] = do
    runErrPrim pk [] (getPrimInventoryInfo info invType) >>= continueWith . IVal . length

llGetInventoryCreator info@(ScriptInfo _ _ _ pk _) [SVal name] =
    do result <- runErrPrim pk nullKey $ do
           all <- getAllInventoryInfo pk
           case findByInvName name all of
               Nothing -> (lift $ putChat (Just 20.0) pk 0 ("no item named '" ++ name ++ "'")) >> return nullKey
               Just (_,invInfo) -> return $ inventoryInfoCreator invInfo
       continueWith $ KVal result 

llGetInventoryKey info@(ScriptInfo _ _ _ pk _) [SVal name] =
    do result <- runErrPrim pk nullKey $ do
           all <- getAllInventoryInfo pk
           case findByInvName name all of
               Nothing -> return nullKey
               Just (InventoryItemIdentification (_,key),invInfo) -> 
                   if inventoryInfoTransferPerm invInfo && inventoryInfoModifyPerm invInfo && inventoryInfoCopyPerm invInfo then
                       return key
                   else return nullKey
       continueWith $ KVal result
       
llGetInventoryName info@(ScriptInfo _ _ _ pk _) [invType@(IVal _), IVal index] =
    runErrPrim pk "" (getPrimInventoryInfo info invType >>= return . map (\ (InventoryItemIdentification  (n,_),_) -> n) >>=
                      return . fromMaybe "" . lookupByIndex index) >>= continueWith . SVal

llGetInventoryType info@(ScriptInfo _ _ _ pk _) [SVal name] = do
        result <- runErrPrim pk (IVal (-1)) $ do
            val <- foldM findIt Nothing gettersAndTypes
            case val of
                Nothing -> return (IVal (-1))
                Just i -> return i
        continueWith result
    where findIt Nothing (f,t) =do
              list <- f pk
              case findByInvName name list of
                  Nothing -> return Nothing
                  Just _ -> return (Just t) 
          findIt v _ = return v
          gettersAndTypes = [(getPrimAnimationInfo,llcInventoryAnimation),(getPrimBodyPartInfo,llcInventoryBodyPart),
                             (getPrimClothingInfo,llcInventoryClothing),(getPrimGestureInfo,llcInventoryGesture),
                             (getPrimNotecardInfo,llcInventoryNotecard),(getPrimObjectInfo,llcInventoryObject),
                             (getPrimSoundInfo,llcInventorySound),(getPrimTextureInfo,llcInventoryTexture),
                             (getPrimScriptInfo,llcInventoryScript)]
------------------------------------------------------------------------------
llCloud info [v@(VVal x y z)] =
    do logFromScript info "llCloud currently returns default density"
       continueWith $ FVal 0

llWind info [v@(VVal x y z)] =
    do logFromScript info "llWind currently returns default density"
       continueWith $ VVal 0 0 0

llWater info [v@(VVal x y z)] =
    do logFromScript info "llWater currently returns default water elevation"
       continueWith $ FVal 0


llSitTarget (ScriptInfo _ _ _ pk _) [v@(VVal _ _ _),r@(RVal _ _ _ _)] =
    (setPrimSitTarget pk (Just (vVal2Vec v, rVal2Rot r)) >> continueWith VoidVal)
    
llAvatarOnSitTarget (ScriptInfo _ _ _ pk _) [] = do
    result <- runErrPrim pk nullKey $ do
        sitTarget <- getPrimSitTarget pk
        when (sitTarget == Nothing) mzero
        val <- getPrimSittingAvatar pk
        case val of
            Nothing -> return nullKey
            Just ak -> return ak
    continueWith (KVal result)

llUnSit info@(ScriptInfo _ _ _ pk _) [KVal k] = do
    runErrPrim pk () $ do
        val <- getPrimSittingAvatar pk
        case val of
            Nothing -> lift $ logFromScript info "llUnSit - no avatar sitting on prim (land unsit not implemented)"
            Just ak | ak == k -> lift $ setPrimSittingAvatar pk Nothing
                    | otherwise -> lift $ logFromScript info "llUnSit - unsit of avatar not sitting on prim attempted (land unsit not implemented)"
    continueWith VoidVal

llMessageLinked :: Monad m => ScriptInfo -> [LSLValue] -> WorldM m (EvalResult,LSLValue)       
llMessageLinked (ScriptInfo oid pid sid pkey _) [IVal link,IVal val,SVal msg,KVal key] =
    do mNumPrims <- queryObject oid (\ o -> return $ length (primKeys o))
       case mNumPrims of
           Nothing -> do logAMessage LogWarn "sim" ("object " ++ oid ++ " not found!")
                         return (EvalIncomplete,VoidVal)
           Just n -> 
              let targetList = targetLinks n link pid
              in
                   do mapM (flip (pushEvents oid) (Event "link_message" [IVal pid,IVal val,SVal msg, KVal key] [])) targetList
                      return (EvalIncomplete,VoidVal)

targetLinks n link pid =
    case link of
        l | l == linkSet -> [0 .. (n - 1)]
          | l == linkAllOthers -> [0..(pid - 1)] ++ [(pid + 1).. (n - 1)]
          | l == linkAllChildren -> [1..(n - 1)]
          | l == linkThis -> [pid]
          | otherwise -> [link - 1]
          
-- the key to 'sleep' is that instead of returning 'EvalIncomplete' as its EvalResult, it returns
-- 'YieldTil <some-tick>', which puts the script into a sleep state.
-- other functions which have a built in delay should use this mechanism as well.
llSleep info@(ScriptInfo oid pid sid pkey event) [FVal f] =
    do tick <- getTick
       return $ (YieldTil (tick + durationToTicks f),VoidVal)
       

llSay info params= chat (Just 20.0) info params
llWhisper info params = chat (Just 10.0) info params
llShout info params = chat (Just 100.0) info params
llRegionSay info params@[IVal chan, SVal message] =
    if (chan == 0) 
       then do logAMessage LogWarn (scriptInfoPrimKey info ++ ": " ++ scriptInfoScriptName info) "attempt to llRegionSay on channel 0"
               return (EvalIncomplete,VoidVal)
       else chat Nothing info params
       
chat range info@(ScriptInfo oid pid sid pkey event) [IVal chan, SVal message] =
    do logFromScript info $ concat ["chan = ", show chan, ", message = ", message]
       putChat range pkey chan message
       return (EvalIncomplete,VoidVal)
       
putChat range pk chan message = do
    (VVal x y z) <- getPos pk
    region <- getRegionIndex pk
    tick <- getTick
    Just name <- queryPrim pk primName
    putWorldEvent tick $ Chat chan name pk message (region,(x,y,z)) (Just 20.0)
    
llListen (ScriptInfo oid pid sid pkey event) [IVal chan, SVal sender, KVal key, SVal msg] =
    do lid <- registerListener $ Listener pkey sid chan sender key msg
       return (EvalIncomplete,IVal lid)
llListen info params = do
    logAMessage LogError "sim" ("invalid call to llListen.  info = " ++ (show info) ++ ", params = " ++ (show params))
    return (EvalIncomplete, IVal 0)
    
llListenRemove (ScriptInfo _ _ sid pk _) [IVal id] =
    runAndLogIfErr "can't remove listener" () (unregisterListener pk sid id) >> continueWith VoidVal 

llListenControl (ScriptInfo _ _ sid pk _) [IVal id, IVal active] =
    runAndLogIfErr "can't update listener" () (updateListener pk sid id $ active /= 0) >> continueWith VoidVal

llFrand _ [FVal maxval] =
    do r <- wrand
       continueWith $ FVal (maxval * r)

llEjectFromLand info@(ScriptInfo _ _ _ pk _) [KVal user] =
    runErrPrim pk () (do
        owner <- getPrimOwner pk
        regionIndex <- getPrimRegion pk
        (_,parcel) <- getPrimPosition pk >>= getParcelByPosition regionIndex
        if parcelOwner parcel == owner 
            then lift $ logFromScript info ("llEjectFromLand: user " ++ user ++ " ejected (in theory)")
            else lift $ logFromScript info ("llEjectFromLand: not permitted to eject from this parcel")
    ) >> continueWith VoidVal
llBreakLink info@(ScriptInfo oid _ sid pk _) [IVal link] = 
    runErrPrim pk () (do
            perm <- getPermissions pk sid
            if perm .&. cPermissionChangeLinks /= 0
                then do
                    objects <- lift getObjects
                    LSLObject links <- M.lookup oid objects
                    attachment <- getPrimAttachment oid
                    case attachment of
                        Nothing ->
                            if link < 1 || link > length links then lift $ logFromScript info ("llBreakLink: invalid link id")
                                else do
                                    let (xs,y:ys) = splitAt (link - 1) links
                                    let (linkSet1,linkSet2) = (xs ++ ys, [y])
                                    let objects' = if null linkSet1 then objects else M.insert (head linkSet1) (LSLObject linkSet1) objects
                                    lift $ setObjects (M.insert (head linkSet2) (LSLObject linkSet2) objects')
                                    t <- lift getTick
                                    when (not (null linkSet1)) $ lift $ putWorldEvent t (mkChangedEvent (head linkSet1) cChangedLink)
                                    lift $ putWorldEvent t (mkChangedEvent (head linkSet2) cChangedLink)
                        _ -> lift $ logFromScript info ("llBreakLink: can't change links of attached object")
                else lift $ logFromScript info ("llBreakLink: no permission") 
        ) >> continueWith VoidVal

llBreakAllLinks info@(ScriptInfo oid _ sid pk _) [] =
    runErrPrim pk () (do
            perm <- getPermissions pk sid
            if perm .&. cPermissionChangeLinks /= 0
                then do
                    objects <- lift getObjects
                    LSLObject links <- M.lookup oid objects
                    attachment <- getPrimAttachment oid
                    case attachment of
                        Nothing ->
                            if length links > 1 
                                then do
                                    -- order of parameters to union is vital: union is left biased, so we want the 
                                    -- new objects (in the left argument) to replace the old (in the right) where
                                    -- both exist (the root key of the old object is found in both)
                                    let objects' = M.union (M.fromList $ map (\ k -> (k, LSLObject [k])) links) objects
                                    lift $ setObjects objects'
                                    t <- lift getTick
                                    lift $ mapM_ (putWorldEvent t . (flip mkChangedEvent cChangedLink)) links
                                else return ()
                        _ -> lift $ logFromScript info ("llBreakAllLinks: can't change links of attached object")
                else lift $ logFromScript info ("llBreakAllLinks: no permission")
        ) >> continueWith VoidVal
        
-- TODO: verify link order
llCreateLink info@(ScriptInfo oid _ sid pk _) [KVal target, IVal iparent] =
    runErrPrim oid () (do
        perm <- getPermissions pk sid
        if perm .&. cPermissionChangeLinks /= 0
            then do
                objects <- lift getObjects
                LSLObject (link:links) <- M.lookup oid objects
                case M.lookup target objects of
                    Nothing -> lift $ logFromScript info "llCreateLink: target not found"
                    Just (LSLObject (link':links')) -> do
                        attachment0 <- getPrimAttachment oid
                        attachment1 <- getPrimAttachment target
                        case (attachment0,attachment1) of
                            (Nothing,Nothing) -> do
                                mask <- getObjectPermMask target cMaskOwner
                                ownerTarget <- getPrimOwner target
                                owner <- getPrimOwner oid
                                if mask .&. cPermModify /= 0 && owner == ownerTarget 
                                   then do
                                        let (newLinkset,deleteKey) = if parent 
                                                then ((link:link':links') ++ links, link')
                                                else ((link':link:links) ++ links', link)
                                        lift $ setObjects (M.insert (head newLinkset) (LSLObject newLinkset) $ M.delete deleteKey objects)
                                        t <- lift getTick
                                        lift $ putWorldEvent t (mkChangedEvent (head newLinkset) cChangedLink)
                                    else lift $ logFromScript info "llCreateLink: no modify permission on target"
                            _ -> lift $ logFromScript info ("llCreateLink: can't change links of attached object")
            else lift $ logFromScript info ("llCreateLink: no permission to change links")
        ) >> continueWith VoidVal
    where parent = iparent /= 0

llDie info@(ScriptInfo oid _ _ pk _) [] =
     runErrPrim oid (EvalIncomplete,VoidVal) $ do
         attachment <- getPrimAttachment oid
         case attachment of
             Nothing -> do
                     objects <- lift getObjects
                     LSLObject (links) <- M.lookup oid objects
                     allScriptInfo <- mapM getPrimScriptInfo links
                     let primsWithScriptNames = zip links (map (map (fst . inventoryItemNameKey . fst)) allScriptInfo)
                     let skeys = concat $ map ( \ (k,list) -> map ((,)k) list) primsWithScriptNames
                     lift $ mapM_ delScript skeys
                     lift $ mapM_ (\ link -> (getPrims >>= return . M.delete link >>= setPrims)) links
                     lift $ setObjects (M.delete oid objects)
                     lift $ logFromScript info ("object, and therefore this script, is dying")
                     return (EvalComplete Nothing,VoidVal)
                 where delScript (k,s) = getWorldScripts >>= return . M.delete (k,s) >>= setWorldScripts
             _ -> do lift $ logFromScript info ("llDie: attachment cannot die") 
                     return (EvalIncomplete,VoidVal)
        
llAttachToAvatar info@(ScriptInfo oid _ sid pk _) [IVal attachPoint] =
    if attachPoint `elem` validAttachmentPoints then
            runErrPrim pk () (do
                script <- (lift getWorldScripts >>= M.lookup (pk,sid))
                attachment <- getPrimAttachment oid
                case attachment of
                    Just _ -> lift $ logFromScript info "llAttachToAvatar: already attached"
                    Nothing ->
                        case scriptLastPerm script of
                            Nothing -> lift $ logFromScript info "llAttachToAvatar: no permission to attach"
                            Just k -> do
                                owner <- getPrimOwner pk
                                if (k /= pk)
                                    then lift $ putChat (Just 20.0) pk 0 ("Script trying to attach to someone other than owner!")
                                    else do 
                                        avatars <- lift getWorldAvatars
                                        let av = M.lookup k avatars
                                        case av of 
                                            Nothing -> lift $ logFromScript info ("llAttatchToAvatar: avatar not present in sim")
                                            Just av ->
                                                let attachments = avatarAttachments av in
                                                    case IM.lookup attachPoint attachments of
                                                        Nothing -> let av' = av { avatarAttachments = IM.insert attachPoint oid attachments } in
                                                            do lift $ setWorldAvatars (M.insert k av' avatars)
                                                               lift $ setPrimAttachment oid (Just $ Attachment k attachPoint)
                                                        Just _ -> lift $ logFromScript info ("llAttachToAvatar: attachment point already occupied")
            ) >> continueWith VoidVal
        else logFromScript info ("llAttachToAvatar: invalid attachment point: " ++ show attachPoint) >> continueWith VoidVal

llGetAttached info@(ScriptInfo oid _ _ _ _) [] = 
    runErrPrim oid 0 (getPrimAttachment oid >>= return . maybe 0 attachmentPoint) >>= continueWith . IVal
    
llGetPermissionsKey (ScriptInfo _ _ sid pk _) [] =
    fromErrorT nullKey (lift getWorldScripts >>= M.lookup (pk,sid) >>= return . fromMaybe nullKey . scriptLastPerm) >>= continueWith . KVal
    
llGetPermissions (ScriptInfo _ _ sid pk _) [] =  
    fromErrorT 0 (getPermissions pk sid) >>= continueWith . IVal

getPermissions pk s = do
    script <- (lift getWorldScripts >>= M.lookup (pk,s))
    case scriptLastPerm script of
        Nothing -> return 0
        Just k -> M.lookup k (scriptPermissions script)

llRequestPermissions (ScriptInfo _ _ sid pk _) [KVal k, IVal mask] =
    do avatars <- getWorldAvatars
       case M.lookup k avatars of
           Nothing -> logAMessage LogInfo (pk ++ ":" ++ sid) ("Invalid permissions request: no such avatar: " ++ k)
           _ -> getTick >>= flip putWorldEvent (PermissionRequestEvent pk sid k mask)
       continueWith VoidVal
       
llGetRot (ScriptInfo _ _ _ pk _) [] =
    runErrPrim pk (0,0,0,1) (getPrimRotation pk) >>= continueWith . rot2RVal

llGetLocalRot info@(ScriptInfo oid _ _ pk _) [] =
    if oid == pk then llGetRot info []
        else do result <- runErrPrim pk (0,0,0,1) $ do
                          (x,y,z,s) <- getPrimRotation oid
                          rot <- getPrimRotation pk
                          return $ quaternionMultiply rot (-x,-y,-z,s) -- TODO: check this! may be backward in some way
                continueWith (rot2RVal result)

llGetRootRotation info@(ScriptInfo oid _ _ pk _) [] =
    runErrPrim oid (0,0,0,1) (getPrimRotation oid) >>= continueWith . rot2RVal
    
--TODO: handle attachments...
--TODO: confirm order of rotations in both of these
llSetRot (ScriptInfo oid _ _ pk _) [r@(RVal _ _ _ _)] = 
    if oid == pk then setPrimRotation pk (rVal2Rot r) >> continueWith VoidVal
        else let rot = rVal2Rot r in do
            runErrPrim oid () $ do
                rootRot <- getPrimRotation oid
                lift $ setPrimRotation pk (rot `quaternionMultiply` rootRot `quaternionMultiply` rootRot)
            continueWith VoidVal

llSetLocalRot (ScriptInfo oid _ _ pk _) [r@(RVal _ _ _ _)] =
    if oid == pk then setPrimRotation pk (rVal2Rot r) >> continueWith VoidVal
        else let rot = rVal2Rot r in do
            runErrPrim oid () $ do
                rootRot <- getPrimRotation oid
                lift $ setPrimRotation pk (rootRot `quaternionMultiply` rot)
            continueWith VoidVal
            
llSetPos (ScriptInfo oid pid sid pkey event) [val] = 
    if oid == pkey 
        then setRootPos pkey val >> continueWith VoidVal
        else do
            -- i'm a child prim...
            rootPos <- getPos oid
            -- TODO: limit link distance...
            setPos pkey (liftV2 add3d rootPos val)
            continueWith VoidVal
            
-- updates the world coordinates of the root prim and all its child prims
-- does NOT consider region boundaries (TODO: fix!)
setRootPos oid v =
   runAndLogIfErr "" () $ do 
      vOld <- lift $ getPos oid
      let vec0 = vVal2Vec vOld
      let vec1 = vVal2Vec v
      let vec = diff3d vec1 vec0
      let dist = mag3d vec
      let vec' = if dist <= 10.0 
              then vec
              else scale3d 10.0 $ norm3d vec
      objects <- lift getObjects
      (LSLObject keys) <- M.lookup oid objects
      lift $ mapM_ (flip addPos (vec2VVal vec')) keys

addPos key v = getPos key >>= (setPos key) . (liftV2 add3d v)
setPos key v = setPrimPosition key (vVal2Vec v)

llGetOwner info [] = 
    let k = scriptInfoPrimKey info in (runErrPrim k nullKey $ getPrimOwner k) >>= continueWith . KVal
llGetCreator info [] =
    let k = scriptInfoPrimKey info in runErrPrim k nullKey (getPrimCreator k) >>= continueWith . KVal
    
llGetLinkKey (ScriptInfo oid _ _ _ _) [IVal link] = do
    result <- runAndLogIfErr ("object " ++ oid ++ " or link " ++ show link ++ "not found") nullKey $ do
        LSLObject pkeys <- getObject oid
        pk <- lookupByIndex (link - 1) pkeys
        return pk
    continueWith (KVal result)

llGetLinkName (ScriptInfo oid _ _ _ _) [IVal link] = do
    result <- runAndLogIfErr ("object " ++ oid ++ " or link " ++ show link ++ "not found") nullKey $ do
        LSLObject pkeys <- getObject oid
        pk <- lookupByIndex (link - 1) pkeys
        prim <- getPrim pk
        return (primName prim)
    continueWith (SVal result)
    
-- TODO: should check for av/prim in same region
-- TODO: no concept of online/offline for av           
llKey2Name info [KVal k] =
    do avs <- getWorldAvatars
       case M.lookup k avs of
           Just av -> continueWith $ SVal (avatarName av)
           _ -> do
               prims <- getPrims
               case M.lookup k prims of
                   Just prim -> continueWith $ SVal (primName prim)
                   Nothing -> continueWith (SVal "")
llOwnerSay info [SVal s] =
    do logAMessage LogInfo (infoToLogSource info) ("Owner Say: " ++ s)
       continueWith VoidVal
       
llGetPos (ScriptInfo _ _ _ pk _) [] = getPos pk >>= continueWith
llGetRootPosition (ScriptInfo oid _ _ _ _) [] = getPos oid >>= continueWith

llGetLocalPos (ScriptInfo oid _ _ pk _) [] | oid == pk = getPos oid >>= continueWith
                                           | otherwise =
    (runAndLogIfErr ("parent " ++ oid ++ " or child " ++ pk ++ " not found") 
                    (0,0,0) 
                    (liftM2 diff3d (getPrimPosition pk) (getPrimPosition oid))) >>= 
                    continueWith . vec2VVal
                    
llGetAlpha (ScriptInfo _ _ _ pkey _) [IVal side] =
    computeAlpha >>= continueWith
    where computeAlpha = if (side /= -1) 
            then runErr $ (getPrimFaceAlpha pkey side >>= return . FVal)
            else runErr $ do
                faces <- getPrimFaces pkey
                let n = length faces
                let alpha = if n > 0 then sum (map faceAlpha faces) / (fromInt $ length faces)
                                     else 1.0
                return $ FVal alpha
          runErr = runErrFace pkey side (FVal 1.0)

llSetAlpha (ScriptInfo _ _ _ pkey _) [FVal alpha, IVal face] = setAlpha alpha face pkey

setAlpha alpha face pkey =
    if face == -1
        then runErrFace pkey face (EvalIncomplete,VoidVal) $ do 
                faces <- getPrimFaces pkey
                let faces' = map (\ face -> face { faceAlpha = alpha }) faces
                lift $ setPrimFaces pkey faces'
                continueWith VoidVal
        else setPrimFaceAlpha pkey face alpha >> continueWith VoidVal

llSetLinkAlpha (ScriptInfo oid pid _ _ _) [IVal link, FVal alpha, IVal face] = do
    pks <- getTargetPrimKeys oid link pid
    mapM_ (setAlpha alpha face) pks
    continueWith VoidVal

getTargetPrimKeys oid link pid = do
    LSLObject prims <- runAndLogIfErr ("can't find object " ++ oid) (LSLObject []) $ getObject oid
    let targetList = targetLinks (length prims) link pid
    mapM (flip lookupByIndex prims) targetList
    
llGetColor (ScriptInfo _ _ _ pkey _) [IVal side] =
    computeColor >>= continueWith
    where computeColor = if (side /= -1)
            then runErr $ (getPrimFaceColor pkey side >>= return . vec2VVal)
            else runErr $ do
                faces <- getPrimFaces pkey
                let n = length faces
                let color = if n > 0 then scale3d (1.0/ (fromInt $ length faces) ) 
                                                  (foldr add3d (0.0,0.0,0.0) (map faceColor faces))
                                          else (1.0,1.0,1.0)
                return $ vec2VVal color
          runErr = runErrFace pkey side (VVal 1.0 1.0 1.0)

llSetColor (ScriptInfo _ _ _ pkey _) [color, IVal face] = setColor color face pkey

setColor color face pkey =
    if face == -1
        then runErrFace pkey face (EvalIncomplete,VoidVal) $ do
                faces <- getPrimFaces pkey
                let colorVal = vVal2Vec color
                let faces' = map (\ face -> face { faceColor = colorVal }) faces
                lift $ setPrimFaces pkey faces'
                continueWith VoidVal
        else setPrimFaceColor pkey face (vVal2Vec color) >> continueWith VoidVal

llSetLinkColor (ScriptInfo oid pid _ _ _) [IVal link, color, IVal face] = do
    pks <- getTargetPrimKeys oid link pid
    mapM (setColor color face) pks
    continueWith VoidVal

llGetTextureOffset (ScriptInfo _ _ _ pkey _) [IVal face] =
    let face' = if face == -1 then 0 else face in
        runErrFace pkey face (0,0,0) (getPrimFaceTextureInfo pkey face >>= return . textureOffsets) >>= continueWith . vec2VVal
llGetTextureScale (ScriptInfo _ _ _ pkey _) [IVal face] =
    let face' = if face == -1 then 0 else face in
        runErrFace pkey face (0,0,0) (getPrimFaceTextureInfo pkey face >>= return . textureRepeats) >>= continueWith . vec2VVal
llGetTextureRot (ScriptInfo _ _ _ pkey _) [IVal face] =
    let face' = if face == -1 then 0 else face in
        runErrFace pkey face 0 (getPrimFaceTextureInfo pkey face >>= return . textureRotation) >>= continueWith . FVal
        
llSetTexture (ScriptInfo _ _ _ pkey _) [SVal texture,IVal face] = setTexture texture face pkey

-- TODO: worry about texture permissions
llGetTexture (ScriptInfo _ _ _ pkey _) [IVal face] =
    let face' = if face == -1 then 0 else face in
        runErrFace pkey face "" (do
            info <- getPrimFaceTextureInfo pkey face
            invTextures <- getPrimTextureInfo pkey
            let tname = textureName info
            case find ((tname==) . snd . inventoryItemNameKey . fst) invTextures of
                 Nothing -> return tname
                 Just (idInfo,_) -> return $ fst $ inventoryItemNameKey idInfo) >>= continueWith . SVal
                 
llSetLinkTexture (ScriptInfo oid pid _ _ _) [IVal link, SVal texture,IVal face] = do
    pks <- getTargetPrimKeys oid link pid
    mapM (setTexture texture face) pks
    continueWith VoidVal
    
setTexture texture face pkey = 
    if face == -1
        then runErrFace pkey face (EvalIncomplete,VoidVal) $ do
                faces <- getPrimFaces pkey
                let faces' = map (\ face -> 
                        let info = faceTextureInfo face in 
                            face { faceTextureInfo = info { textureName = texture } }) faces
                lift $ setPrimFaces pkey faces'
                continueWith VoidVal
        else runErrFace pkey face (EvalIncomplete,VoidVal) $ do
            faces <- getPrimFaces pkey
            f <- lookupByIndex face faces
            let tInfo = faceTextureInfo f
            updatePrimFace pkey face (\ f -> f { faceTextureInfo = tInfo { textureName = texture } })
            continueWith VoidVal
            
getPos pkey = runErrPrim pkey
                  (VVal 0.0 0.0 0.0)
                  (getPrimPosition pkey >>= return . vec2VVal)

getRegionIndex pkey = return (0,0)

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
        
llGetNumberOfPrims (ScriptInfo oid _ _ pkey _) [] = 
    do result <- runAndLogIfErr ("object " ++ oid ++ " not found") (IVal 1) $
            do objects <- lift getObjects
               (LSLObject prims) <- M.lookup oid objects
               return $ IVal (length prims)
       continueWith result

llGetObjectPrimCount info@(ScriptInfo oid _ _ pkey _) [KVal k] =
    do result <- runAndLogIfErr ("object " ++ oid ++ " not found") (IVal 0) $ do
            region <- getPrimRegion oid
            p <- lift $ evalErrorT $ getPrim k
            case p of
                Left _ -> lift (logFromScript info ("object " ++ k ++ " does not exist")) >> return (IVal 0)
                Right prim -> if region /= primRegion prim 
                                    then return (IVal 0)
                                    else case primParent prim of
                                        Nothing -> primCount k
                                        Just k' -> primCount k'
       continueWith result   
    where primCount oid = do
            objects <- lift getObjects
            case M.lookup oid objects of
                Nothing -> lift (logAMessage LogWarn "sim" ("object prim " ++ oid ++ "not found"))
                    >> return (IVal 0)
                Just (LSLObject l) -> return $ IVal $ length l                               

llGetNumberOfSides (ScriptInfo _ _ _ pkey _) [] =
     (runErrPrim pkey (IVal 1)
        (getPrimFaces pkey >>= return . IVal . length)) >>= continueWith
     
llGetObjectDesc (ScriptInfo _ _ _ pkey _) [] =
    (runErrPrim pkey (SVal "default description")
        (getPrimDescription pkey >>= return . SVal)) >>= continueWith
llGetObjectName (ScriptInfo _ _ _ pkey _) [] =
    (runErrPrim pkey (SVal "default name")
        (getPrimName pkey >>= return . SVal)) >>= continueWith

llSetObjectName (ScriptInfo _ _ _ pkey _) [SVal name] =
    setPrimName pkey (take 255 name) >> continueWith VoidVal
    
llSetObjectDesc (ScriptInfo _ _ _ pkey _) [SVal desc] =
    setPrimDescription pkey (take 127 desc) >> continueWith VoidVal
    
llGetObjectPermMask (ScriptInfo oid _ _ _ _) [IVal maskId] =
    do result <- runAndLogIfErr ("object " ++ oid ++ " not found") 0 (getObjectPermMask oid maskId) 
       continueWith $ IVal result

getObjectPermMask oid maskId = do
       masks <- getPrimPermissions oid
       let base = if null masks then 0x0008e000 else masks !! 0
       let n = length masks
       return (if maskId `elem` [0..(n-1)] then masks !! maskId else 
                      if maskId == 3 then 0 else base)
          
llGetScale (ScriptInfo _ _ _ pkey _) [] =
    runErrPrim pkey (1.0, 1.0, 1.0) (getPrimScale pkey) >>= continueWith . vec2VVal
    
llSetScale (ScriptInfo _ _ _ pk _) [scale] =
    if tooSmall then continueWith VoidVal
       else setPrimScale pk clippedVec >> continueWith VoidVal
    where (x,y,z) = vVal2Vec scale
          tooSmall = (x < 0.01 || y < 0.01 || z < 0.01)
          clippedVec = (min x 10.0, min y 10.0, min z 10.0) 
  
llGetBoundingBox _ [KVal k] =
    do 
        logAMessage LogInfo "sim" "note: llGetBoundingBox does not return accurate results (yet)"
        avatars <- getWorldAvatars
        case M.lookup k avatars of
            Just avatar -> let pos = avatarPosition avatar in
                continueWith (LVal $ map (vec2VVal . (add3d pos)) [(-1.0,-1.0,-1.0),(1.0,1.0,1.0)])
            Nothing -> do
                result <- evalErrorT (getPrimParent k)
                case result of
                    Left _ -> continueWith $ LVal [VVal 0.0 0.0 0.0]
                    Right Nothing ->  primBox k
                    Right (Just oid) -> primBox oid        
    where primBox pk = do
            (pos,(xs,ys,zs)) <- fromErrorT ((128,128,0),(1,1,1)) $ do
                pos <- getPrimPosition k
                scale <- getPrimScale k
                return (pos,scale)
            continueWith (LVal $ map (vec2VVal . (add3d pos)) [(-1.0 * xs, -1.0 * ys, -1.0 * zs),
                                                               (1.0 * xs, 1.0 * ys, 1.0 * zs)])


llSetTimerEvent (ScriptInfo _ _ sn pk _) [FVal interval] =
    -- TODO: this may not accurately reflect buggy behavior in SL
    do removePendingTimerEvent pk sn
       t <- getTick
       putWorldEvent (t + durationToTicks interval) (TimerEvent interval (pk,sn))
       continueWith VoidVal
    where removePendingTimerEvent pk sn = do
              wq <- getWQueue
              let wq' = flip filter wq $ \ e -> case e of
                      (_,TimerEvent _ (pk',sn')) -> pk /= pk' || sn /= sn'
                      _ -> True
              setWQueue wq
--------------
-- get/set prim parameters
llGetPrimitiveParams (ScriptInfo _ _ _ pk _) [LVal l] = (runErrPrim pk [] $ getPrimParameters pk l) >>= continueWith . LVal
llSetPrimitiveParams (ScriptInfo _ _ _ pk _) [LVal l] = (runAndLogIfErr "problem updating prim" () $ setPrimParameters pk l) >> continueWith VoidVal

llSetLinkPrimitiveParams (ScriptInfo oid pid _ _ _) [IVal link,LVal l] = do
    runAndLogIfErr ("problem setting link parameters for object " ++ oid) () $ do
        LSLObject pks <- getObject oid
        let targetList = targetLinks (length pks) link pid
        pkList <- mapM (flip lookupByIndex pks) targetList
        mapM (flip setPrimParameters l) pkList
        return ()
    continueWith VoidVal
    
Just(Constant _ llcPrimBumpShiny) = findConstant "PRIM_BUMP_SHINY" 
Just (Constant _ llcPrimColor) = findConstant "PRIM_COLOR"
Just (Constant _ llcPrimTexture) = findConstant "PRIM_TEXTURE"
Just (Constant _ llcPrimTexgen) = findConstant "PRIM_TEXGEN"
Just (Constant _ llcPrimFullbright) = findConstant "PRIM_FULLBRIGHT"

Just (Constant _ llcPrimMaterial) = findConstant "PRIM_MATERIAL"
Just (Constant _ llcPrimPhantom) = findConstant "PRIM_PHANTOM"
Just (Constant _ llcPrimPhysics) = findConstant "PRIM_PHYSICS"
Just (Constant _ llcPrimFlexible) = findConstant "PRIM_FLEXIBLE"
Just (Constant _ llcPrimPointLight) = findConstant "PRIM_POINT_LIGHT"
Just (Constant _ llcPrimPosition) = findConstant "PRIM_POSITION"
Just (Constant _ llcPrimRotation) = findConstant "PRIM_ROTATION"
Just (Constant _ llcPrimSize) = findConstant "PRIM_SIZE"
Just (Constant _ llcPrimTempOnRez) = findConstant "PRIM_TEMP_ON_REZ"
Just (Constant _ llcPrimType) = findConstant "PRIM_TYPE"

getPrimParameters pk params =
    case params of
        [] -> return []
        (x:xs) | x `elem` [llcPrimBumpShiny,llcPrimFullbright,llcPrimColor,llcPrimTexture,llcPrimTexgen] && null xs -> return []
               | x == llcPrimBumpShiny  -> queryAndDoRest (queryPrimBumpShiny $ head xs) (tail xs)
               | x == llcPrimColor -> queryAndDoRest (queryPrimColor $ head xs) (tail xs)
               | x == llcPrimTexture -> queryAndDoRest (queryPrimTexture $ head xs) (tail xs)
               | x == llcPrimTexgen -> queryAndDoRest (queryPrimTexgen $ head xs) (tail xs)
               | x == llcPrimMaterial -> queryAndDoRest queryPrimMaterial xs
               | x == llcPrimPhantom -> queryAndDoRest (queryPrimStatus primPhantomBit) xs
               | x == llcPrimPhysics -> queryAndDoRest (queryPrimStatus primPhysicsBit) xs
               | x == llcPrimFlexible -> queryAndDoRest queryPrimFlexible xs
               | x == llcPrimPointLight -> queryAndDoRest queryPrimLight xs
               | x == llcPrimPosition -> queryAndDoRest queryPrimPosition xs
               | x == llcPrimRotation -> queryAndDoRest queryPrimRotation xs
               | x == llcPrimSize -> queryAndDoRest queryPrimScale xs
               | x == llcPrimTempOnRez -> queryAndDoRest queryPrimTempOnRez xs
               | x == llcPrimType -> queryAndDoRest queryPrimType xs
               | x == IVal 1 -> queryAndDoRest queryPrimTypeOldSkool xs
               | otherwise -> getPrimParameters pk xs
    where queryAndDoRest q rest = liftM2 (++) (q pk) (getPrimParameters pk rest)

queryPrimTempOnRez k = getPrimTempOnRez k >>= return . return . IVal . (\ b -> if b then 1 else 0)
queryPrimRotation k = getPrimRotation k >>= return . return . rot2RVal
queryPrimPosition k = getPrimPosition k >>= return . return . vec2VVal
queryPrimScale k = getPrimScale k >>= return . return . vec2VVal
queryPrimFlexible k = 
    getPrimFlexibility k >>= (\ flex -> case flex of
        Nothing -> return [IVal 0, IVal 0, FVal 0, FVal 0, FVal 0, FVal 0, VVal 0.0 0.0 0.0]
        Just flex' -> return $ map ($flex') [const (IVal 1),IVal . flexSoftness, FVal . flexGravity, FVal . flexFriction,
                                             FVal . flexWind, FVal . flexTension, vec2VVal . flexForce])
queryPrimLight k =
    getPrimLight k >>= (\ light -> case light of
        Nothing -> return [IVal 0, VVal 0 0 0, FVal 0, FVal 0, FVal 0]
        Just light' -> return $ map ($light') [const (IVal 1), vec2VVal . lightColor, FVal . lightIntensity, FVal . lightRadius, FVal . lightFalloff])
    
queryPrimMaterial k = getPrimMaterial k >>= return . return . IVal
queryPrimStatus bit k =  getPrimStatus k >>= return . return . IVal . (\ i -> if testBit i bit then 1 else 0)
queryPrimBumpShiny side =  queryFaceVals bumpShiny side
    where bumpShiny face = [IVal $ faceShininess face, IVal $ faceBumpiness face]
queryPrimColor side = queryFaceVals colorAlpha side
    where colorAlpha face = [vec2VVal $ faceColor face, FVal $ faceAlpha face]
queryPrimTexture side = queryFaceVals textureInfo side
    where textureInfo face = let tinfo = faceTextureInfo face in map ($tinfo) 
                                   [SVal .textureName,vec2VVal . textureRepeats,vec2VVal . textureOffsets,FVal . textureRotation]
queryPrimTexgen side = queryFaceVals (return . IVal . faceTextureMode) side
queryPrimFullbright side = queryFaceVals (\ face -> if faceFullbright face then [IVal 1] else [IVal 0]) side

queryFaceVals f (IVal side) k = 
    if side == -1
        then getPrimFaces k >>= return . concat . (map f)
        else getPrimFaces k >>= fromErrorT [] . (\ faces -> (lookupByIndex side faces >>= return . f))

Just (Constant _ llcObjectUnknownDetail) = findConstant "OBJECT_UNKNOWN_DETAIL"
Just (Constant _ llcObjectName) = findConstant "OBJECT_NAME"
Just (Constant _ llcObjectDesc) = findConstant "OBJECT_DESC"
Just (Constant _ llcObjectPos) = findConstant "OBJECT_POS"
Just (Constant _ llcObjectRot) = findConstant "OBJECT_ROT"
Just (Constant _ llcObjectVelocity) = findConstant "OBJECT_VELOCITY"
Just (Constant _ llcObjectOwner) = findConstant "OBJECT_OWNER"
Just (Constant _ llcObjectGroup) = findConstant "OBJECT_GROUP"
Just (Constant _ llcObjectCreator) = findConstant "OBJECT_CREATOR"
Just (Constant _ llcPrimTypeBox) = findConstant "PRIM_TYPE_BOX"
Just (Constant _ llcPrimTypePrism) = findConstant "PRIM_TYPE_PRISM"
Just (Constant _ llcPrimTypeTorus) = findConstant "PRIM_TYPE_TORUS"
Just (Constant _ llcPrimTypeCylinder) = findConstant "PRIM_TYPE_CYLINDER"
Just (Constant _ llcPrimTypeSphere) = findConstant "PRIM_TYPE_SPHERE"
Just (Constant _ llcPrimTypeRing) = findConstant "PRIM_TYPE_RING"
Just (Constant _ llcPrimTypeTube) = findConstant "PRIM_TYPE_TUBE"
Just (Constant _ llcPrimTypeSculpt) = findConstant "PRIM_TYPE_SCULPT"

IVal icPrimTypeSphere = llcPrimTypeSphere
IVal icPrimTypeSculpt = llcPrimTypeSculpt
IVal icPrimTypeTorus = llcPrimTypeTorus
IVal icPrimTypeTube = llcPrimTypeTube

queryPrimType k = do
    (PrimType version typecode holeshape cut twist holesize topshear hollow taper advancedcut roffset revs skew sculpt sculptType) <- 
        getPrimTypeInfo k
    case typecode of
       i | (IVal i) `elem` [llcPrimTypeBox,llcPrimTypeCylinder,llcPrimTypePrism] -> 
                      return [IVal i,IVal holeshape, vec2VVal cut, FVal hollow, vec2VVal twist, vec2VVal taper, vec2VVal topshear]
         | (IVal i) == llcPrimTypeSphere -> return [IVal i,IVal holeshape, vec2VVal cut, FVal hollow, vec2VVal twist, vec2VVal advancedcut]
         | (IVal i) `elem` [llcPrimTypeRing,llcPrimTypeTorus,llcPrimTypeTube] -> 
                      return [IVal i,IVal holeshape, vec2VVal cut, FVal hollow, vec2VVal twist, vec2VVal holesize, vec2VVal topshear, 
                    vec2VVal advancedcut, vec2VVal taper, FVal revs, FVal roffset, FVal skew]
         | (IVal i) == llcPrimTypeSculpt -> return [IVal i,SVal $ fromMaybe "" sculpt, IVal sculptType]
         | otherwise -> return []

queryPrimTypeOldSkool k = do
    (PrimType version typecode holeshape cut twist holesize topshear hollow taper advancedcut roffset revs skew sculpt sculptType) <- 
        getPrimTypeInfo k
    case typecode of
       i | (IVal i) `elem` [llcPrimTypeBox,llcPrimTypeCylinder,llcPrimTypePrism] -> 
                      return [IVal i, vec2VVal cut, FVal hollow, FVal $ yOf twist, vec2VVal taper, vec2VVal topshear]
         | (IVal i) == llcPrimTypeSphere -> return [IVal i,vec2VVal cut, FVal hollow,vec2VVal advancedcut]
         | (IVal i) == llcPrimTypeTorus -> 
                      return [IVal i, vec2VVal cut, FVal hollow, FVal $ yOf twist, FVal $ yOf taper, vec2VVal topshear, vec2VVal advancedcut]
         | (IVal i) == llcPrimTypeTube -> 
                      return [IVal i, vec2VVal cut, FVal hollow, FVal $ yOf twist, FVal $ xOf topshear]
         | otherwise -> return []
    where xOf (x,y,z) = x
          yOf (x,y,z) = y

setPrimParameters key params = do
    prim <- getPrim key
    (prim',_) <- updatePrimParameters (prim,params)
    lift $ setPrim key prim'
    return ()
    
updatePrimParameters (prim,[]) = return (prim,[])
updatePrimParameters (prim,code:rest) = do
    result <- case code of
        i | i == llcPrimTempOnRez -> updatePrimTempOnRez prim rest
          | i == llcPrimMaterial -> updatePrimMaterial prim rest
          | i == llcPrimPhantom -> updatePrimPhantom prim rest
          | i == llcPrimPhysics -> updatePrimPhysics prim rest
          | i == llcPrimPosition -> updatePrimPosition prim rest
          | i == llcPrimRotation -> updatePrimRotation prim rest
          | i == llcPrimSize -> updatePrimScale prim rest
          | i == llcPrimFlexible -> updatePrimFlexible prim rest
          | i == llcPrimPointLight -> updatePrimLight prim rest
          | i == llcPrimBumpShiny -> updatePrimBumpShiny prim rest
          | i == llcPrimColor -> updatePrimColor prim rest
          | i == llcPrimTexture -> updatePrimTexture prim rest
          | i == llcPrimTexgen -> updatePrimTexgen prim rest
          | i == llcPrimFullbright -> updatePrimFullbright prim rest
          | i == llcPrimType -> updatePrimType prim rest
          | i == IVal 1 -> updatePrimTypeOldSkool prim rest
          | otherwise -> fail "incorrect parameter"
    updatePrimParameters result
    
updatePrimType prim (primCode@(IVal i):rest) | primCode == llcPrimTypeSphere = updatePrimTypeSphere prim rest
                                             | primCode `elem` [llcPrimTypeBox,llcPrimTypeCylinder,llcPrimTypePrism] =
                                                 updatePrimTypeBoxCylPrism i prim rest
                                             | primCode `elem` [llcPrimTypeTorus,llcPrimTypeTube,llcPrimTypeRing] =
                                                 updatePrimTypeRingTorusTube i prim rest
                                             | primCode == llcPrimTypeSculpt = updatePrimTypeSculpt prim rest
                                             | otherwise = fail "incorrect parameters for PRIM_TYPE"
updatePrimType _ _ = fail "insufficient or incorrect parameters for PRIM_TYPE"

updatePrimTypeOldSkool prim (primCode@(IVal i):rest) | primCode == llcPrimTypeSphere = updatePrimTypeSphereOld prim rest
                                                     | primCode `elem` [llcPrimTypeBox,llcPrimTypeCylinder,llcPrimTypePrism] =
                                                         updatePrimTypeBoxCylPrismOld i prim rest
                                                     | primCode == llcPrimTypeTorus = updatePrimTypeTorusOld prim rest
                                                     | primCode == llcPrimTypeTube = updatePrimTypeTubeOld prim rest
                                                     | otherwise = fail "incorrect parameters for deprecated prim type"
updatePrimTypeOldSkool _ _ = fail "insufficient or incorrect parameters for deprecated prim type"

updatePrimTempOnRez prim (IVal tempOnRez:rest) = return (prim { primTempOnRez = if tempOnRez /= 0 then True else False }, rest)
updatePrimTempOnRez _ _ = fail "insufficient or incorrect parameters for PRIM_TEMP_ON_REZ"
updatePrimMaterial prim (IVal material:rest) = return (prim { primMaterial = material }, rest)
updatePrimMaterial _ _ = fail "insufficient or incorrect parameters for PRIM_MATERIAL"

updatePrimPhantom prim params = updatePrimStatus "insufficient or incorrect parameters for PRIM_PHANTOM" primPhantomBit prim params
updatePrimPhysics prim params = updatePrimStatus "insufficient or incorrect parameters for PRIM_PHYSICS" primPhysicsBit prim params

updatePrimStatus _ bit prim (IVal i:rest) = return (prim { primStatus = if i == 0 then clearBit (primStatus prim) bit
                                                                                  else setBit (primStatus prim) bit }, rest)
updatePrimStatus fMsg _ _ _ = fail fMsg

updatePrimPosition prim (pos@(VVal _ _ _):rest) = return (prim { primPosition = vVal2Vec pos }, rest)
updatePrimPosition _ _ = fail "insufficient or incorrect parameters for PRIM_POSITION"
updatePrimRotation prim (rot@(RVal _ _ _ _):rest) = return (prim { primRotation = rVal2Rot rot }, rest)
updatePrimRotation _ _ = fail "insufficient or incorrect parameters for PRIM_ROTATION"
updatePrimScale prim (scale@(VVal _ _ _):rest) = return (prim { primScale = vVal2Vec scale }, rest)
updatePrimScale _ _ = fail "insufficient or incorrect parameters for PRIM_SIZE"

updatePrimFlexible prim (IVal flex:IVal soft:FVal gravity:FVal friction:FVal wind:FVal tension:VVal fx fy fz:rest) =
    return (prim { primFlexibility = if flex == 0 then Nothing else Just (Flexibility soft gravity friction wind tension (fx,fy,fz)) }, rest)
updatePrimFlexible _ _ = fail "insufficient or incorrect parameters for PRIM_FLEXIBLE"

updatePrimLight prim (IVal light:VVal r g b:FVal intensity:FVal radius:FVal falloff:rest) =
    return (prim { primLight = if light == 0 then Nothing else Just (LightInfo (r,g,b) intensity radius falloff) }, rest)
updatePrimLight _ _ = fail "insufficient or incorrect parameters for PRIM_POINT_LIGHT"
updatePrimBumpShiny prim params =
    let extract (IVal face:IVal bump:IVal shiny:rest) = return (face, [IVal bump, IVal shiny], rest)
        extract _ = fail ("insufficient or incorrect parameters for PRIM_BUMP_SHINY")
        update [IVal bump, IVal shiny] face = face { faceBumpiness = bump, faceShininess = shiny }
    in updatePrimFaceParams prim params extract update
updatePrimColor prim params =
    let extract (IVal face:VVal r g b:FVal alpha:rest) = return (face,[VVal r g b, FVal alpha], rest)
        extract _ = fail ("insufficient or incorrect parameters for PRIM_COLOR")
        update [color, FVal alpha] face = face { faceColor = vVal2Vec color, faceAlpha = alpha }
    in updatePrimFaceParams prim params extract update
updatePrimTexture prim params =
    let extract (IVal face:name@(SVal _):repeats@(VVal _ _ _):offsets@(VVal _ _ _):rotation@(FVal _):rest) = 
            return (face,[name,repeats,offsets,rotation],rest)
        extract _ = fail ("insufficient or incorrect parameters for PRIM_TEXTURE")
        update [SVal name,repeats,offsets,FVal rotation] face = 
            face { faceTextureInfo = TextureInfo name (vVal2Vec repeats) (vVal2Vec offsets) rotation }
    in updatePrimFaceParams prim params extract update
updatePrimTexgen prim params =
    let extract (IVal face:IVal mode:rest) = return (face,[IVal mode],rest)
        extract _ = fail "insufficient or incorrect parameters for PRIM_TEXGEN"
        update [IVal mode] face = face { faceTextureMode = mode }
    in updatePrimFaceParams prim params extract update
updatePrimFullbright prim params =
    let extract (IVal face:IVal fullbright:rest) = return (face,[IVal fullbright],rest)
        extract _ = fail "insufficient or incorrect parameters for PRIM_FULLBRIGHT"
        update [IVal fullbright] face = face { faceFullbright = if fullbright == 0 then False else True }
    in updatePrimFaceParams prim params extract update
updatePrimFaceParams prim params extract update = do 
    (face, faceParams, rest) <- extract params -- this can fail
    if face == -1
        then return (prim { primFaces = map (update faceParams) $ primFaces prim }, rest)
        else let (xs,ys) = splitAt face (primFaces prim) in
            return (if null ys then prim else prim { primFaces = xs ++ [(update faceParams $ head ys)] ++ (tail ys) }, rest)

updatePrimTypeBoxCylPrism ptype prim (IVal holeshape:VVal cx cy cz:FVal hollow:VVal twx twy twz:VVal tx ty tz:VVal sx sy sz:rest) =
    return (prim { primTypeInfo = (primTypeInfo prim) { primTypeCode = ptype, primHoleshape = holeshape, primCut = (cx,cy,cz),
                                                primHollow = hollow, primTwist = (twx,twy,twz), primTaper = (tx,ty,tz),
                                                primTopshear = (sx,sy,sz) }}, rest)
updatePrimTypeBoxCylPrism _ _ _ = fail "insufficient or incorret parameters for PRIM_TYPE (PRIM_TYPE_PRISM, PRIM_TYPE_CYLINDER, or PRIM_TYPE_BOX)"
updatePrimTypeSphere prim (IVal holeshape:VVal cx cy cz:FVal hollow:VVal tx ty tz:VVal ax ay az:rest) =
    return (prim { primTypeInfo = (primTypeInfo prim) { primTypeCode = icPrimTypeSphere, primHoleshape = holeshape, primCut = (cx,cy,cz),
                                                primHollow = hollow, primTwist = (tx,ty,tz), primAdvancedCut = (ax,ay,az) } }, rest)
updatePrimTypeSphere _ _ = fail "insufficient or incorrect parameters for PRIM_TYPE (PRIM_TYPE_SPHERE)"
updatePrimTypeRingTorusTube ptype prim (IVal holeshape:VVal cx cy cz:FVal hollow:VVal twx twy twz:VVal hx hy hz:VVal sx sy sz:
                                        VVal ax ay az:VVal tx ty tz:FVal revs:FVal roffset:FVal skew:rest) =
    return (prim { primTypeInfo = (primTypeInfo prim) { primTypeCode = ptype, primHoleshape = holeshape, primCut = (cx,cy,cz),
                                                primHollow = hollow, primTwist = (twx,twy,twz), primHolesize = (hx,hy,hz),
                                                primTopshear = (sx,sy,sz), primAdvancedCut = (ax,ay,az), primTaper = (tx,ty,tz),
                                                primRevolutions = revs, primRadiusOffset = roffset, primSkew = skew } }, rest)
updatePrimTypeRingTorusTube _ _ _ = fail "insufficient or incorrect parameters for PRIM_TYPE (PRIM_TYPE_RING, PRIM_TYPE_TORUS, or PRIM_TYPE_TUBE)"
updatePrimTypeSculpt prim (SVal name:IVal sculptType:rest) =
    return (prim { primTypeInfo = (primTypeInfo prim) { primTypeCode = icPrimTypeSculpt, primSculptTexture = Just name, primSculptType = sculptType } }, rest)
updatePrimTypeSculpt _ _ = fail "insufficient or incorrect parameters for PRIM_TYPE (PRIM_TYPE_SCULPT)"

updatePrimTypeBoxCylPrismOld ptype prim (VVal cx cy cz:FVal hollow:FVal twisty:VVal tx ty tz:VVal sx sy sz:rest) =
    let info = primTypeInfo prim in
    return (prim { primTypeInfo = info { primTypeCode = ptype, primCut = (cx,cy,cz), primHollow = hollow,
                                          primTwist = let (x,y,z) = primTwist info in (x,twisty,z), primTaper = (tx,ty,tz),
                                          primTopshear = (sx,sy,sz) } }, rest)
updatePrimTypeBoxCylPrismOld _ _ _ = fail "insufficient or inccorrect parameters for deprecated prim type (PRIM_TYPE_BOX, PRIM_TYPE_CYLINDER, or PRIM_TYPE_PRISM)"

updatePrimTypeSphereOld prim (VVal cx cy cz:FVal hollow:VVal ax ay az:rest) =
    return (prim { primTypeInfo = (primTypeInfo prim) { primTypeCode = icPrimTypeSphere, primCut = (cx,cy,cz), primHollow = hollow,
                                                        primAdvancedCut = (ax,ay,az) }}, rest)
updatePrimTypeSphereOld _ _ = fail "insufficient or incorrect parameters for deprecated prim type (PRIM_TYPE_SPHERE)"

updatePrimTypeTorusOld prim (VVal cx cy cz:FVal hollow:FVal twisty:FVal tapery:VVal sx sy sz:VVal ax ay az:rest) =
    let info = primTypeInfo prim in
    return (prim { primTypeInfo = info { primTypeCode = icPrimTypeTorus, primCut = (cx,cy,cz), primHollow = hollow,
                                         primTwist = let (x,y,z) = primTwist info in (x,twisty,z),
                                         primTaper = let (x,y,z) = primTaper info in (x,tapery,z),
                                         primTopshear = (sx,sy,sz), primAdvancedCut = (ax,ay,az) } }, rest)
updatePrimTypeTorusOld _ _ = fail "insufficient or incorrect parameters for deprecated prim type (PRIM_TYPE_TORUS)"

updatePrimTypeTubeOld prim (VVal cx cy cz:FVal hollow:FVal twisty:FVal shearx:rest) =
    let info = primTypeInfo prim in
    return (prim { primTypeInfo = info { primTypeCode = icPrimTypeTube, primCut = (cx,cy,cz), primHollow = hollow,
                                         primTwist = let (x,y,z) = primTwist info in (x,twisty,z),
                                         primTopshear = let (x,y,z) = primTopshear info in (shearx,y,z) } }, rest)
updatePrimTypeTubeOld _ _ = fail "insufficient or incorrect parameters for deprecated prim type (PRIM_TYPE_TUBE)"

llGetObjectDetails _ [KVal k, LVal params] =
     -- TODO: this doesn't take into account multiple regions
     do  result <- fromErrorT [] (getAvatarDetails k params `mplus` getPrimDetails k params)
         continueWith $ LVal result
     where getAvatarDetails k params = 
               do  avs <- lift getWorldAvatars
                   av <- M.lookup k avs
                   return $ map (avq av) params
               where avq av i | i == llcObjectName = SVal $ avatarName av
                              | i == llcObjectDesc = SVal ""
                              | i == llcObjectPos = vec2VVal $ avatarPosition av
                              | i == llcObjectRot = rot2RVal $ avatarRotation av
                              | i == llcObjectVelocity = VVal 0.0 0.0 0.0 -- TODO: avatar velocities
                              | i == llcObjectOwner = KVal k
                              | i == llcObjectGroup = KVal nullKey
                              | i == llcObjectCreator = KVal nullKey
                              | otherwise = llcObjectUnknownDetail
           getPrimDetails k params = 
               do  prim <- getPrim k
                   return $ map (primq prim) params
               where primq prim i | i == llcObjectName = SVal $ primName prim
                                  | i == llcObjectDesc = SVal $ primDescription prim
                                  | i == llcObjectPos = vec2VVal $ primPosition prim
                                  | i == llcObjectRot = rot2RVal $ primRotation prim
                                  | i == llcObjectVelocity = VVal 0.0 0.0 0.0 -- TODO: prim velocities
                                  | i == llcObjectOwner = KVal $ primOwner prim
                                  | i == llcObjectGroup = KVal nullKey            -- TODO: prim groups
                                  | i == llcObjectCreator = KVal $ primOwner prim -- TODO: prim creators
                                  | otherwise = llcObjectUnknownDetail

--------------------------------------------------------------------------------------------------------------
llAllowInventoryDrop info@(ScriptInfo _ _ _ k _) [IVal add] =
    (runErrPrim k () $ getPrim k >>= return . (\ p -> p { primAllowInventoryDrop = add /= 0 }) >>= lift . setPrim k >> 
                       lift (logFromScript info ("drop is now " ++ (if add == 0 then "not " else "") ++ "allowed"))) >>
        continueWith VoidVal
        
llAdjustSoundVolume info [FVal f] = do
    do if (f < 0.0 || f > 1.0) then logFromScript info ("llAdjustSoundVolume: volume " ++ show f ++ " out of range.")
                               else logFromScript info ("llAdjustSoundVolume: volume adjusted to " ++ show f)
       continueWith VoidVal
                                  
whenParcelPermitted info pk action= do
    (regionIndex,parcelIndex,parcel) <- getPrimParcel pk
    prim <- getPrim pk
    if parcelOwner parcel /= primOwner prim then lift $ logFromScript info "prim not permitted to change parcel"
        else action regionIndex parcelIndex parcel >>= putParcel regionIndex parcelIndex
        
addToLandACLList aclType aclFromParcel aclIntoParcel info@(ScriptInfo _ _ _ pk _) [KVal ak,FVal duration] = do
    runErrPrim pk () $ do
        whenParcelPermitted info pk $ \ regionIndex parcelIndex parcel -> do
            lift $ runAndLogIfErr ("attempt to change " ++ aclType ++ " unknown avatar " ++ ak) parcel $ do
                avs <- lift getWorldAvatars
                _ <- M.lookup ak avs
                when (duration < 0.0) $ lift $ logFromScript info (aclType ++ " change attempted with invalid duration: " ++ show duration)
                t <- lift getTick
                let acl = (ak, if duration == 0 then Nothing else Just $ t + durationToTicks duration)
                let acllist = acl : ([ b | b@(k,Just expire) <- aclFromParcel parcel,expire < t, k /= ak] ++
                                     [ b | b@(k,Nothing) <- aclFromParcel parcel, k /= ak])
                let parcel' = aclIntoParcel parcel acllist
                lift $ logFromScript info ("added " ++ ak ++ " to " ++ aclType ++ " list for parcel in " ++ (show regionIndex))
                return parcel'
    continueWith VoidVal
                                  
llAddToLandPassList info args = 
    let aclFromParcel = parcelPassList
        aclIntoParcel = (\ parcel list -> parcel { parcelPassList = list })
    in addToLandACLList "pass" aclFromParcel aclIntoParcel info args
llAddToLandBanList info args =
    let aclFromParcel = parcelBanList
        aclIntoParcel = (\ parcel list -> parcel { parcelBanList = list })
    in addToLandACLList "ban" aclFromParcel aclIntoParcel info args

llSound info@(ScriptInfo _ _ _ pk _) [SVal sound, FVal vol, IVal q, IVal loop] =
    runErrPrim pk (EvalIncomplete, VoidVal) $ do 
        lift $ logFromScript info ("call to deprecated function 'llSound'")
        sounds <- getPrimSoundInfo pk
        when (isNothing $ findByInvName sound sounds) $ lift $ logFromScript info ("sound " ++ sound ++ " does not exist")
        continueWith VoidVal

llGetKey (ScriptInfo _ _ _ pk _) [] = continueWith $ KVal pk

llGetOwnerKey info@(ScriptInfo _ _ _ pk _) [KVal k] = 
    runErrPrim pk k (do
        regionIndex <- getPrimRegion pk
        mRegionIndex  <- fromErrorT Nothing (getPrimRegion k >>= return . Just)
        case mRegionIndex of 
            Nothing -> (lift $ logFromScript info "llGetOwnerKey: object key not found") >> return k
            Just regionIndex' | regionIndex /= regionIndex' -> (lift $ logFromScript info "llGetOwnerKey: object in different simulator") >> return k
                              | otherwise -> getPrimOwner k
    ) >>= continueWith . KVal
    
llGetLinkNumber (ScriptInfo oid pid _ pk _) [] =
     if pid /= 0 then continueWith (IVal $ pid + 1) else
         runErrPrim oid 0 (do
             LSLObject links <- getObject oid
             return (if length links == 1 then 0 else 1)
         ) >>= continueWith . IVal
        
llGetUnixTime (ScriptInfo _ _ _ _ _) [] = 
    getUnixTime >>= continueWith . IVal

llGetTimestamp (ScriptInfo _ _ _ _ _) [] = do
    cal <- getTimeOfDay >>= return . toUTCTime
    continueWith $ SVal $ printf "%04d-%02d-%02dT%02d:%02d:%02.6f"
         (ctYear cal) (1 + (fromEnum $ ctMonth cal)) (ctDay cal) (ctHour cal) (ctMin cal) 
         (((fromIntegral $ ctSec cal) / (10.0^12 * (fromIntegral $ ctPicosec cal))) :: Float)
         
llGetDate (ScriptInfo _ _ _ _ _) [] = getUTCDate >>= continueWith . SVal
llGetGMTclock (ScriptInfo _ _ _ _ _) [] = do
    cal <- getTimeOfDay >>= return . toUTCTime
    continueWith $ IVal $ (24 * ctHour cal) + (60 * ctMin cal) + (ctSec cal)
    
llGetWallclock (ScriptInfo _ _ _ _ _) [] = do
    cal <- getLocalTimeOfDay (-8) >>= return . toUTCTime
    continueWith $ IVal $ (24 * ctHour cal) + (60 * ctMin cal) + (ctSec cal)
llGetTimeOfDay (ScriptInfo _ _ _ _ _) [] = do
    t <- getTick
    continueWith $ FVal $ (ticksToDuration (t `mod` durationToTicks 4.0))
getUnixTime :: (Monad (StateT (World a) a), Monad a) => StateT (World a) a Int
getUnixTime = (liftM2 (+) (getWorldZeroTime) (getTick >>= return . floor . ticksToDuration))
getTimeOfDay :: (Monad (StateT (World a) a), Monad a) => StateT (World a) a ClockTime
getTimeOfDay = getUnixTime >>= return . (flip TOD 0) . fromIntegral 

getLocalTimeOfDay offset = getTimeOfDay >>= return . (addToClockTime (TimeDiff 0 0 0 offset 0 0 0))
    
getUTCDate :: (Monad (StateT (World a) a), Monad a, PrintfType (Int -> Int -> Int -> b)) => StateT (World a) a b
getUTCDate = do
    cal <- getTimeOfDay >>= return . toUTCTime
    return $ printf "%04d-%02d-%02d" (ctYear cal) (1 + fromEnum (ctMonth cal)) (ctDay cal)    

llGetRegionFPS _ _ = continueWith $ FVal 45.0
llGetRegionTimeDilation _ _ = continueWith $ FVal 1.0

llGetTime (ScriptInfo _ _ sid pk _) [] = 
     runErrPrim pk 0 (do
         t <- lift getTick
         script <- lift getWorldScripts >>= M.lookup (pk,sid)
         return $ ticksToDuration (t - scriptLastResetTick script)
     ) >>= continueWith . FVal

getAndResetTick pk sid =
    do
        scripts <- lift getWorldScripts
        script <- M.lookup (pk,sid) scripts
        let t = scriptLastResetTick script
        t' <- lift getTick
        lift $ setWorldScripts (M.insert (pk,sid) (script { scriptLastResetTick = t' } ) scripts)
        return $ t' - t
    
llGetAndResetTime (ScriptInfo _ _ sid pk _) [] =
    runErrPrim pk 0 (getAndResetTick pk sid) >>= continueWith . FVal . ticksToDuration
    
llResetTime (ScriptInfo _ _ sid pk _) [] =
    runErrPrim pk 0 (getAndResetTick pk sid) >> continueWith VoidVal
    
llSetText info [SVal text, VVal r g b, FVal alpha] = setText "llSetText" 254 info text
llSetSitText info [SVal text] = setText "llSetSitText" 9 info text
llSetTouchText info [SVal text] = setText "llSetTouchText" 9 info text

setText func lim info text =
    do logFromScript info (func ++ ": setting text to " ++ text)
       when (length text > lim) $ logFromScript info (func ++ ": text exceeds " ++ show lim ++ " character limit")
       continueWith VoidVal

llGetScriptName (ScriptInfo _ _ sid _ _) [] = continueWith (SVal sid)

continueWith val = return (EvalIncomplete,val)

-- all the predefined functions for which implementations have been created
defaultPredefs :: Monad m => [PredefFunc m]
defaultPredefs = map (\(x,y) -> defaultPredef x y) 
    ([
        ("llAddToLandBanList",llAddToLandBanList),
        ("llAddToLandPassList",llAddToLandPassList),
        ("llAdjustSoundVolume",llAdjustSoundVolume),
        ("llAllowInventoryDrop",llAllowInventoryDrop),
        ("llAttachToAvatar",llAttachToAvatar),
        ("llAvatarOnSitTarget",llAvatarOnSitTarget),
        ("llBreakAllLinks",llBreakAllLinks),
        ("llBreakLink",llBreakLink),
        ("llCloud",llCloud),
        ("llCreateLink",llCreateLink),
        ("llDie",llDie),
        ("llEjectFromLand",llEjectFromLand),
        ("llFrand",llFrand),
        ("llGetAlpha",llGetAlpha),
        ("llGetAndResetTime", llGetAndResetTime),
        ("llGetAttached",llGetAttached),
        ("llGetBoundingBox",llGetBoundingBox),
        ("llGetColor",llGetColor),
        ("llGetCreator",llGetCreator),
        ("llGetDate",llGetDate),
        ("llGetGMTclock",llGetGMTclock),
        ("llGetInventoryCreator",llGetInventoryCreator),
        ("llGetInventoryKey",llGetInventoryKey),
        ("llGetInventoryName",llGetInventoryName),
        ("llGetInventoryNumber",llGetInventoryNumber),
        ("llGetInventoryType",llGetInventoryType),
        ("llGetKey",llGetKey),
        ("llGetLocalPos",llGetLocalPos),
        ("llGetNumberOfPrims",llGetNumberOfPrims),
        ("llGetNumberOfSides",llGetNumberOfSides),
        ("llGetLinkKey",llGetLinkKey),
        ("llGetLinkNumber", llGetLinkNumber),
        ("llGetLocalRot", llGetLocalRot),
        ("llGetObjectDesc", llGetObjectDesc),
        ("llGetObjectDetails", llGetObjectDetails),
        ("llGetObjectName", llGetObjectName),
        ("llGetObjectPermMask", llGetObjectPermMask),
        ("llGetObjectPrimCount", llGetObjectPrimCount),
        ("llGetOwner", llGetOwner),
        ("llGetOwnerKey",llGetOwnerKey),
        ("llGetPermissions",llGetPermissions),
        ("llGetPermissionsKey",llGetPermissionsKey),
        ("llGetPos", llGetPos),
        ("llGetPrimitiveParams", llGetPrimitiveParams),
        ("llGetRegionFPS", llGetRegionFPS),
        ("llGetRegionTimeDilation", llGetRegionTimeDilation),
        ("llGetRot",llGetRot),
        ("llGetRootPosition",llGetRootPosition),
        ("llGetRootRotation",llGetRootRotation),
        ("llGetScale", llGetScale),
        ("llGetScriptName", llGetScriptName),
        ("llGetTexture", llGetTexture),
        ("llGetTextureOffset", llGetTextureOffset),
        ("llGetTextureRot", llGetTextureRot),
        ("llGetTextureScale", llGetTextureScale),
        ("llGetTime",llGetTime),
        ("llGetTimeOfDay",llGetTimeOfDay),
        ("llGetTimestamp",llGetTimestamp),
        ("llGetUnixTime",llGetUnixTime),
        ("llGetWallclock",llGetWallclock),
        ("llKey2Name", llKey2Name),
        ("llListRandomize",llListRandomize),
        ("llListen", llListen),
        ("llListenControl",llListenControl),
        ("llListenRemove",llListenRemove),
        ("llMessageLinked", llMessageLinked),
        ("llOwnerSay", llOwnerSay),
        ("llRegionSay", llRegionSay),
        ("llRequestPermissions",llRequestPermissions),
        ("llResetTime",llResetTime),
        ("llSay",llSay),
        ("llSetAlpha", llSetAlpha),
        ("llSetColor", llSetColor),
        ("llSetLinkAlpha",llSetLinkAlpha),
        ("llSetLinkColor",llSetLinkColor),
        ("llSetLinkPrimitiveParams",llSetLinkPrimitiveParams),
        ("llSetLinkTexture",llSetLinkTexture),
        ("llSetLocalRot",llSetLocalRot),
        ("llSetObjectName",llSetObjectName),
        ("llSetObjectDesc",llSetObjectDesc),
        ("llSetPos",llSetPos),
        ("llSetPrimitiveParams",llSetPrimitiveParams),
        ("llSetRot",llSetRot),
        ("llSetScale",llSetScale),
        ("llSetSitText",llSetSitText),
        ("llSetText",llSetText),
        ("llSetTouchText",llSetTouchText),
        ("llSitTarget",llSitTarget),
        ("llShout",llShout),
        ("llSleep", llSleep),
        ("llSetTimerEvent",llSetTimerEvent),
        ("llSound",llSound),
        ("llUnSit",llUnSit),
        ("llWhisper",llWhisper),
        ("llWind",llWind),
        ("llWater",llWater)
    ] ++ internalLLFuncs)
    
allFuncs = (map (\ (name,_,_) -> name) funcSigs)
implementedFuncs = (map predefFuncName (defaultPredefs::[PredefFunc Maybe]))
unimplementedFuncs = S.toList (S.difference (S.fromList allFuncs) (S.fromList implementedFuncs))

---------------------------------------------------------------------------------------------------
logFromScript :: Monad m => ScriptInfo -> String -> WorldM m ()
logFromScript scriptInfo msg = logAMessage LogInfo (infoToLogSource scriptInfo) msg

infoToLogSource info = (scriptInfoPrimKey info ++ ": " ++ scriptInfoScriptName info)

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
    
addScript prim name = prim { primScripts = (name:(primScripts prim)) }
         

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

maybeM m = case m of Nothing -> fail "nothing"; (Just v) -> return v

pushEvents oid pid e = do
    fromErrorT () $ do
            objects <- lift getObjects
            o <- M.lookup oid objects
            key <- lookupByIndex pid (primKeys o)
            prims <- lift $ getPrims
            p <- M.lookup key prims
            lift $ mapM_ (pushEvent e key) ((inventoryItemNames . primScripts) p)
    
pushEvent e key sid =
    do scripts <- getWorldScripts
       case M.lookup (key,sid) scripts of
           Nothing -> logAMessage LogWarn "sim" ("no such script: " ++ (show (key, sid)))
           Just script -> setWorldScripts (M.insert (key,sid) (script { scriptEventQueue = (scriptEventQueue script) ++ [e] } ) scripts)

pushEventToPrim e key =
    do prims <- getPrims
       case M.lookup key prims of
           Nothing -> logAMessage LogWarn "sim" ("no such prim: " ++ key)
           Just p -> mapM_ (pushEvent e key) ((inventoryItemNames . primScripts) p)
           
pushEventToObject e key =
    do objects <- getObjects
       case M.lookup key objects of
           Nothing -> logAMessage LogWarn "sim" ("no such object: " ++ key)
           Just o ->  mapM_ (pushEventToPrim e) (primKeys o)

getObjectNames :: (Monad m) => WorldM m [String]
getObjectNames = getObjects >>= (return . M.keys)
getListenerIds :: (Monad m) => WorldM m [Int]
getListenerIds = liftM (map fst) getListeners
getObject name = lift getObjects >>= M.lookup name

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
               worldAvatars = M.empty,
               worldBreakpointManager = emptyBreakpointManager,
               worldSuspended = Nothing,
               worldRegions = M.empty,
               worldZeroTime = 0
           }
           
newWorld' slice maxt iq lib scripts avatars = 
    (newWorld slice maxt iq) { 
        wscripts = scripts, 
        wlibrary = lib,
        worldAvatars = avatars }
newWorld'' slice maxt iq lib scripts avatars objs prims activeScripts valueDB regions =
    (newWorld' slice maxt iq lib scripts avatars) {
        wobjects = objs,
        wprims = prims,
        worldScripts = activeScripts,
        worldDB = valueDB,
        worldRegions = regions }
        
addObjectToInventory name obj world = world { inventory = (name,obj):(inventory world) }

ticksPerSecond :: Int
ticksPerSecond = 1000


putWorldEvent tick we = 
    do weq <- getWQueue
       setWQueue $ putWQ tick we weq

checkBp bp sm =
    do  bpm <- getWorldBreakpointManager
        let (result,bpm',sm') = checkBreakpoint bp bpm sm
        setWorldBreakpointManager bpm'
        return (result,sm')
       
updateObject f name = 
    do objects <- getObjects
       case M.lookup name objects of
           Nothing -> logAMessage LogWarn "sim" ("object " ++ name ++ " not found")
           Just o -> do o' <- f o
                        setObjects (M.insert name o' objects)

updatePrim f key =
    do  prims <- getPrims
        case M.lookup key prims of
            Nothing -> logAMessage LogWarn "sim" ("prim " ++ key ++ " not found")
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
      setListeners ((id,(listener,True)):listeners)
      return id
unregisterListener pk sname id = 
    do listeners <- lift getListeners
       (listener,_) <- lookupM id listeners
       when (listenerScriptName listener /= sname && listenerPrimKey listener /= pk)
            (throwError ("listener " ++ show id ++ " not registered for calling script"))
       lift $ setListeners [(i,l) | (i,l) <- listeners, id /= i]
updateListener pk sname id active =
    do listeners <- lift getListeners
       (listener,_) <- lookupM id listeners
       when (listenerScriptName listener /= sname && listenerPrimKey listener /= pk)
            (throwError ("listener " ++ show id ++ " not registered for calling script"))
       lift $ setListeners ([(i,l) | (i,l) <- listeners, id /= i] ++ [(id,(listener,active))])
              
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
          t <- getTick
          case lookup script scripts of
              Nothing -> logAMessage LogWarn "sim" ("no such script: " ++ script)
              Just (Invalid s) -> do
                  updatePrim (\ p -> return $ addScript p (scriptInventoryItem name)) key
                  scripts <- getWorldScripts
                  setWorldScripts (M.insert (key,name) (Script (Invalid s) M.empty Nothing t t []) scripts)
              Just (Valid code) -> do
                  updatePrim (\ p -> return $ addScript p (scriptInventoryItem name)) key
                  let sstate = initLSLScript code
                  scripts <- getWorldScripts
                  setWorldScripts (M.insert (key,name) (Script (Valid sstate) M.empty Nothing t t [Event "state_entry" [] []]) scripts)
processEvent chat@(Chat chan name key msg location range) =
    do listeners <- getListeners 
       locatedListeners <- mapM locateListener (map snd listeners)
       let listeners'= [ l | (l,_,_) <- filter (matchListener chat) locatedListeners]
       -- event goes to all UNIQUE addresses in list
       let addresses = nub $ map listenAddress listeners'
       mapM (\ (key,sid) -> pushEvent (Event "listen" [IVal chan, SVal name, KVal key, SVal msg] []) key sid) addresses
       return ()
    where locateListener (listener,active) = do
              (VVal x y z) <- getPos (listenerPrimKey listener)
              region <- getRegionIndex (listenerPrimKey listener)
              return (listener,active,(region,(x,y,z)))
processEvent (WorldSimEvent name args) = 
    case M.lookup name eventDescriptors of
        Nothing -> logAMessage LogWarn "sim" ("event " ++ name ++ " not understood")
        Just def -> handleSimInputEvent def args
processEvent (PermissionRequestEvent pk sname ak mask) = 
    runAndLogIfErr ("can't find prim/script: " ++ pk ++ "/" ++ sname) () $ do
        scripts <- lift getWorldScripts
        script <- M.lookup (pk,sname) scripts
        let script' = script { scriptPermissions = M.insert ak mask (scriptPermissions script), scriptLastPerm = Just ak }
        lift $ setWorldScripts (M.insert (pk,sname) script' scripts)
        t <- lift getTick
        lift $ putWorldEvent t (mkRunTimePermissionsEvent pk sname mask)
processEvent evt@(TimerEvent interval (primKey,scriptName)) =
    do pushEvent (Event "timer" [] []) primKey scriptName
       t <- getTick
       putWorldEvent (t + durationToTicks interval) evt
               
    
processEvent _ = error "not implemented"

matchListener (Chat chan' sender' key' msg' (region,position) range) ((Listener pkey sid chan sender key msg),active,(region',position')) =
    active &&
    region == region' &&
    chan == chan' &&
    key' /= pkey &&
    (sender == "" || sender == sender') &&
    (key == nullKey || key == key') &&
    (msg == "" || msg == msg') &&
    (case range of
        Nothing -> True
        Just dist -> dist3d2 position position' <= dist^2)

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
checkAndRunScript _ (Script (Invalid _) _ _ _ _ _) = return ()
checkAndRunScript k@(pkey,sname) (Script (Valid img) _ _ _ _ q) =
    do  pInfo <- getPrimInfo pkey
        suspenseInfo <- getWorldSuspended
        case suspenseInfo of
            Nothing -> run pInfo
            Just (suspKey,suspName) | pkey == suspKey && suspName == sname -> run pInfo
                                    | otherwise -> return ()
   where run pInfo =
             case pInfo of
                 Nothing -> logAMessage LogWarn "sim" ("script " ++ (show k) ++ ": prim not found!")
                 Just (parent, index, primName) -> runScript parent index pkey sname primName img q
runScript parent index pkey sname primName img q =
    do slice <- getSliceSize
       tick <- getTick
       let img' = img { scriptImageName = sname ++ "/" ++ primName ++ "/" ++ pkey }
       let log = logTrace (pkey ++ ":" ++ sname)
       --let checkBp _ sm = return (False,sm)
       result <- executeLsl img' parent index sname pkey doPredef log getTick setTick checkBp q (tick + slice)
       case result of
           Left s -> logAMessage LogWarn "sim" ("execution error in script " ++ "(" ++ pkey ++ "," ++ sname ++ ") ->" ++ s)
           Right (img'',q') -> do
               scripts <- getWorldScripts
               let result = M.lookup (pkey,sname) scripts
               case result of
                   Nothing -> do logAMessage LogInfo "sim" ("script seems to have disappeared while executing (result of llDie?)")
                   Just script -> do
                       let script' = script { scriptImage = Valid img'', scriptEventQueue = q' }
                       setWorldScripts (M.insert (pkey,sname) script' scripts)
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
                | SimStepOut { simCmdBreakpoints :: [Breakpoint], simCmdEvents :: [SimEvent] } deriving (Show)

data SimStatus = SimEnded { simStatusMessage :: String, simStatusLog :: [LogMessage], simStatusState :: SimStateInfo } | 
                 SimInfo { simStatusEvents :: [SimEvent], simStatusLog :: [LogMessage], simStatusState :: SimStateInfo } |
                 SimSuspended { simStatusEvents :: [SimEvent], 
                                simStatusSuspendInfo :: ExecutionInfo,
                                simStatusLog :: [LogMessage],
                                simStatusState :: SimStateInfo } deriving (Show)

data SimStateInfo = SimStateInfo {
        simStateInfoTime :: Int,
        simStateInfoPrims :: [(String,String)],
        simStateInfoAvatars :: [(String,String)]
    } deriving (Show)
    
nullSimState = SimStateInfo 0 [] []

stateInfoFromWorld world =
    SimStateInfo {
        simStateInfoTime = tick world,
        simStateInfoPrims = map (\ p -> (primKey p, primName p)) (M.elems $ wprims world),
        simStateInfoAvatars = map (\ (_,a) -> (avatarKey a, avatarName a)) (M.toList $ worldAvatars world)
    }
    
data SimInputEventDefinition m = SimInputEventDefinition { 
    simInputEventName :: String,
    simInputEventDescription :: String,
    simInputEventParameters :: [SimParam],
    simInputEventHandler :: String -> [LSLValue] -> WorldM m () }

data SimParam = SimParam { simParamName :: String, simParamDescription :: String,
                           simParamType :: SimParamType }
     deriving (Show)
data SimParamType = SimParamPrim | SimParamAvatar | SimParamLSLValue LSLType | SimParamRootPrim | SimParamKey |
                    SimParamScript
     deriving (Show)
     
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
                 let Just script = M.lookup (k,s) (worldScripts world) 
                     Valid img = scriptImage script -- TODO: non-exhaustive but MUST succeed
                     stepMgr = stepManager img
                     img' = case command of
                                SimStep _ _ -> img { stepManager = setStepBreakpoint stepMgr }
                                SimStepOver _ _ -> img { stepManager = setStepOverBreakpoint stepMgr }
                                SimStepOut _ _ -> img { stepManager = setStepOutBreakpoint stepMgr }
                                _ -> img
                 in M.insert (k,s) (script { scriptImage = Valid img' }) (worldScripts world)
        simEventToWorldEvent (SimEvent name args delay) = (t + delay, WorldSimEvent name args)
        wq' = putManyWQ (map simEventToWorldEvent events) (wqueue world)
        world' = world { wqueue = wq', 
                         nextPause = t + slice,
                         worldBreakpointManager = newBreakpointManager,
                         worldScripts = newScripts }
        (_,world'') = runIdentity $ runStateT simulate world'
        log = msglog world''
        world''' = world'' { msglog = [] } in
        if (tick world''') < (maxTick world''') 
            then case worldSuspended world''' of
                Nothing ->
                     (SimInfo { simStatusEvents = [], 
                                simStatusLog = log,
                                simStatusState = stateInfoFromWorld world''' }, Right world''')
                Just (k,s) ->
                     let Just script = M.lookup (k,s) (worldScripts world''') -- TODO: non-exhaustive but MUST succeed
                         Valid img = scriptImage script
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
checkEventArg (SimParam name _ (SimParamLSLValue t)) (Just arg) = 
    case evaluateExpression t (simEventArgValue arg) of
        Nothing -> fail $ ("invalid " ++ (lslTypeString t) ++ " expression" )
        Just v -> return v
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
      Left s -> logAMessage LogWarn "sim" s
      Right argValues -> (simInputEventHandler def) (simInputEventName def) argValues

mkRunTimePermissionsEvent pk sn perm = 
    WorldSimEvent "run_time_permissions" [SimEventArg "Prim Key" pk, SimEventArg "Script" sn, SimEventArg "perm" (show perm)]
mkTouchStartEvent pk nd ak = WorldSimEvent "touch_start" [SimEventArg "Prim Key" pk, SimEventArg "num_detected" nd, SimEventArg "Avatar key" ak]
mkTouchEvent pk nd ak = WorldSimEvent "touch" [SimEventArg "Prim Key" pk, SimEventArg "num_detected" nd, SimEventArg "Avatar key" ak, SimEventArg "Grab vector" "<0.0,0.0,0.0>"]
mkTouchEndEvent pk nd ak = WorldSimEvent "touch_end" [SimEventArg "Prim Key" pk, SimEventArg "num_detected" nd, SimEventArg "Avatar key" ak]
mkChangedEvent oid change = WorldSimEvent "changed" [SimEventArg "Object Key" oid,SimEventArg "change" (show change)]

userTouchEventDef :: Monad m => SimInputEventDefinition m
userTouchEventDef =
    SimInputEventDefinition {
        simInputEventName = "Touch Prim",
        simInputEventDescription = "Avatar touches a prim for a duration",
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
                f name _ = logAMessage LogWarn "sim" ("invalid event activation: " ++ name)
            in f
    }
        
userChatEventDef :: Monad m => SimInputEventDefinition m
userChatEventDef =
    SimInputEventDefinition {
        simInputEventName = "Chat",
        simInputEventDescription = "Avatar chats",
        simInputEventParameters = [
            SimParam "Avatar" "The avatar that should do the chatting" SimParamAvatar,
            SimParam "Message" "What the avatar should say" (SimParamLSLValue LLString),
            SimParam "Channel" "The channel to chat on" (SimParamLSLValue LLInteger)],
        simInputEventHandler = 
            let f _ [KVal ak, SVal message, IVal chan] = do
                    t <- getTick
                    avatars <- getWorldAvatars
                    case M.lookup ak avatars of
                        Nothing -> logAMessage LogWarn "sim" ("avatar with key " ++ ak ++ " not found")
                        Just av -> do
                            putWorldEvent t $ Chat chan (avatarName av) ak message (avatarRegion av,avatarPosition av) (Just 20.0)
                f name _ = logAMessage LogWarn "sim" ("invalid event activation: " ++ name)
            in f
    }
        
eventDescriptors :: Monad m => Map [Char] (SimInputEventDefinition m)
eventDescriptors = M.fromList $ mkEventDefList ([userTouchEventDef,userChatEventDef] ++ rawLslEventDescriptors)

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
ticksToDuration ticks = fromInt ticks / 1000.0

logInitialProblems world =
    let log = msglog world
        newLog = [LogMessage 0 LogWarn "init" ("script \"" ++ name ++ "\" in prim " ++ prim ++ " failed to activate because of error: " ++ s) |
                          ((prim,name),Script (Invalid (_,s)) _ _ _ _ _) <- M.toList (worldScripts world) ] ++ log
    in world { msglog = newLog }