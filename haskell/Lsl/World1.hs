{-# OPTIONS_GHC -XFlexibleContexts -fwarn-unused-binds #-}
module Lsl.World1(SimStatus(..), 
                  SimStateInfo(..),
                  SimCommand(..),
                  SimInputEventDefinition(..),
                  SimParam(..),
                  SimParamType(..),
                  eventDescriptors,
                  simStep,
                  unimplementedFuncs,
                  module Lsl.WorldState) where

import Control.Monad(MonadPlus(..),(>=>),filterM,foldM,forM_,liftM,liftM2,unless,when)
import Control.Monad.State(StateT(..),lift)
import Control.Monad.Identity(Identity(..))
import Control.Monad.Error(MonadError(..),ErrorT(..))
import Data.List(elemIndex,find,foldl',isSuffixOf,nub,sortBy)
import Data.Bits((.&.),(.|.),bit,clearBit,complement,xor,setBit,shiftL,testBit)
import Data.Int()
import Data.Map(Map)
import Data.Maybe(fromMaybe,isJust,isNothing)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM

import Language.Lsl.Internal.Animation(builtInAnimations)
import Lsl.AvEvents(AvatarOutputEvent(..),AvatarInputEvent(..))
import Lsl.Breakpoint(Breakpoint,emptyBreakpointManager,setStepOverBreakpoint,setStepOutBreakpoint,replaceBreakpoints,
                      setStepBreakpoint,breakpointFile,breakpointLine,checkBreakpoint)
import Lsl.CodeHelper(renderCall)
import Lsl.Constants
import Lsl.Evaluation(ScriptInfo(..),Event(..),EvalResult(..))
import Lsl.EventSigs(EventAdditionalData(..),EventDelivery(..),lslEventDescriptors)
import Lsl.Exec(ExecutionState(..),ExecutionInfo(ExecutionInfo),ScriptImage(..),executeLsl,frameInfo,hardReset,hasActiveHandler,initLSLScript)
import Lsl.ExpressionHandler(evaluateExpression)
import Lsl.FuncSigs(funcSigs)
import Lsl.InternalLLFuncs(internalLLFuncs)
import Lsl.Key(nullKey)
import Lsl.Log(LogLevel(..),LogMessage(..))
import Lsl.Physics(calcAccel,checkIntersections,dampForce,dampTorque,dampZForce,gravC,kin,primMassApprox,rotDyn,totalTorque)
import Language.Lsl.Syntax(Ctx(..),FuncDec(..),predefFuncs)
import Lsl.Type(LSLValue(..),LSLType(..),defaultValue,isIVal,isLVal,isSVal,
                lslShowVal,lslTypeString,lslValString,rVal2Rot,rot2RVal,typeOfLSLValue,vVal2Vec,vec2VVal)
import Lsl.UnitTestWorld(simFunc,hasFunc)
import Lsl.Util(add3d,angleBetween,diff3d,dist3d2,fac,findM,fromInt,generatePermutation,ilookup,lookupByIndex,
                mag3d,mlookup,neg3d,norm3d,quaternionMultiply,quaternionToMatrix,rot3d,rotationBetween,scale3d,tuplify)
import Lsl.WorldDef(Attachment(..),Avatar(..),AvatarControlListener(..),Email(..),Flexibility(..),InventoryInfo(..),
                    InventoryItem(..),InventoryItemData(..),InventoryItemIdentification(..),LightInfo(..),LSLObject(..),
                    ObjectDynamics(..),Parcel(..),Prim(..),PrimFace(..),PrimType(..),PositionTarget(..),Region(..),RotationTarget(..),
                    Script(..),TextureInfo(..),WebHandling(..),
                    defaultCamera,defaultDynamics,emptyPrim,findByInvKey,findByInvName,
                    inventoryInfoPermValue,inventoryItemName,mkScript,
                    isInvAnimationItem,isInvBodyPartItem,isInvClothingItem,isInvGestureItem,isInvNotecardItem,
                    isInvObjectItem,isInvScriptItem,isInvSoundItem,isInvTextureItem,
                    primPhantomBit,primPhysicsBit,scriptInventoryItem,worldFromFullWorldDef)
import Lsl.WorldState

import System.Random(mkStdGen)
import System.Time(ClockTime(..),CalendarTime(..),TimeDiff(..),addToClockTime,toUTCTime)
import Text.Printf(PrintfType(..),printf)

-- execute a predefined ('ll') function
doPredef :: Monad m => String -> ScriptInfo -> [LSLValue] -> WorldM m (EvalResult,LSLValue)
doPredef name info@(ScriptInfo oid pid sid pkey event) args =
    do predefs <- getPredefFuncs
       -- find the correct function to execute
       case tryPredefs name predefs of
           Just (t,f) -> runErrFunc info name (defaultValue t) args f
           Nothing -> do
               (_,rettype,argtypes) <- findM (\ (x,y,z) -> x == name) funcSigs
               logAMessage LogDebug (pkey ++ ":" ++ sid) ("unimplemented predefined function called: " ++ renderCall name args)
               continueWith $ defaultValue rettype

    
getParcelByPosition regionIndex (x,y,_) = 
    do  region <- getRegion regionIndex
        findParcel 0 (regionParcels region)
    where findParcel _ [] = throwError "parcel not found" -- mzero
          findParcel i (p:ps) =
              let (south,north,west,east) = parcelBoundaries p
                  (xc,yc) = (floor x, floor y) in
                  if xc < east && xc >= west && yc < north && yc >= south
                      then return (i,p) else findParcel (i + 1) ps

getPrimParcel pk = do
    regionIndex <- getPrimRegion pk
    pos <- getRootPrim pk >>= getObject >>= return . objectPosition . objectDynamics
    (index,parcel) <- getParcelByPosition regionIndex pos
    return (regionIndex,index,parcel)
    
putParcel regionIndex index parcel = do
    region <- getRegion regionIndex
    let (before,after) = splitAt index (regionParcels region)
    let parcels' = if null after then parcel : before else before ++ (parcel : tail after)
    lift $ setRegion regionIndex $ region { regionParcels = parcels' }
                
tryPredefs name predefs =
    case M.lookup name predefs of
        Just (PredefFunc _ t predef) -> Just (t,predef)
        Nothing -> Nothing
        
defaultPredef name predef =
    case lookup name $ map (\ (FuncDec n t ps) -> (ctxItem n,t)) predefFuncs of
        Nothing -> error ("undefined predef " ++ name)
        Just t -> PredefFunc name t predef

runErrFunc info fname defval args f =
    do v <- evalErrorT (f info args)
       case v of
           Left s -> logFromScript info (fname ++ ": " ++ s) >> return (EvalIncomplete,defval)
           Right result -> return result
           
runFace k i action = action <||> throwError ("face " ++ (show i) ++ " or prim " ++ k ++ " not found")

(<||>) a b = a `catchError` (\ _ -> b)

infixl 0 <||>

primErr k = (\ s -> throwError ("problem - can't find prim " ++ k))

--- Web related functions ----------------------------------------------------

llHTTPRequest info@(ScriptInfo _ _ sn pk _) [SVal url, LVal params, SVal body] = 
    do  t <- lift getTick
        -- verify the parameters
        when (length params `mod` 2 /= 0) (throwError "invalid parameter list")
        when (length params > 8) (throwError "invalid parameter list")
        let params' = tuplify params
        when (not $ all (\ (x,_) -> x `elem` [llcHTTPMethod,llcHTTPMimetype,llcHTTPBodyMaxlength,llcHTTPVerifyCert]) params')
            (throwError "invalid parameter list")
        method <- maybe (return "GET") 
                      (\ v -> if not $ v `elem` (map SVal ["GET","POST","PUT","DELETE"]) 
                          then throwError ("invalid HTTP method " ++ lslValString v)
                          else let SVal s = v in return s) 
                      (lookup llcHTTPMethod params')
        mimetype <- maybe (return "text/plain;charset=utf-8")
              (\ v -> if LLString /= typeOfLSLValue v then throwError "invalid mimtype" else let SVal s = v in return s)
              (lookup llcHTTPMimetype params')
        maxlength <- maybe (return 2048)
              (\ v -> if LLInteger /= typeOfLSLValue v then throwError "invalid body length" else return (let IVal i = v in i))
              (lookup llcHTTPBodyMaxlength params')
        verify <- maybe (return 1)
              (\ v -> if LLInteger /= typeOfLSLValue v then throwError "invalid verify cert value" else return (let IVal i = v in i))
              (lookup llcHTTPVerifyCert params')
        -- parameters ok
        key <- lift newKey
        lift $ putWorldEvent 0 (HTTPRequestEvent (pk,sn) key url method mimetype maxlength verify body)
        continueWith $ KVal key

--- Sensor related functions -------------------------------------------------

llSensor info@(ScriptInfo _ _ sn pk _) [SVal name, KVal key, IVal etype, FVal range, FVal arc] =
    lift $ putWorldEvent 0 (SensorEvent (pk,sn) name key etype range arc Nothing) >> continueWith VoidVal
llSensorRepeat info@(ScriptInfo _ _ sn pk _) [SVal name, KVal key, IVal etype, FVal range, FVal arc, FVal interval] =
    let interval' = if interval < 0 then 0 else interval in
        lift $ putWorldEvent 0 (SensorEvent (pk,sn) name key etype range arc (Just interval')) >> continueWith VoidVal
llSensorRemove info@(ScriptInfo _ _ sn pk _) [] =
    do q <- lift getWQueue
       lift $ setWQueue [ qentry | qentry@(_,event) <- q, not (isSensorEvent event) || sensorAddress event /= (pk,sn)]
       continueWith VoidVal
------------------------------------------------------------------------------

llRequestAgentData info@(ScriptInfo _ _ sn pk _) [KVal ak, IVal d] =
    do key <- lift newKey
       av <- (lift getWorldAvatars >>= mlookup ak) <||> (throwError ("no such avatar" ++ ak))
       dataVal <- case d of
                         d | d == cDataBorn -> return "1981-01-01"
                           | d == cDataOnline -> return "1"
                           | d == cDataRating -> return "0,0,0,0,0"
                           | d == cDataName -> return $ avatarName av
                           | d == cDataPayinfo -> return "3"
                           | otherwise -> throwError ("invalid data requested: " ++ show d)
       lift $ pushDataserverEvent pk sn key dataVal
       continueWith (KVal key)

llRequestSimulatorData info@(ScriptInfo _ _ sn pk _) [SVal simName, IVal d] =
    do key <- lift newKey
       regions <- lift getWorldRegions
       let findRegion = findM ((==simName) . regionName . snd) (M.toList regions)
       let simErr = throwError ("unknown simulator " ++ simName)
       val <- case d of
                  d | d == cDataSimStatus -> (findRegion >> return "up") <||> return "unknown"
                    | d == cDataSimRating -> (findRegion >> return "PG") <||> simErr
                    | d == cDataSimPos -> (do ((x,y),_) <- findRegion
                                              return $ lslValString $ vec2VVal (256 * fromIntegral x, 256 * fromIntegral y, 0))
                                          <||> simErr
                    | otherwise -> throwError ("invalid data requested: " ++ show d)
       lift $ pushDataserverEvent pk sn key val
       continueWith (KVal key)
--- INVENTORY related functions ----------------------------------------------

sortByInvName l = sortBy (\ i i' -> compare (invName i) (invName i')) l

invName = inventoryItemName

getPrimInventoryInfo info@(ScriptInfo _ _ _ pk _) invType =
    case invType of
        i | i == llcInventoryAll -> getPrimInventory pk >>= return . sortByInvName
          | i == llcInventoryAnimation -> getPrimAnimations pk >>= return . sortByInvName
          | i == llcInventoryBodyPart -> getPrimBodyParts pk >>= return . sortByInvName
          | i == llcInventoryClothing -> getPrimClothing pk >>= return . sortByInvName
          | i == llcInventoryGesture -> getPrimGestures pk >>= return . sortByInvName
          | i == llcInventoryNotecard -> getPrimNotecards pk >>= return . sortByInvName
          | i == llcInventoryObject -> getPrimObjects pk >>= return . sortByInvName
          | i == llcInventoryScript -> getPrimScripts pk >>= return . sortByInvName
          | i == llcInventorySound -> getPrimSounds pk >>= return . sortByInvName
          | i == llcInventoryTexture -> getPrimTextures pk >>= return . sortByInvName
          | otherwise -> do lift $ logFromScript info ("invalid inventory type: " ++ lslValString invType)
                            return []

llGetInventoryNumber info@(ScriptInfo _ _ _ pk _) [invType@(IVal _)] = do
    getPrimInventoryInfo info invType >>= continueWith . IVal . length

llGetInventoryCreator info@(ScriptInfo _ _ _ pk _) [SVal name] =
    do  all <- getPrimInventory pk
        result <- case findByInvName name all of
            Nothing -> (lift $ putChat (Just 20.0) pk 0 ("no item named '" ++ name ++ "'")) >> return nullKey
            Just item -> return $ inventoryInfoCreator $ inventoryItemInfo  item
        continueWith $ KVal result 

llGetInventoryPermMask info@(ScriptInfo _ _ _ pk _) [SVal name, IVal mask] =
    do  all <- getPrimInventory pk
        mask <- case findByInvName name all of
            Nothing -> (lift $ putChat (Just 20.0) pk 0 ("No item named '" ++ name ++ "'")) >> throwError ("No item named '" ++ name ++ "'")
            Just item -> inventoryInfoPermValue mask (inventoryInfoPerms $ inventoryItemInfo item)
        continueWith (IVal mask)
    
llGetInventoryKey info@(ScriptInfo _ _ _ pk _) [SVal name] =
    do  all <- getPrimInventory pk
        result <- case findByInvName name all of
            Nothing -> return nullKey
            Just (InventoryItem id invInfo _) -> 
                let Right perms =  inventoryInfoPermValue cMaskOwner (inventoryInfoPerms invInfo) in
                    if perms .&. cFullPerm == cFullPerm then
                        return $ snd (inventoryItemNameKey id)
                    else return nullKey
        continueWith $ KVal result
       
llGetInventoryName info@(ScriptInfo _ _ _ pk _) [invType@(IVal _), IVal index] =
    getPrimInventoryInfo info invType >>= return . map (\ item -> invName item) >>= continueWith . SVal . fromMaybe "" . lookupByIndex index
    
llGetInventoryType info@(ScriptInfo _ _ _ pk _) [SVal name] = do
        getPrimInventory pk >>= continueWith . classify . findByInvName name
    where classifications = [(isInvAnimationItem,llcInventoryAnimation),(isInvBodyPartItem,llcInventoryBodyPart),
                             (isInvClothingItem,llcInventoryClothing),(isInvGestureItem,llcInventoryGesture),
                             (isInvNotecardItem,llcInventoryNotecard),(isInvObjectItem,llcInventoryObject),
                             (isInvSoundItem,llcInventorySound),(isInvTextureItem,llcInventoryTexture),
                             (isInvScriptItem,llcInventoryScript)]
          classify Nothing = IVal (-1)
          classify (Just item) = foldl' (\ (IVal cur) (f,i) -> if (cur >= 0) || not (f item) then IVal cur else i) (IVal (-1)) classifications

llRemoveInventory info@(ScriptInfo _ _ _ pk _) [SVal name] =
    do  inv <- getPrimInventory pk
        maybe (lift $ putChat (Just 20) pk cDebugChannel ("Missing inventory item '" ++ name ++ "'"))
              (\  _ -> let inv' = [ item | item <- inv, name /= (inventoryItemName item)] in
                  lift $ setPrimInventory pk inv')
              (findByInvName name inv)
        continueWith VoidVal

copyInventoryItem newOwner newGroup item =
    case item of
        item | isInvObjectItem item -> do
                  let copyLink prim = do
                         k <- lift newKey
                         inventory <- mapM (copyInventoryItem newOwner newGroup) (primInventory prim)
                         return (prim { primKey = k, primInventory = inventory, primOwner = newOwner, primGroup = newGroup })
                  InvObject links <- return $ inventoryItemData item
                  links' <- mapM copyLink links
                  when (null links') $ throwError "empty linkset!"
                  let info = inventoryItemInfo item
                  return (InventoryItem (InventoryItemIdentification (inventoryItemName item,primKey $ head links'))
                                         info (InvObject links'))
             | otherwise -> do
                  k <- lift newKey
                  let info = inventoryItemInfo item
                  let dat = inventoryItemData item
                  return (InventoryItem (InventoryItemIdentification (inventoryItemName item, k)) info dat)
changeName (InventoryItem (InventoryItemIdentification (name,key)) info dat) newName =
    (InventoryItem (InventoryItemIdentification (newName,key)) info dat)
                                 
llGiveInventory info@(ScriptInfo _ _ _ pk _) [KVal k, SVal inv] =
    do  inventory <- getPrimInventory pk <||> throwError "cant find prim!"
        item <- maybe (throwError ("inventory item " ++ inv ++ "not found")) (return . id) (findByInvName inv inventory)
        prims <- lift getPrims
        case M.lookup k prims of
            Just p -> do
               owner <- getPrimOwner pk
               group <- getPrimGroup pk
               let (_,ownerP,groupP,everybodyP,_) = let [p1,p2,p3,p4,p5] = take 5 (primPermissions p) ++ repeat 0 in (p1,p2,p3,p4,p5)
               let hasModifyPerm = (cPermModify .&. everybodyP /= 0) ||
                                   (cPermModify .&. groupP /= 0 && not (isNothing group) && group == primGroup p) ||
                                   (cPermModify .&. ownerP /= 0 && owner == primOwner p)
               if hasModifyPerm || primAllowInventoryDrop p
                   then do
                       let newOwner = primOwner p
                       let newGroup = primGroup p
                       item' <- copyInventoryItem newOwner newGroup item
                       let inventory' = primInventory p
                       let findAName i base = let name = base ++  " " ++ show i in
                               if isNothing (findByInvName name inventory') then name 
                                                                            else findAName (i + 1) base
                       let inventory'' = case findByInvName inv inventory' of
                               Nothing -> item' : inventory'
                               Just _ -> let newName = findAName 1 inv in (changeName item' newName) : inventory'
                       lift $ setPrimInventory k inventory''
                       -- TODO: are scripts handled right?
                       lift $ pushDeferredScriptEventToPrim
                                  (Event "changed" [if hasModifyPerm then llcChangedInventory else llcChangedAllowedDrop] M.empty) k 0
                   else throwError ("can't modify/drop inventory on target")
            Nothing -> do
                avatars <- lift getWorldAvatars
                case M.lookup k avatars of
                    Nothing -> throwError ("no object or avatar found with key " ++ k)
                    Just av -> lift (putWorldEvent 0 (GiveAvatarInventoryEvent k item)) >>
                         (lift $ logFromScript info $  "llGiveInventory: avatar " ++ k ++ " given option of accepting inventory")
        continueWith VoidVal
        
llSetRemoteScriptAccessPin info@(ScriptInfo _ _ _ pk _) [IVal pin] =
    lift $ setPrimRemoteScriptAccessPin pk pin >> continueWith VoidVal
llRemoteLoadScript info@(ScriptInfo _ _ _ pk _) [KVal _,SVal _, IVal _, IVal _] =
    lift $ putChat (Just 20.0) pk cDebugChannel "Deprecated.  Please use llRemoteLoadScriptPin instead." >> continueWith VoidVal
       
llRezObject info params = rezObject False info params
llRezAtRoot info params = rezObject True info params

rezObject atRoot info@(ScriptInfo _ _ _ pk _) [SVal inventory, VVal px py pz, VVal vx vy vz, RVal rx ry rz rs, IVal param] =
    do  objects <- getPrimObjects pk
        maybe (do lift $ putChat (Just 20) pk 0 ("Couldn't find object '" ++ inventory ++ "'")
                  throwError ("no such inventory item - " ++ inventory))
              (\ item -> do
                  let copy = isCopyable item
                  let InvObject linkSet = inventoryItemData item
                  lift $ putWorldEvent 0 (RezObjectEvent linkSet (px,py,pz) (vz,vy,vz) (rz,ry,rz,rs) param pk copy atRoot)
                  when (not copy) $ do -- delete!
                     wholeInventory <- getPrimInventory pk
                     lift $ setPrimInventory pk
                         [ item | item <- wholeInventory, inventory /= inventoryItemName item]
              )
              (findByInvName inventory objects)
        continueWith VoidVal
    
isCopyable item = 
    let (_,perm,_,_,_) = inventoryInfoPerms $ inventoryItemInfo item in
    if cPermCopy .&. perm == 0
        then False
        else 
            (if isInvObjectItem item
                then let InvObject links = inventoryItemData item 
                         primCopyable prim = all isCopyable (primInventory prim)
                     in all primCopyable links
                else True)
                
llResetScript info@(ScriptInfo _ _ sn pk _) [] =
    do lift $ putWorldEvent 0 (ResetScriptEvent pk sn)
       return (EvalComplete Nothing,VoidVal)
       
llResetOtherScript info@(ScriptInfo _ _ sn pk _) [SVal sn'] =
    do  when (sn == sn') $ throwError ("trying to reset the current script - must use llResetScript for this")
        lift getWorldScripts >>= mlookup (pk,sn) >>= return . scriptActive >>= flip when (throwError "can't reset a stopped script") . not
        resetScript pk sn'
        continueWith VoidVal
    
resetScript pk sn = do
         lift $ logAMessage LogDebug "sim" "resetting a script"
         scripts <- lift getWorldScripts
         script <- mlookup (pk,sn) scripts
         t <- lift getTick
         let img = scriptImage script
         let doReset execState =
                 let script' = script {
                            scriptImage = (hardReset img) {executionState = execState},
                            scriptActive = True,
                            scriptPermissions = M.empty,
                            scriptLastPerm = Nothing,
                            scriptLastResetTick = t,
                            scriptEventQueue = [Event "state_entry" [] M.empty]
                            } in lift $ setWorldScripts (M.insert (pk,sn) script' scripts)
                 in case executionState img of
                     Executing -> doReset Waiting
                     Waiting -> doReset Waiting
                     SleepingTil i -> doReset (WaitingTil i)
                     WaitingTil i -> doReset (WaitingTil i)
                     _ -> throwError ("can't reset script that is crashed or suspended")   
                     
llGetScriptState info@(ScriptInfo _ _ _ pk _) [SVal sn] =
    do  script <- (lift getWorldScripts >>= mlookup (pk,sn)) <||> throwError "no such script/invalid script"
        (continueWith . IVal) $ if not $ scriptActive script then 0
                else case executionState (scriptImage script) of
                            Halted -> 0
                            Erroneous _ -> 0
                            Crashed _ -> 0
                            _ -> 1 
    
llSetScriptState info@(ScriptInfo _ _ sn pk _) [SVal sn', IVal state] =
    do  when (sn == sn') $ throwError ("can't change your own state")
        scripts <- lift getWorldScripts
        script <- mlookup (pk,sn') scripts <||> throwError "no such script (could be invalid script)"
        if state == 0 
            then
                if (scriptActive script) 
                    then lift $ setWorldScripts (M.insert (pk,sn') (script { scriptActive = False }) scripts)
                    else return ()
            else case executionState (scriptImage script) of
                    Crashed _ -> throwError "can't change state of crashed script"
                    Erroneous _ -> throwError "can't change state of erroneous script"
                    _ -> if scriptActive script
                             then throwError "script is already running"
                             else resetScript pk sn'
        continueWith VoidVal
------------------------------------------------------------------------------
llSetPayPrice info@(ScriptInfo _ _ sn pk _) [IVal price, LVal l] =
    let toI (IVal i) = if (i < (-2)) then (-1) else i
        toI _ = (-1)
        [i1,i2,i3,i4] = take 4 $ (map toI l) ++ replicate 4 (-1) in
    do  hasMoney <- scriptHasActiveHandler pk sn "money"
        when (not hasMoney) $ throwError ("no money handler, pay price info ignored")
        lift $ setPrimPayInfo pk (price,i1,i2,i3,i4)
        continueWith VoidVal
    
llGetStartParameter info@(ScriptInfo _ _ sn pk _) [] =
    (lift getWorldScripts >>= mlookup (pk,sn) >>= continueWith . IVal . scriptStartParameter)
------------------------------------------------------------------------------
llCloud info [v@(VVal x y z)] =
    do lift $ logFromScript info "llCloud: returning default density"
       continueWith $ FVal 0

llWind info [v@(VVal x y z)] =
    lift $ logFromScript info "llWind: returning default density" >> continueWith (VVal 0 0 0)

llWater info [v@(VVal x y z)] =
    do lift $ logFromScript info "llWater: returning default water elevation"
       continueWith $ FVal 0

llSitTarget (ScriptInfo _ _ _ pk _) [v@(VVal _ _ _),r@(RVal _ _ _ _)] =
    lift $ setPrimSitTarget pk (Just (vVal2Vec v, rVal2Rot r)) >> continueWith VoidVal
    
llAvatarOnSitTarget info@(ScriptInfo _ _ _ pk _) [] =
    do  sitTarget <- getPrimSitTarget pk
        when (sitTarget == Nothing) (throwError ("no sit target"))
        k <- getPrimSittingAvatar pk >>= return . fromMaybe nullKey
        continueWith (KVal k)

llUnSit info@(ScriptInfo _ _ _ pk _) [KVal k] = 
    do  val <- getPrimSittingAvatar pk
        maybe (throwError "no avatar sitting on prim (land unsit not implemented")
              (\ ak -> if ak == k then lift $ setPrimSittingAvatar pk Nothing
                                  else throwError "unsit of avatar not sitting on prim attempted (land unsit not implemented)")
              val
        continueWith VoidVal

llMessageLinked info@(ScriptInfo oid pid sid pkey _) [IVal link,IVal val,SVal msg,KVal key] =
    do  LSLObject { primKeys = links } <- (getObject oid) <||> throwError ("object not found!")
        when (null links) $ throwError ("object is has no links!")
        let targetLinkIndices = targetLinks (length links) link pid
        let targetLinks = map (links !!) targetLinkIndices
        let event = (Event "link_message" [IVal pid, IVal val, SVal msg, KVal key] M.empty)
        lift $ mapM_ (\ pk -> pushDeferredScriptEventToPrim event pk 0) targetLinks
        continueWith VoidVal

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
    do tick <- lift $ getTick
       return $ (YieldTil (tick + durationToTicks f),VoidVal)
       

llInstantMessage info@(ScriptInfo _ _ _ pk _) [KVal ak, SVal message] =
    do tick <- lift getTick
       name <- getPrimName pk
       av <- (lift getWorldAvatars >>= mlookup ak) <||> throwError ("no such user: " ++ ak)
       let avname = avatarName av
       lift $ logAMessage LogInfo "sim" ("IM to " ++ avname ++ " from " ++ name ++ ": " ++ message)
       return (YieldTil (tick + durationToTicks 2),VoidVal)
       
llSay info params= lift $ chat (Just 20.0) info params
llWhisper info params = lift $ chat (Just 10.0) info params
llShout info params = lift $ chat (Just 100.0) info params
llRegionSay info params@[IVal chan, SVal message] =
    if (chan == 0) 
       then do lift $ logAMessage LogWarn (scriptInfoPrimKey info ++ ": " ++ scriptInfoScriptName info) "attempt to llRegionSay on channel 0"
               return (EvalIncomplete,VoidVal)
       else lift $ chat Nothing info params
       
chat range info@(ScriptInfo oid pid sid pkey event) [IVal chan, SVal message] =
    do logFromScript info $ concat ["chan = ", show chan, ", message = ", message]
       putChat range pkey chan message
       return (EvalIncomplete,VoidVal)
       
putChat range k chan message = do
    prims <- getPrims
    case M.lookup k prims of
        Just prim -> runErrPrim k () $ do
            region <- (getPrimRegion k)
            pos <- getRootPrim k >>= getObject >>= return . objectPosition . objectDynamics
            lift $ putWorldEvent 0 $ Chat chan (primName prim) k message (region, pos) (range)
        Nothing -> do
            avatars <- getWorldAvatars
            case M.lookup k avatars of
                Just avatar -> putWorldEvent 0 $ Chat chan (avatarName avatar) k message (avatarRegion avatar, avatarPosition avatar) (range)
                Nothing -> logAMessage LogWarn "sim" ("Trying to chat from unknown object/avatar with key " ++ k)
    
llListen (ScriptInfo oid pid sid pkey event) [IVal chan, SVal sender, KVal key, SVal msg] = 
    (lift $ registerListener $ Listener pkey sid chan sender key msg) >>=  continueWith . IVal
    
llListenRemove (ScriptInfo _ _ sid pk _) [IVal id] =
    (unregisterListener pk sid id) >> continueWith VoidVal 

llListenControl (ScriptInfo _ _ sid pk _) [IVal id, IVal active] =
    (updateListener pk sid id $ active /= 0) >> continueWith VoidVal

llFrand _ [FVal maxval] =
    lift $ do r <- wrand
              continueWith $ FVal (maxval * r)

llTeleportAgentHome info@(ScriptInfo _ _ _ pk _) [KVal ak] =
    do  owner <- getPrimOwner pk
        regionIndex <- getPrimRegion pk
        (_,_,parcel) <- getPrimParcel pk
        if parcelOwner parcel == owner
            then lift $ logFromScript info ("llTeleportAgentHome: user " ++ ak ++ " teleported home (in theory)")
            else lift $ logFromScript info ("llTeleportAgentHome: not permitted to teleport agents from this parcel")
        continueWith VoidVal
        
llEjectFromLand info@(ScriptInfo oid _ _ pk _) [KVal user] =
    do  owner <- getPrimOwner pk
        regionIndex <- getPrimRegion pk
        (_,parcel) <- getObjectPosition oid >>= getParcelByPosition regionIndex
        if parcelOwner parcel == owner 
            then lift $ logFromScript info ("llEjectFromLand: user " ++ user ++ " ejected (in theory)")
            else lift $ logFromScript info ("llEjectFromLand: not permitted to eject from this parcel")
        continueWith VoidVal
    
llBreakLink info@(ScriptInfo oid _ sid pk _) [IVal link] = 
    do  perm <- getPermissions pk sid
        if perm .&. cPermissionChangeLinks /= 0
            then do
                objects <- lift getObjects
                LSLObject { primKeys = links, objectDynamics = dynamics } <- mlookup oid objects
                attachment <- getPrimAttachment oid
                case attachment of
                    Nothing ->
                        if link < 1 || link > length links then lift $ logFromScript info ("llBreakLink: invalid link id")
                            else do
                                let (dyn1,dyn2) = if link == 1 then (defaultDynamics,dynamics) else (dynamics,defaultDynamics)
                                let (xs,y:ys) = splitAt (link - 1) links
                                let (linkSet1,linkSet2) = (xs ++ ys, [y])
                                let objects' = if null linkSet1 then objects else M.insert (head linkSet1) (LSLObject linkSet1 dyn1) objects
                                lift $ setObjects (M.insert (head linkSet2) (LSLObject linkSet2 dyn2) objects')
                                when (not (null linkSet1)) $ do
                                    lift $ pushChangedEventToObject (head linkSet1) cChangedLink
                                    lift $ setPrimParent (head linkSet1) Nothing
                                lift $ pushChangedEventToObject (head linkSet2) cChangedLink
                                lift $ setPrimParent (head linkSet2) Nothing
                    _ -> lift $ logFromScript info ("llBreakLink: can't change links of attached object")
            else lift $ logFromScript info ("llBreakLink: no permission") 
        continueWith VoidVal

llBreakAllLinks info@(ScriptInfo oid _ sid pk _) [] =
    do  perm <- getPermissions pk sid
        if perm .&. cPermissionChangeLinks /= 0
            then do
                objects <- lift getObjects
                LSLObject links dynamics <- mlookup oid objects
                attachment <- getPrimAttachment oid
                case attachment of
                    Nothing ->
                        if length links > 1 
                            then do
                                -- order of parameters to union is vital: union is left biased, so we want the 
                                -- new objects (in the left argument) to replace the old (in the right) where
                                -- both exist (the root key of the old object is found in both)
                                let objects' = M.union 
                                        (M.fromList $ map (\ k -> (k, LSLObject [k] (if k == oid then dynamics else defaultDynamics))) links) objects
                                lift $ setObjects objects'
                                lift $ mapM_ (\ link -> pushChangedEventToObject link cChangedLink >>
                                                        setPrimParent link Nothing) links
                            else return ()
                    _ -> lift $ logFromScript info ("llBreakAllLinks: can't change links of attached object")
            else lift $ logFromScript info ("llBreakAllLinks: no permission")
        continueWith VoidVal
        
-- TODO: verify link order
llCreateLink info@(ScriptInfo oid _ sid pk _) [KVal target, IVal iparent] =
    do  perm <- getPermissions pk sid
        if perm .&. cPermissionChangeLinks /= 0
            then do
                objects <- lift getObjects
                LSLObject (link:links) dynamics <- mlookup oid objects
                case M.lookup target objects of
                    Nothing -> lift $ logFromScript info "llCreateLink: target not found"
                    Just (LSLObject (link':links') dynamics') -> do
                        attachment0 <- getPrimAttachment oid
                        attachment1 <- getPrimAttachment target
                        case (attachment0,attachment1) of
                            (Nothing,Nothing) -> do
                                mask <- getObjectPermMask target cMaskOwner
                                ownerTarget <- getPrimOwner target
                                owner <- getPrimOwner oid
                                if mask .&. cPermModify /= 0 && owner == ownerTarget 
                                   then do
                                        let (newLinkset,deleteKey,newDynamics) = if parent 
                                                then ((link:link':links') ++ links, link',dynamics)
                                                else ((link':link:links) ++ links', link,dynamics')
                                        lift $ setObjects (M.insert (head newLinkset) (LSLObject newLinkset newDynamics) $ M.delete deleteKey objects)
                                        lift $ pushChangedEventToObject (head newLinkset) cChangedLink
                                    else lift $ logFromScript info "llCreateLink: no modify permission on target"
                            _ -> lift $ logFromScript info ("llCreateLink: can't change links of attached object")
            else lift $ logFromScript info ("llCreateLink: no permission to change links")
        continueWith VoidVal
    where parent = iparent /= 0

llDie info@(ScriptInfo oid _ _ pk _) [] =
     do  attachment <- getPrimAttachment oid
         case attachment of
             Nothing -> do
                     objects <- lift getObjects
                     LSLObject { primKeys = (links) } <- mlookup oid objects
                     allScripts <- mapM getPrimScripts links
                     let primsWithScriptNames = zip links (map (map (invName)) allScripts)
                     let skeys = concat $ map ( \ (k,list) -> map ((,)k) list) primsWithScriptNames
                     lift $ mapM_ delScript skeys
                     lift $ mapM_ (\ link -> (getPrims >>= return . M.delete link >>= setPrims)) links
                     lift $ setObjects (M.delete oid objects)
                     lift $ logFromScript info ("object, and therefore this script, is dying")
                     return (EvalComplete Nothing,VoidVal)
                 where delScript (k,s) = getWorldScripts >>= return . M.delete (k,s) >>= setWorldScripts
             _ -> do lift $ logFromScript info ("attachment cannot die") 
                     return (EvalIncomplete,VoidVal)
        
llAttachToAvatar info@(ScriptInfo oid _ sid pk _) [IVal attachPoint] =
    if attachPoint `elem` validAttachmentPoints then
        do  script <- (lift getWorldScripts >>= mlookup (pk,sid))
            attachment <- getPrimAttachment oid
            case attachment of
                Just _ -> lift $ logFromScript info "llAttachToAvatar: already attached"
                Nothing ->
                    case scriptLastPerm script of
                        Nothing -> throwError "no permission to attach"
                        Just k -> do
                            perm <- mlookup k (scriptPermissions script)
                            when (perm .&. cPermissionAttach == 0) $ throwError "no permission to attach"
                            owner <- getPrimOwner pk
                            if (k /= owner)
                                then lift $ putChat (Just 20.0) pk 0 ("Script trying to attach to someone other than owner!")
                                else do 
                                    avatars <- lift getWorldAvatars
                                    let av = M.lookup k avatars
                                    case av of 
                                        Nothing -> throwError "avatar not present in sim"
                                        Just av ->
                                            let attachments = avatarAttachments av in
                                                case IM.lookup attachPoint attachments of
                                                    Nothing -> let av' = av { avatarAttachments = IM.insert attachPoint oid attachments } in
                                                        do lift $ setWorldAvatars (M.insert k av' avatars)
                                                           lift $ setPrimAttachment oid (Just $ Attachment k attachPoint)
                                                           lift $ pushAttachEvent pk k
                                                    Just _ -> throwError "attachment point already occupied"
            continueWith VoidVal
        else throwError ("invalid attachment point: " ++ show attachPoint)

llDetachFromAvatar info@(ScriptInfo oid _ sid pk _) [] =
    do  when (oid /= pk) $ throwError "can't detach from within child prim"
        script <- (lift getWorldScripts >>= mlookup (pk,sid)) <||> throwError "can't find current script!"
        attachment <- getPrimAttachment oid
        when (isNothing attachment) $ throwError "not attached!"
        Just (Attachment k attachPoint) <- return attachment
        perm <- (mlookup k $ scriptPermissions script) <||> throwError "no permission to detach"
        when (perm .&. cPermissionAttach == 0) $ throwError "no permission to attach"
        avatars <- lift getWorldAvatars
        avatar <- mlookup k avatars <||> throwError "can't find avatar!"
        let attachments = avatarAttachments avatar
        lift $ setWorldAvatars (M.insert k (avatar { avatarAttachments = IM.delete attachPoint attachments }) avatars)
        lift $ setPrimAttachment oid Nothing
        lift $ pushAttachEvent oid nullKey
        lift $ putWorldEvent 1 (DetachCompleteEvent oid k)
        continueWith VoidVal
    
llGetAttached info@(ScriptInfo oid _ _ _ _) [] = 
    getPrimAttachment oid >>= return . maybe 0 attachmentPoint >>= continueWith . IVal

llGiveMoney info@(ScriptInfo _ _ sn pk _) [KVal ak, IVal amount] =
    do  script <- (lift getWorldScripts >>= mlookup (pk,sn)) <||> (throwError "can't find script!")
        permKey <- maybe (throwError "no permission") (return . id) (scriptLastPerm script)
        perm <- mlookup permKey (scriptPermissions script)
        when (perm .&. cPermissionAttach == 0) $ throwError "no permission"
        lift $ logFromScript info ("llGiveMoney: pretending to give " ++ show amount ++ " to avatar with key " ++ ak)
        continueWith (IVal 0)
    
llGetPermissionsKey (ScriptInfo _ _ sid pk _) [] =
    lift getWorldScripts >>= mlookup (pk,sid) >>= continueWith . KVal . fromMaybe nullKey . scriptLastPerm
    
llGetPermissions (ScriptInfo _ _ sid pk _) [] = getPermissions pk sid >>= continueWith . IVal

getPermissions pk s = do
    script <- (lift getWorldScripts >>= mlookup (pk,s))
    case scriptLastPerm script of
        Nothing -> return 0
        Just k -> mlookup k (scriptPermissions script)

getPermittedAvKey info@(ScriptInfo _ _ s pk _) fname perm = do
    script <- (lift getWorldScripts >>= mlookup (pk,s))
    case scriptLastPerm script of
        Nothing -> lift $ logFromScript info (fname ++ ": not permitted") >> return Nothing
        Just k -> case M.lookup k (scriptPermissions script) of
            Nothing -> lift $ logFromScript info (fname ++ ": internal error getting permissions") >> return Nothing
            Just i | i .&. perm /= 0 -> return (Just k)
                   | otherwise -> return Nothing 
                   
llRequestPermissions (ScriptInfo _ _ sid pk _) [KVal k, IVal mask] =
    do avatars <- lift $ getWorldAvatars
       case M.lookup k avatars of
           Nothing -> lift $ logAMessage LogInfo (pk ++ ":" ++ sid) ("Invalid permissions request: no such avatar: " ++ k)
           _ -> lift $ putWorldEvent 0 (PermissionRequestEvent pk sid k mask)
       continueWith VoidVal

llClearCameraParams info@(ScriptInfo _ _ sid pk _) [] =
    do  avKey <- getPermittedAvKey info "llClearCameraParams" cPermissionControlCamera
        case avKey of
            Nothing -> return ()
            Just k -> do
                avatars <- lift getWorldAvatars
                av <- mlookup k avatars
                lift $ setWorldAvatars (M.insert k (av { avatarCameraControlParams = defaultCamera }) avatars)
        continueWith VoidVal
    
llGetCameraPos info@(ScriptInfo _ _ sid pk _) [] =
    do  avKey <- getPermittedAvKey info "llGetCameraPos" cPermissionTrackCamera
        maybe (return (0,0,0)) (\ k -> lift getWorldAvatars >>= mlookup k >>= return . avatarCameraPosition) avKey >>=
            continueWith . vec2VVal

llGetCameraRot info@(ScriptInfo _ _ sid pk _) [] =
    do  avKey <- getPermittedAvKey info "llGetCameraRot" cPermissionTrackCamera
        maybe (return (0,0,0,1)) (\ k -> lift getWorldAvatars >>= mlookup k >>= return . avatarCameraRotation) avKey
            >>= continueWith . rot2RVal
 
llTakeCamera info _ = lift $ logFromScript info "llTakeCamera: deprecated!" >> continueWith VoidVal
llReleaseCamera info _ = lift $ logFromScript info "llRelaaseCamera: deprecated!" >> continueWith VoidVal
llPointAt info _ = lift $ logFromScript info "llPointAt: deprecated!" >> continueWith VoidVal
llStopPointAt info _ = lift $ logFromScript info "llStopPointAt: deprecated!" >> continueWith VoidVal

llSetPrimURL info _ = lift $ logFromScript info "llSetPrimURL: unimplemented!" >> continueWith VoidVal
llRefreshPrimURL info _ = lift $ logFromScript info "llRefreshPrimURL: unimplemted!" >> continueWith VoidVal

llGetRot info@(ScriptInfo oid _ _ pk _) [] =
    getGlobalRot pk >>= continueWith . rot2RVal

llGetLocalRot info@(ScriptInfo oid _ _ pk _) [] =
    (if oid == pk
        then getObjectRotation oid
        else getPrimRotation pk) >>= continueWith . rot2RVal

llGetRootRotation info@(ScriptInfo oid _ _ pk _) [] = getObjectRotation oid >>= continueWith . rot2RVal
    
--TODO: handle attachments...
--TODO: confirm order of rotations in both of these
llSetRot info@(ScriptInfo oid _ _ pk _) [r@(RVal _ _ _ _)] = 
    (if oid == pk then setRootRotation oid rot else setChildRotation oid pk rot) >> continueWith VoidVal
    where rot = rVal2Rot r

-- TODO: fix this!!
setChildRotation rk pk rot = do
    rootRot <- getPrimRotation rk
    lift $ setPrimRotation pk (rot `quaternionMultiply` rootRot `quaternionMultiply` rootRot)

setRootRotation rk rot = do
    obj <- getObject rk
    lift $ setObject rk (obj { objectDynamics = (objectDynamics obj) { objectRotation = rot }})
        
llSetLocalRot info@(ScriptInfo oid _ _ pk _) [r@(RVal _ _ _ _)] =
    (if oid == pk then setRootRotation oid rot else lift $ setPrimRotation pk rot) >> continueWith VoidVal
    where rot = rVal2Rot r
    
llSetPos info@(ScriptInfo oid pid sid pk event) [val] = 
    (if oid == pk then setRootPos oid v else setChildPos pk v) >> continueWith VoidVal
    where v = vVal2Vec val

setChildPos pk val = lift $ setPrimPosition pk val
                
-- updates the world coordinates of object
-- does NOT consider region boundaries (TODO: fix!)
setRootPos oid v =
   do v0 <- getObjectPosition oid
      let vec = diff3d v v0
      let dist = mag3d vec
      let vec' = if dist <= 10.0 
              then vec
              else scale3d 10.0 $ norm3d vec
      --objects <- lift getObjects
      obj <- getObject oid
      lift $ setObject oid (obj { objectDynamics = (objectDynamics obj) { objectPosition = v0 `add3d` vec' } })

llGetOwner info [] = 
    let k = scriptInfoPrimKey info in getPrimOwner k >>= continueWith . KVal
llGetCreator info [] =
    let k = scriptInfoPrimKey info in getPrimCreator k >>= continueWith . KVal
llSameGroup info@(ScriptInfo _ _ _ pk _) [KVal k] =
    do  group <- avGroup <||> getPrimGroup k <||> throwError ("no such object or avatar: " ++ k)
        myGroup <- getPrimGroup pk <||> throwError ("can't find " ++ pk)
        continueWith (IVal (if group == myGroup then 1 else 0))
    where avGroup = lift getWorldAvatars >>= mlookup k >>= return . avatarActiveGroup

llGetLinkKey (ScriptInfo oid _ _ _ _) [IVal link] = do
    do  LSLObject { primKeys = pkeys } <- getObject oid
        pk <- lookupByIndex (link - 1) pkeys
        continueWith (KVal pk)

-- TODO: should check for av/prim in same region
-- TODO: no concept of online/offline for av           
llKey2Name info [KVal k] = lift $
    do avs <- getWorldAvatars
       case M.lookup k avs of
           Just av -> continueWith $ SVal (avatarName av)
           _ -> do
               prims <- getPrims
               case M.lookup k prims of
                   Just prim -> continueWith $ SVal (primName prim)
                   Nothing -> continueWith (SVal "")
llOwnerSay info [SVal s] =
    do lift $ logAMessage LogInfo (infoToLogSource info) ("Owner Say: " ++ s)
       owner <- getPrimOwner (scriptInfoObjectKey info)
       lift $ putWorldEvent 0 (AvatarInputEvent owner (AvatarOwnerSay (scriptInfoPrimKey info) s))
       continueWith VoidVal
       
llGetGeometricCenter info@(ScriptInfo oid _ _ _ _) [] =
    do  LSLObject { primKeys = links } <- getObject oid
        when (null links) $ throwError "error: empty linkset!"
        rootPos <- getObjectPosition oid
        foldM (\ v k -> getGlobalPos k >>= return . add3d v) (0,0,0) links 
            >>= continueWith . vec2VVal . scale3d (1 / fromIntegral (length links))

llGetMass info@(ScriptInfo oid _ _ _ _) [] =
    objectMass oid >>= continueWith . FVal    

objectMass oid = do
    LSLObject { primKeys = links } <- getObject oid
    foldM (\ v k -> getPrim k >>= ( \ prim -> return $ v + primMassApprox prim)) 0.0 links
    
llGetObjectMass info [KVal k] =
    do  avatars <- lift getWorldAvatars
        val <- case M.lookup k avatars of
            Just _ -> return 80.0
            Nothing -> objectMass k
        continueWith (FVal val)
    
llGetVel info@(ScriptInfo oid _ _ _ _) [] =
    getObject oid >>= continueWith . vec2VVal . objectVelocity . objectDynamics

llGetForce info@(ScriptInfo oid _ _ _ _) [] =
    getObject oid >>= return . objectForce . objectDynamics >>= continueWith . vec2VVal . fst
        
llSetForce info@(ScriptInfo oid _ _ _ _) [v@(VVal x y z), IVal local] =
    do  obj <- getObject oid
        lift $ setObject oid (obj { objectDynamics = (objectDynamics obj) { objectForce = ((x,y,z), local /= 0) }})
        continueWith VoidVal

llSetBuoyancy info@(ScriptInfo { scriptInfoObjectKey = oid }) [FVal buoy] =
   do  obj <- getObject oid
       lift $ setObject oid (obj { objectDynamics = (objectDynamics obj) { objectBuoyancy = buoy }})
       continueWith VoidVal 
   
llApplyImpulse info@(ScriptInfo oid _ _ _ _) [v@(VVal x y z), IVal local] =
   do t <- lift getTick
      obj <- getObject oid
      lift $ setObject oid (obj { objectDynamics = (objectDynamics obj) { objectImpulse = (((x,y,z), local /= 0), t + durationToTicks 1) } })
      continueWith VoidVal
        
llGetAccel info@(ScriptInfo oid _ _ _ _) [] =
    do  prim <- getPrim oid
        obj <- getObject oid
        let dyn = objectDynamics obj
        let force = fst (objectForce dyn) `rot3d` rotation 
                where rotation = if snd $ objectForce dyn then objectRotation dyn else (0,0,0,1)
        let impulse = ((fst . fst . objectImpulse) dyn `rot3d` rotation, (snd . objectImpulse) dyn)
                where rotation = if (snd . fst . objectImpulse) dyn then objectRotation dyn else (0,0,0,1)
        t <- lift getTick
        mass <- objectMass oid
        pos <- getObjectPosition oid
        continueWith $ vec2VVal $ calcAccel t 0 mass pos force impulse
        
llSetTorque info@(ScriptInfo { scriptInfoObjectKey = oid }) [VVal x y z, IVal local] =
    do  obj <- getObject oid
        lift $ setObject oid (obj { objectDynamics = (objectDynamics obj) { objectTorque = ((x,y,z),local /= 0) } })
        continueWith VoidVal

llSetForceAndTorque info@(ScriptInfo { scriptInfoObjectKey = oid}) [VVal fx fy fz, VVal tx ty tz, IVal local] =
    do  obj <- getObject oid
        let loc = local /= 0
        lift $ setObject oid (obj { objectDynamics = (objectDynamics obj) { 
                                        objectForce = ((fx,fy,fz),loc), objectTorque = ((tx,ty,tz),loc) } })
        continueWith VoidVal

llGetTorque info@(ScriptInfo { scriptInfoObjectKey = oid }) [] =
    getObject oid >>= continueWith . vec2VVal . fst . objectTorque . objectDynamics

llGetOmega info@(ScriptInfo { scriptInfoObjectKey = oid }) [] =
    getObject oid >>= continueWith . vec2VVal . objectOmega . objectDynamics
    
llApplyRotationalImpulse info@(ScriptInfo { scriptInfoObjectKey = oid }) [VVal x y z, IVal local] =
    do  obj <- getObject oid
        t <- lift getTick
        lift $ setObject oid (obj { 
            objectDynamics = (objectDynamics obj) { objectRotationalImpulse = (((x,y,z),local /= 0), t + durationToTicks 1) } })
        continueWith VoidVal
   
llGetPos info@(ScriptInfo _ _ _ pk _) [] = getGlobalPos pk >>= continueWith . vec2VVal 
llGetRootPosition info@(ScriptInfo oid _ _ _ _) [] = getObjectPosition oid >>= continueWith . vec2VVal

getGlobalPos pk = do
        oid <- getRootPrim pk
        root <- getObjectPosition oid
        rel <- getPrimPosition pk
        rot <- getObjectRotation oid
        return (root `add3d` (rot3d rel rot))
        
getGlobalRot pk = do
    oid <- getRootPrim pk
    rot0 <- getObjectRotation oid
    rot <- getPrimRotation pk
    return (rot `quaternionMultiply` rot0)
    
llRotLookAt info@(ScriptInfo oid _ _ _ _) [RVal x y z s, FVal strength, FVal tau] =
    rotLookAt oid (x,y,z,s) strength tau >> continueWith VoidVal

llLookAt info@(ScriptInfo oid _ _ _ _) [VVal x y z, FVal strength, FVal tau] =
    do  pos <- getObjectPosition oid
        rot <- getGlobalRot oid
        let v = diff3d (x,y,z) pos
        let rot1 = rot `quaternionMultiply` (rotationBetween (rot3d (0,0,1) rot) v)
        rotLookAt oid rot1 strength tau
        continueWith VoidVal
        
rotLookAt oid (x,y,z,s) strength tau = do
    obj <- getObject oid
    lift $ setObject oid $ obj { objectDynamics = (objectDynamics obj) { objectRotationTarget = 
                                                                            Just (RotationTarget {
                                                                                     rotationTarget = (x,y,z,s),
                                                                                     rotationTargetStrength = strength,
                                                                                     rotationTargetTau = tau } ) }}

llStopLookAt info@(ScriptInfo oid _ _ _ _) [] =
    do  obj <- getObject oid
        lift $ setObject oid $ obj { objectDynamics = (objectDynamics obj) { objectRotationTarget = Nothing }}
        continueWith VoidVal
                                                                                     
llGetLocalPos info@(ScriptInfo oid _ _ pk _) [] =
    getLocalPos pk >>= continueWith . vec2VVal
    
getLocalPos k = getPrimPosition k         

llMoveToTarget info@(ScriptInfo { scriptInfoObjectKey = oid, scriptInfoScriptName = sn, scriptInfoPrimKey = pk }) [VVal x y z, FVal tau] =
    do  obj <- getObject oid
        let action = setTarg obj
        case objectPositionTarget (objectDynamics obj) of
            Nothing -> action
            Just (PositionTarget { positionTargetSetBy = scriptId }) 
                | scriptId == (pk,sn) -> action
                | otherwise -> lift $ logAMessage LogInfo "llMoveToTarget" ("target already set by another script: " ++ show scriptId)
            Just _ -> action
        continueWith VoidVal                        
    where setTarg obj = lift $ setObject oid obj'
              where targ = PositionTarget { positionTargetTau = tau,
                                            positionTargetLocation = (x,y,z),
                                            positionTargetSetBy = (pk,sn) }
                    obj' = obj { objectDynamics = (objectDynamics obj) { objectPositionTarget = Just targ } }

llStopMoveToTarget info@(ScriptInfo { scriptInfoObjectKey = oid, scriptInfoScriptName = sn, scriptInfoPrimKey = pk }) [] =
    do  obj <- getObject oid
        case objectPositionTarget $ objectDynamics obj of
            Just (PositionTarget {}) -> lift $ setObject oid (obj { objectDynamics = (objectDynamics obj) { objectPositionTarget = Nothing }})
            _ -> return ()
        continueWith VoidVal
    
llGroundRepel info@(ScriptInfo { scriptInfoObjectKey = oid }) [FVal height, IVal water, FVal tau] =
    do  obj <- getObject oid
        lift $ setObject oid (obj { objectDynamics = (objectDynamics obj) { 
                                         objectPositionTarget = Just $ Repel {
                                             positionTargetTau = tau,
                                             positionTargetOverWater = water /= 0,
                                             positionTargetHeight = height } } })
        continueWith VoidVal

llSetHoverHeight info@(ScriptInfo { scriptInfoObjectKey = oid }) [FVal height, IVal water, FVal tau] =
    do  obj <- getObject oid
        lift $ setObject oid (obj { objectDynamics = (objectDynamics obj) { 
                                         objectPositionTarget = Just $ Hover {
                                             positionTargetTau = tau,
                                             positionTargetOverWater = water /= 0,
                                             positionTargetHeight = height } } })
        continueWith VoidVal

llStopHover info@(ScriptInfo { scriptInfoObjectKey = oid }) [] =
    do  obj <- getObject oid
        case objectPositionTarget $ objectDynamics obj of 
            Just (Hover {}) -> lift $ setObject oid (obj { objectDynamics = (objectDynamics obj) { objectPositionTarget = Nothing }})
            _ -> lift $ logAMessage LogWarn "llStopHover" "not hovering"
        continueWith VoidVal
    
llGetAlpha (ScriptInfo _ _ _ pkey _) [IVal side] =
    computeAlpha >>= continueWith
    where computeAlpha = if (side /= -1) 
            then runFace pkey side $ (getPrimFaceAlpha pkey side >>= return . FVal)
            else runFace pkey side $ do
                faces <- getPrimFaces pkey
                let n = length faces
                let alpha = if n > 0 then sum (map faceAlpha faces) / (fromInt $ length faces)
                                     else 1.0
                return $ FVal alpha

llSetAlpha (ScriptInfo _ _ _ pkey _) [FVal alpha, IVal face] = lift $ setAlpha alpha face pkey

setAlpha alpha face pkey =
    if face == -1
        then runErrFace pkey face (EvalIncomplete,VoidVal) $ do 
                faces <- getPrimFaces pkey
                let faces' = map (\ face -> face { faceAlpha = alpha }) faces
                lift $ setPrimFaces pkey faces'
                continueWith VoidVal
        else setPrimFaceAlpha pkey face alpha >> continueWith VoidVal

llSetLinkAlpha (ScriptInfo oid pid _ _ _) [IVal link, FVal alpha, IVal face] = do
    pks <- lift $ getTargetPrimKeys oid link pid
    lift $ mapM_ (setAlpha alpha face) pks
    continueWith VoidVal

getTargetPrimKeys oid link pid = do
    LSLObject { primKeys = prims } <- runAndLogIfErr ("can't find object " ++ oid) (LSLObject [] defaultDynamics) $ getObject oid
    let targetList = targetLinks (length prims) link pid
    mapM (flip lookupByIndex prims) targetList
    
llGetColor (ScriptInfo _ _ _ pkey _) [IVal side] =
    computeColor >>= continueWith
    where computeColor = if (side /= -1)
            then runFace pkey side $ (getPrimFaceColor pkey side >>= return . vec2VVal)
            else runFace pkey side $ do
                faces <- getPrimFaces pkey
                let n = length faces
                let color = if n > 0 then scale3d (1.0/ (fromInt $ length faces) ) 
                                                  (foldr add3d (0.0,0.0,0.0) (map faceColor faces))
                                          else (1.0,1.0,1.0)
                return $ vec2VVal color

llSetColor (ScriptInfo _ _ _ pkey _) [color, IVal face] = setColor color face pkey

setColor color face pkey =
    if face == -1
        then runFace pkey face $ do
                faces <- getPrimFaces pkey
                let colorVal = vVal2Vec color
                let faces' = map (\ face -> face { faceColor = colorVal }) faces
                lift $ setPrimFaces pkey faces'
                continueWith VoidVal
        else lift $ setPrimFaceColor pkey face (vVal2Vec color) >> continueWith VoidVal

llSetLinkColor (ScriptInfo oid pid _ _ _) [IVal link, color, IVal face] = do
    pks <- lift $ getTargetPrimKeys oid link pid
    mapM (setColor color face) pks
    continueWith VoidVal

llGetTextureOffset (ScriptInfo _ _ _ pkey _) [IVal face] =
    let face' = if face == -1 then 0 else face in
        runFace pkey face (getPrimFaceTextureInfo pkey face' >>= return . textureOffsets) >>= continueWith . vec2VVal
llGetTextureScale (ScriptInfo _ _ _ pkey _) [IVal face] =
    let face' = if face == -1 then 0 else face in
        runFace pkey face (getPrimFaceTextureInfo pkey face' >>= return . textureRepeats) >>= continueWith . vec2VVal
llGetTextureRot (ScriptInfo _ _ _ pkey _) [IVal face] =
    let face' = if face == -1 then 0 else face in
        runFace pkey face (getPrimFaceTextureInfo pkey face' >>= return . textureRotation) >>= continueWith . FVal
        
llSetTexture info@(ScriptInfo _ _ _ pk _) [SVal texture,IVal face] = 
    do  tk <- findTexture pk texture
        setTexture tk face pk
        continueWith VoidVal
       
-- TODO: worry about texture permissions
llGetTexture (ScriptInfo _ _ _ pkey _) [IVal face] =
    let face' = if face == -1 then 0 else face in
        runFace pkey face (do
            info <- getPrimFaceTextureInfo pkey face'
            invTextures <- getPrimTextures pkey
            let tKey = textureKey info
            case findByInvKey tKey invTextures of
                 Nothing -> return tKey
                 Just item -> return $ invName item) >>= continueWith . SVal
                 
llSetLinkTexture info@(ScriptInfo oid pid _ pk _) [IVal link, SVal texture,IVal face] = do
    pks <- lift $ getTargetPrimKeys oid link pid
    tk <- findTexture pk texture
    mapM_ (setTexture tk face) pks
    continueWith VoidVal
    
findTexture pk id =
    do  textures <- getPrimTextures pk
        case findByInvName id textures of
            Just item -> return $ snd $ inventoryItemNameKey $ inventoryItemIdentification item
            Nothing -> do
                result <- return $ findTextureAsset id
                case result of
                    Nothing -> throwError ("cannot find texture " ++ id)
                    Just item -> return $ snd $ inventoryItemNameKey $ inventoryItemIdentification item
            
setTexture tk face pkey = do
    if face == -1
        then do faces <- getPrimFaces pkey
                let faces' = map (\ face -> 
                        let info = faceTextureInfo face in 
                            face { faceTextureInfo = info { textureKey = tk } }) faces
                lift $ setPrimFaces pkey faces'
        else do faces <- getPrimFaces pkey
                f <- lookupByIndex face faces
                let tInfo = faceTextureInfo f
                updatePrimFace pkey face (\ f -> f { faceTextureInfo = tInfo { textureKey = tk } })
    
updateTextureInfo update pk faceIndex =
    runFace pk faceIndex (
        if faceIndex == -1
            then do
                faces <- getPrimFaces pk
                let faces' = map (\ face ->
                        let info = faceTextureInfo face in
                            face { faceTextureInfo = update info }) faces
                lift $ setPrimFaces pk faces'
            else do
                f <- getPrimFaces pk >>= lookupByIndex faceIndex
                let tInfo = update $ faceTextureInfo f
                updatePrimFace pk faceIndex (\ f -> f { faceTextureInfo = tInfo })
    ) >> continueWith VoidVal

llScaleTexture info@(ScriptInfo _ _ _ pk _) [FVal h,FVal v,IVal faceIndex] = 
    updateTextureInfo update pk faceIndex where update info = info { textureRepeats = (h,v,0) }
llOffsetTexture (ScriptInfo _ _ _ pk _) [FVal h, FVal v, IVal faceIndex] =
    updateTextureInfo update pk faceIndex where update info = info { textureOffsets = (h,v,0) }
llRotateTexture (ScriptInfo _ _ _ pk _) [FVal rot, IVal faceIndex] =
    updateTextureInfo update pk faceIndex where update info = info { textureRotation = rot }
        
getPos pkey = runErrPrim pkey
                  (VVal 0.0 0.0 0.0)
                  (getRootPrim pkey >>= getObjectPosition >>= return . vec2VVal)

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
            foldl' (.|.) 0 $ zipWith (shiftL) (replicate (length rands') 0xffffffff) [0,32..]
        randval :: Rational
        randval = fromInteger $ foldl' (.|.) 0 $ zipWith (shiftL) rands' [0,32]
    in floor ((randval / topval) * (fromInteger maxval))
        
    
llListRandomize _ [LVal list, IVal stride] =
    let l1 = groupList stride list
        n = fac (toInteger $ length l1)
        randsNeeded = ceiling $ (logBase 2 (fromInteger n)) / 32
        wrands = replicate randsNeeded wrand
    in  lift $ do 
           rands <- sequence wrands
           let permutation = genNum rands n
           let list' = generatePermutation list permutation
           continueWith $ LVal list'
        
llGetNumberOfPrims (ScriptInfo oid _ _ pkey _) [] = 
    do (LSLObject { primKeys = prims }) <- getObject oid
       continueWith $ IVal (length prims)

llGetObjectPrimCount info@(ScriptInfo oid _ _ pkey _) [KVal k] =
    do
        region <- getPrimRegion oid
        p <- getPrim k
        region' <- getPrimRegion k
        if region /= region' 
            then continueWith (IVal 0)
            else getRootPrim k >>= primCount >>= continueWith . IVal
    where primCount oid = do
            (LSLObject { primKeys = l}) <- getObject oid
            return $ length l

llGetNumberOfSides (ScriptInfo _ _ _ pkey _) [] =
     getPrimFaces pkey >>= continueWith . IVal . length
     
llGetObjectDesc (ScriptInfo _ _ _ pkey _) [] =
    getPrimDescription pkey >>= continueWith . SVal
llGetObjectName (ScriptInfo _ _ _ pkey _) [] =
    getPrimName pkey >>= continueWith . SVal 

llSetObjectName (ScriptInfo _ _ _ pkey _) [SVal name] =
    lift $ setPrimName pkey (take 255 name) >> continueWith VoidVal
    
llSetObjectDesc (ScriptInfo _ _ _ pkey _) [SVal desc] =
    lift $ setPrimDescription pkey (take 127 desc) >> continueWith VoidVal
    
llGetObjectPermMask (ScriptInfo oid _ _ _ _) [IVal maskId] =
    getObjectPermMask oid maskId >>= continueWith . IVal

getObjectPermMask oid maskId = do
       masks <- getPrimPermissions oid
       let base = if null masks then 0x0008e000 else masks !! 0
       let n = length masks
       return (if maskId `elem` [0..(n-1)] then masks !! maskId else 
                      if maskId == 3 then 0 else base)
          
llGetScale (ScriptInfo _ _ _ pkey _) [] = getPrimScale pkey >>= continueWith . vec2VVal
    
llSetScale (ScriptInfo _ _ _ pk _) [scale] =
    if tooSmall then continueWith VoidVal else lift $ setPrimScale pk clippedVec >> continueWith VoidVal
    where (x,y,z) = vVal2Vec scale
          tooSmall = (x < 0.01 || y < 0.01 || z < 0.01)
          clippedVec = (min x 10.0, min y 10.0, min z 10.0) 
  
llGetBoundingBox info [KVal k] =
    do  lift $ logAMessage LogInfo "sim" "note: llGetBoundingBox does not return accurate results (yet)"
        avatars <- lift getWorldAvatars
        case M.lookup k avatars of
            Just avatar -> let pos = avatarPosition avatar in
                continueWith (LVal $ map (vec2VVal . (add3d pos)) [(-1.0,-1.0,-1.0),(1.0,1.0,1.0)])
            Nothing -> getRootPrim k >>= getPrimBox >>= continueWith . LVal . map vec2VVal . tup2l
    where tup2l (x,y) = [x,y]
    
getPrimBox pk = do
    pos <- getGlobalPos pk
    scale <- getPrimScale pk
    let (xs,ys,zs) = scale3d 0.5 scale
    return (add3d pos (-xs, -ys, -zs),add3d pos (xs,ys,zs))

-- this is extremely approximate....
objRadius oid = do
        LSLObject { primKeys = keys } <- getObject oid
        foldM f 0 keys
    where f r k = do
              (x,y,z) <- getPrimPosition k
              (sx,sy,sz) <- getPrimScale k
              let (rx,ry,rz) = (sx / 2 + abs x, sy / 2 + abs y, sz / 2 + abs z)
              return $ maximum [r,rx,ry,rz]
              
llSetTimerEvent (ScriptInfo _ _ sn pk _) [FVal interval] =
    -- TODO: this may not accurately reflect buggy behavior in SL
    do lift $ removePendingTimerEvent pk sn
       when (interval > 0) $ lift $ putWorldEvent interval (TimerEvent interval (pk,sn))
       continueWith VoidVal
    where removePendingTimerEvent pk sn = do
              wq <- getWQueue
              let wq' = flip filter wq $ \ e -> case e of
                      (_,TimerEvent _ (pk',sn')) -> pk /= pk' || sn /= sn'
                      _ -> True
              setWQueue wq'
              
llSetVehicleFlags info@(ScriptInfo _ _ _ pk _) [IVal flagsToSet] =
    do  flags <- getPrimVehicleFlags pk `catchError` primErr pk
        lift $ setPrimVehicleFlags pk (flags .|. flagsToSet)
        continueWith VoidVal
    
llRemoveVehicleFlags info@(ScriptInfo _ _ _ pk _) [IVal flagsToClear] =
    do  flags  <- getPrimVehicleFlags pk `catchError` primErr pk
        lift $ setPrimVehicleFlags pk (flags .&. complement flagsToClear)
        continueWith VoidVal
    
----------------------------------------------------------------------
-- get/set prim parameters
llGetPrimitiveParams (ScriptInfo _ _ _ pk _) [LVal l] = getPrimParameters pk l >>= continueWith . LVal
llSetPrimitiveParams (ScriptInfo _ _ _ pk _) [LVal l] = setPrimParameters pk l >> continueWith VoidVal

llSetLinkPrimitiveParams (ScriptInfo oid pid _ _ _) [IVal link,LVal l] = do
    do  LSLObject { primKeys = pks } <- getObject oid
        let targetList = targetLinks (length pks) link pid
        pkList <- mapM (flip lookupByIndex pks) targetList
        mapM_ (flip setPrimParameters l) pkList
        continueWith VoidVal
    
getPrimParameters pk params =
    case params of
        [] -> return []
        (x:xs) | x `elem` [llcPrimBumpShiny,llcPrimFullbright,llcPrimColor,llcPrimTexture,llcPrimTexgen] && null xs -> return []
               | x == llcPrimBumpShiny  -> queryAndDoRest (queryPrimBumpShiny $ head xs) (tail xs)
               | x == llcPrimColor -> queryAndDoRest (queryPrimColor $ head xs) (tail xs)
               | x == llcPrimTexture -> queryAndDoRest (queryPrimTexture $ head xs) (tail xs)
               | x == llcPrimTexgen -> queryAndDoRest (queryPrimTexgen $ head xs) (tail xs)
               | x == llcPrimFullbright -> queryAndDoRest (queryPrimFullbright $ head xs) (tail xs)
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
queryPrimRotation k = getGlobalRot k >>= return . return . rot2RVal
queryPrimPosition k = getGlobalPos k >>= return . return . vec2VVal
queryPrimScale k = getPrimScale k >>= return . return . vec2VVal
queryPrimFlexible k = 
    getPrimFlexibility k >>= (\ flex -> case flex of
        Nothing -> return [IVal 0, IVal 0, FVal 0, FVal 0, FVal 0, FVal 0, VVal 0.0 0.0 0.0]
        Just flex' -> return $ map ($ flex') [const (IVal 1),IVal . flexSoftness, FVal . flexGravity, FVal . flexFriction,
                                             FVal . flexWind, FVal . flexTension, vec2VVal . flexForce])
queryPrimLight k =
    getPrimLight k >>= (\ light -> case light of
        Nothing -> return [IVal 0, VVal 0 0 0, FVal 0, FVal 0, FVal 0]
        Just light' -> return $ map ($ light') [const (IVal 1), vec2VVal . lightColor, FVal . lightIntensity, FVal . lightRadius, FVal . lightFalloff])
    
queryPrimMaterial k = getPrimMaterial k >>= return . return . IVal
queryPrimStatus bit k =  getPrimStatus k >>= return . return . IVal . (\ i -> if testBit i bit then 1 else 0)
queryPrimBumpShiny side =  queryFaceVals bumpShiny side
    where bumpShiny face = [IVal $ faceShininess face, IVal $ faceBumpiness face]
queryPrimColor side = queryFaceVals colorAlpha side
    where colorAlpha face = [vec2VVal $ faceColor face, FVal $ faceAlpha face]
queryPrimTexture side = queryFaceVals textureInfo side
    where textureInfo face = let tinfo = faceTextureInfo face in map ($ tinfo) 
                                   [SVal .textureKey,vec2VVal . textureRepeats,vec2VVal . textureOffsets,FVal . textureRotation]
queryPrimTexgen side = queryFaceVals (return . IVal . faceTextureMode) side
queryPrimFullbright side = queryFaceVals (\ face -> if faceFullbright face then [IVal 1] else [IVal 0]) side

queryFaceVals f (IVal side) k = 
    if side == -1
        then getPrimFaces k >>= return . concat . (map f)
        else getPrimFaces k >>= fromErrorT [] . (\ faces -> (lookupByIndex side faces >>= return . f))

-- TODO: get these out of here
Just (Constant _ llcObjectUnknownDetail) = findConstant "OBJECT_UNKNOWN_DETAIL"
Just (Constant _ llcObjectName) = findConstant "OBJECT_NAME"
Just (Constant _ llcObjectDesc) = findConstant "OBJECT_DESC"
Just (Constant _ llcObjectPos) = findConstant "OBJECT_POS"
Just (Constant _ llcObjectRot) = findConstant "OBJECT_ROT"
Just (Constant _ llcObjectVelocity) = findConstant "OBJECT_VELOCITY"
Just (Constant _ llcObjectOwner) = findConstant "OBJECT_OWNER"
Just (Constant _ llcObjectGroup) = findConstant "OBJECT_GROUP"
Just (Constant _ llcObjectCreator) = findConstant "OBJECT_CREATOR"

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

setPrimParameters pk params = updatePrimParameters pk params >> return ()
    
updatePrimParameters pk [] = return []
updatePrimParameters pk (code:rest) = do
    result <- case code of
        i | i == llcPrimTempOnRez -> updatePrimTempOnRez pk rest
          | i == llcPrimMaterial -> updatePrimMaterial pk rest
          | i == llcPrimPhantom -> updatePrimPhantom pk rest
          | i == llcPrimPhysics -> updatePrimPhysics pk rest
          | i == llcPrimPosition -> updatePrimPosition pk rest
          | i == llcPrimRotation -> updatePrimRotation pk rest
          | i == llcPrimSize -> updatePrimScale pk rest
          | i == llcPrimFlexible -> updatePrimFlexible pk rest
          | i == llcPrimPointLight -> updatePrimLight pk rest
          | i == llcPrimBumpShiny -> updatePrimBumpShiny pk rest
          | i == llcPrimColor -> updatePrimColor pk rest
          | i == llcPrimTexture -> updatePrimTexture pk rest
          | i == llcPrimTexgen -> updatePrimTexgen pk rest
          | i == llcPrimFullbright -> updatePrimFullbright pk rest
          | i == llcPrimType -> updatePrimType pk rest
          | i == IVal 1 -> updatePrimTypeOldSkool pk rest
          | otherwise -> fail "incorrect parameter"
    updatePrimParameters pk result
    
updatePrimType pk vals = getPrim pk >>= flip updatePrimType' vals >>= (\ (prim,rest) -> (lift $ setPrim pk prim) >> return rest)
updatePrimType' prim (primCode@(IVal i):rest) | primCode == llcPrimTypeSphere = updatePrimTypeSphere prim rest
                                              | primCode `elem` [llcPrimTypeBox,llcPrimTypeCylinder,llcPrimTypePrism] =
                                                  updatePrimTypeBoxCylPrism i prim rest
                                              | primCode `elem` [llcPrimTypeTorus,llcPrimTypeTube,llcPrimTypeRing] =
                                                  updatePrimTypeRingTorusTube i prim rest
                                              | primCode == llcPrimTypeSculpt = updatePrimTypeSculpt prim rest
                                              | otherwise = fail "incorrect parameters for PRIM_TYPE"
updatePrimType' _ _ = fail "insufficient or incorrect parameters for PRIM_TYPE"

updatePrimTypeOldSkool pk vals = getPrim pk >>= flip updatePrimTypeOldSkool' vals >>= (\ (prim,rest) -> (lift $ setPrim pk prim) >> return rest)
updatePrimTypeOldSkool' prim (primCode@(IVal i):rest) | primCode == llcPrimTypeSphere = updatePrimTypeSphereOld prim rest
                                                      | primCode `elem` [llcPrimTypeBox,llcPrimTypeCylinder,llcPrimTypePrism] =
                                                          updatePrimTypeBoxCylPrismOld i prim rest
                                                      | primCode == llcPrimTypeTorus = updatePrimTypeTorusOld prim rest
                                                      | primCode == llcPrimTypeTube = updatePrimTypeTubeOld prim rest
                                                      | otherwise = fail "incorrect parameters for deprecated prim type"
updatePrimTypeOldSkool' _ _ = fail "insufficient or incorrect parameters for deprecated prim type"

--updatePrimTempOnRez prim (IVal tempOnRez:rest) = return (prim { primTempOnRez = if tempOnRez /= 0 then True else False }, rest)
updatePrimTempOnRez pk (IVal tempOnRez:rest) = lift (setPrimTempOnRez pk (tempOnRez /= 0)) >> return rest
updatePrimTempOnRez _ _ = fail "insufficient or incorrect parameters for PRIM_TEMP_ON_REZ"

--updatePrimMaterial prim (IVal material:rest) = return (prim { primMaterial = material }, rest)
updatePrimMaterial pk (IVal material:rest) = lift (setPrimMaterial pk material) >> return rest
updatePrimMaterial _ _ = fail "insufficient or incorrect parameters for PRIM_MATERIAL"

updatePrimPhantom pk params = updatePrimStatus "insufficient or incorrect parameters for PRIM_PHANTOM" primPhantomBit pk params
updatePrimPhysics pk params = updatePrimStatus "insufficient or incorrect parameters for PRIM_PHYSICS" primPhysicsBit pk params

-- updatePrimStatus _ bit prim (IVal i:rest) = return (prim { primStatus = if i == 0 then clearBit (primStatus prim) bit
--                                                                                   else setBit (primStatus prim) bit }, rest)
updatePrimStatus _ bit pk (IVal i:rest) = (getPrimStatus pk) >>= lift . (setPrimStatus pk) . (if i == 0 then flip clearBit bit
                                                                                                    else flip setBit bit) >> return rest
updatePrimStatus fMsg _ _ _ = fail fMsg

updatePrimPosition pk (pos@(VVal _ _ _):rest) =
    do  getPrimParent pk >>= \ mok -> case mok of
                Nothing -> setRootPos pk v
                Just ok -> setChildPos pk v
        return rest
    where v = vVal2Vec pos
updatePrimPosition _ _ = fail "insufficient or incorrect parameters for PRIM_POSITION"

updatePrimRotation pk (rot@(RVal _ _ _ _):rest) =
    do getPrimParent pk >>= \ mok -> case mok of
            Nothing -> setRootRotation pk r
            Just rk -> setChildRotation rk pk r
       return rest
    where r = rVal2Rot rot
updatePrimRotation _ _ = fail "insufficient or incorrect parameters for PRIM_ROTATION"
updatePrimScale pk (scale@(VVal _ _ _):rest) = (lift $ setPrimScale pk (vVal2Vec scale)) >> return rest
updatePrimScale _ _ = fail "insufficient or incorrect parameters for PRIM_SIZE"

updatePrimFlexible pk (IVal flex:IVal soft:FVal gravity:FVal friction:FVal wind:FVal tension:VVal fx fy fz:rest) =
    lift (setPrimFlexibility pk (if flex == 0 then Nothing else Just (Flexibility soft gravity friction wind tension (fx,fy,fz)))) >> return rest
updatePrimFlexible _ _ = fail "insufficient or incorrect parameters for PRIM_FLEXIBLE"

updatePrimLight pk (IVal light:VVal r g b:FVal intensity:FVal radius:FVal falloff:rest) =
    lift (setPrimLight pk (if light == 0 then Nothing else Just (LightInfo (r,g,b) intensity radius falloff))) >> return rest
updatePrimLight _ _ = fail "insufficient or incorrect parameters for PRIM_POINT_LIGHT"

updatePrimBumpShiny pk params =
    let extract (IVal face:IVal bump:IVal shiny:rest) = return (face, [IVal bump, IVal shiny], rest)
        extract _ = fail ("insufficient or incorrect parameters for PRIM_BUMP_SHINY")
        update [IVal bump, IVal shiny] face = face { faceBumpiness = bump, faceShininess = shiny }
    in updatePrimFaceParams pk params extract update
updatePrimColor pk params =
    let extract (IVal face:VVal r g b:FVal alpha:rest) = return (face,[VVal r g b, FVal alpha], rest)
        extract _ = fail ("insufficient or incorrect parameters for PRIM_COLOR")
        update [color, FVal alpha] face = face { faceColor = vVal2Vec color, faceAlpha = alpha }
    in updatePrimFaceParams pk params extract update
updatePrimTexture pk params =
    let extract (IVal face:name@(SVal _):repeats@(VVal _ _ _):offsets@(VVal _ _ _):rotation@(FVal _):rest) = 
            return (face,[name,repeats,offsets,rotation],rest)
        extract _ = fail ("insufficient or incorrect parameters for PRIM_TEXTURE")
        update [SVal name,repeats,offsets,FVal rotation] face = 
            face { faceTextureInfo = TextureInfo name (vVal2Vec repeats) (vVal2Vec offsets) rotation }
    in updatePrimFaceParams pk params extract update
updatePrimTexgen pk params =
    let extract (IVal face:IVal mode:rest) = return (face,[IVal mode],rest)
        extract _ = fail "insufficient or incorrect parameters for PRIM_TEXGEN"
        update [IVal mode] face = face { faceTextureMode = mode }
    in updatePrimFaceParams pk params extract update
updatePrimFullbright pk params =
    let extract (IVal face:IVal fullbright:rest) = return (face,[IVal fullbright],rest)
        extract _ = fail "insufficient or incorrect parameters for PRIM_FULLBRIGHT"
        update [IVal fullbright] face = face { faceFullbright = if fullbright == 0 then False else True }
    in updatePrimFaceParams pk params extract update
    
updatePrimFaceParams pk params extract update = do 
    (face, faceParams, rest) <- extract params -- this can fail
    prim <- getPrim pk
    let prim' = if face == -1
                   then prim { primFaces = map (update faceParams) $ primFaces prim }
                   else let (xs,ys) = splitAt face (primFaces prim) in
                       if null ys then prim else prim { primFaces = xs ++ [(update faceParams $ head ys)] ++ (tail ys) }
    lift $ setPrim pk prim'
    return rest
    
updatePrimTypeBoxCylPrism ptype prim (IVal holeshape:VVal cx cy cz:FVal hollow:VVal twx twy twz:VVal tx ty tz:VVal sx sy sz:rest) =
    return (prim { primTypeInfo = (primTypeInfo prim) { primTypeCode = ptype, primHoleshape = holeshape, primCut = (cx,cy,cz),
                                                primHollow = hollow, primTwist = (twx,twy,twz), primTaper = (tx,ty,tz),
                                                primTopshear = (sx,sy,sz) }}, rest)
updatePrimTypeBoxCylPrism _ _ _ = fail "insufficient or incorret parameters for PRIM_TYPE (PRIM_TYPE_PRISM, PRIM_TYPE_CYLINDER, or PRIM_TYPE_BOX)"
updatePrimTypeSphere prim (IVal holeshape:VVal cx cy cz:FVal hollow:VVal tx ty tz:VVal ax ay az:rest) =
    return (prim { primTypeInfo = (primTypeInfo prim) { primTypeCode = cPrimTypeSphere, primHoleshape = holeshape, primCut = (cx,cy,cz),
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
    return (prim { primTypeInfo = (primTypeInfo prim) { primTypeCode = cPrimTypeSculpt, primSculptTexture = Just name, primSculptType = sculptType } }, rest)
updatePrimTypeSculpt _ _ = fail "insufficient or incorrect parameters for PRIM_TYPE (PRIM_TYPE_SCULPT)"

updatePrimTypeBoxCylPrismOld ptype prim (VVal cx cy cz:FVal hollow:FVal twisty:VVal tx ty tz:VVal sx sy sz:rest) =
    let info = primTypeInfo prim in
    return (prim { primTypeInfo = info { primTypeCode = ptype, primCut = (cx,cy,cz), primHollow = hollow,
                                         primTwist = let (x,_,z) = primTwist info in (x,twisty,z), primTaper = (tx,ty,tz),
                                         primTopshear = (sx,sy,sz) } }, rest)
updatePrimTypeBoxCylPrismOld _ _ _ = fail "insufficient or inccorrect parameters for deprecated prim type (PRIM_TYPE_BOX, PRIM_TYPE_CYLINDER, or PRIM_TYPE_PRISM)"

updatePrimTypeSphereOld prim (VVal cx cy cz:FVal hollow:VVal ax ay az:rest) =
    return (prim { primTypeInfo = (primTypeInfo prim) { primTypeCode = cPrimTypeSphere, primCut = (cx,cy,cz), primHollow = hollow,
                                                        primAdvancedCut = (ax,ay,az) }}, rest)
updatePrimTypeSphereOld _ _ = fail "insufficient or incorrect parameters for deprecated prim type (PRIM_TYPE_SPHERE)"

updatePrimTypeTorusOld prim (VVal cx cy cz:FVal hollow:FVal twisty:FVal tapery:VVal sx sy sz:VVal ax ay az:rest) =
    let info = primTypeInfo prim in
    return (prim { primTypeInfo = info { primTypeCode = cPrimTypeTorus, primCut = (cx,cy,cz), primHollow = hollow,
                                         primTwist = let (x,_,z) = primTwist info in (x,twisty,z),
                                         primTaper = let (x,_,z) = primTaper info in (x,tapery,z),
                                         primTopshear = (sx,sy,sz), primAdvancedCut = (ax,ay,az) } }, rest)
updatePrimTypeTorusOld _ _ = fail "insufficient or incorrect parameters for deprecated prim type (PRIM_TYPE_TORUS)"

updatePrimTypeTubeOld prim (VVal cx cy cz:FVal hollow:FVal twisty:FVal shearx:rest) =
    let info = primTypeInfo prim in
    return (prim { primTypeInfo = info { primTypeCode = cPrimTypeTube, primCut = (cx,cy,cz), primHollow = hollow,
                                         primTwist = let (x,_,z) = primTwist info in (x,twisty,z),
                                         primTopshear = let (_,y,z) = primTopshear info in (shearx,y,z) } }, rest)
updatePrimTypeTubeOld _ _ = fail "insufficient or incorrect parameters for deprecated prim type (PRIM_TYPE_TUBE)"

llGetObjectDetails _ [KVal k, LVal params] =
     -- TODO: this doesn't take into account multiple regions
     do  (getAvatarDetails k params <||> getPrimDetails k params) >>= continueWith . LVal
     where getAvatarDetails k params = 
               do  avs <- lift getWorldAvatars
                   av <- mlookup k avs
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
                   pos <- getObjectPosition k
                   rot <- getObjectRotation k
                   vel <- getObject k >>= return . objectVelocity . objectDynamics
                   return $ map (primq prim pos rot vel) params
               where primq prim pos rot vel i | i == llcObjectName = SVal $ primName prim
                                              | i == llcObjectDesc = SVal $ primDescription prim
                                              | i == llcObjectPos = vec2VVal $ pos
                                              | i == llcObjectRot = rot2RVal $ rot
                                              | i == llcObjectVelocity = vec2VVal $ vel
                                              | i == llcObjectOwner = KVal $ primOwner prim
                                              | i == llcObjectGroup = KVal nullKey            -- TODO: prim groups
                                              | i == llcObjectCreator = KVal $ primOwner prim -- TODO: prim creators
                                              | otherwise = llcObjectUnknownDetail

--------------------------------------------------------------------------------------------------------------
llAllowInventoryDrop info@(ScriptInfo _ _ _ k _) [IVal add] =
    (getPrim k >>= return . (\ p -> p { primAllowInventoryDrop = add /= 0 }) >>= lift . setPrim k >> 
                       lift (logFromScript info ("drop is now " ++ (if add == 0 then "not " else "") ++ "allowed"))) >>
        continueWith VoidVal
        
llAdjustSoundVolume info [FVal f] = lift $ do
    do if (f < 0.0 || f > 1.0) then logFromScript info ("llAdjustSoundVolume: volume " ++ show f ++ " out of range.")
                               else logFromScript info ("llAdjustSoundVolume: volume adjusted to " ++ show f)
       continueWith VoidVal
-----------------------------------------------------------------------------------------------------------------
-- Parcel related functions                                  

llSetParcelMusicURL info@(ScriptInfo _ _ _ pk _) [SVal url] =
     -- parcel music URL is a write-only value, so we won't worry about setting anything....
     do  -- make sure object owner == parcel owner
        (_,_,parcel) <- getPrimParcel pk
        owner <- getPrimOwner pk
        when (parcelOwner parcel /= owner) $ throwError ("prim doesn't have permission to modify parcel")
        lift $ logFromScript info ("setting music url for parcel " ++ parcelName parcel ++ " to " ++ url)
        continueWith VoidVal
        
llGetParcelFlags info@(ScriptInfo _ _ _ pk _) [VVal x y z] =
    do  regionIndex <- getPrimRegion pk
        (_,parcel) <- getParcelByPosition regionIndex (x,y,z)
        continueWith $ IVal $ parcelFlags parcel
   
llGetParcelDetails info@(ScriptInfo _ _ _ pk _) [VVal x y z, LVal details] =
     do  regionIndex <- getPrimRegion pk
         (_,parcel) <- getParcelByPosition regionIndex (x,y,z)
         detailList <- mapM (getDetail parcel) details
         continueWith $ LVal [ d | Just d <- detailList]
     where getDetail parcel (IVal detail) | detail == cParcelDetailsName = return $ Just $ SVal $ parcelName parcel
                                          | detail == cParcelDetailsDesc =  return $ Just $ SVal $ parcelDescription parcel
                                          | detail == cParcelDetailsOwner = return $ Just $ KVal $ parcelOwner parcel
                                          | detail == cParcelDetailsGroup = return $ Just $ KVal nullKey
                                          | detail == cParcelDetailsArea = return $ Just $ FVal $ fromIntegral $ 
                                             let (bot,top,left,right) = parcelBoundaries parcel in (top - bot) * (right - left) 
                                          | otherwise = lift $ logFromScript info ("llGetParcelDetails: invalid detail flag: " ++ show detail) >> return Nothing
           getDetail _ v = lift $ logFromScript info ("llGetParcelDetails: invalid detail: " ++ lslValString v) >> return Nothing

whenParcelPermitted info pk action= do
    (regionIndex,parcelIndex,parcel) <- getPrimParcel pk
    prim <- getPrim pk
    if parcelOwner parcel /= primOwner prim then lift $ logFromScript info "prim not permitted to change parcel"
        else action regionIndex parcelIndex parcel >>= putParcel regionIndex parcelIndex
        
addToLandACLList aclType aclFromParcel aclIntoParcel info@(ScriptInfo _ _ _ pk _) [KVal ak,FVal duration] = do
    do
        whenParcelPermitted info pk $ \ regionIndex parcelIndex parcel -> do
            lift $ runAndLogIfErr ("attempt to change " ++ aclType ++ " unknown avatar " ++ ak) parcel $ do
                avs <- lift getWorldAvatars
                _ <- mlookup ak avs
                when (duration < 0.0) $ lift $ logFromScript info (aclType ++ " change attempted with invalid duration: " ++ show duration)
                t <- lift getTick
                let acl = (ak, if duration == 0 then Nothing else Just $ t + durationToTicks duration)
                let acllist = acl : ([ b | b@(k,Just expire) <- aclFromParcel parcel,expire < t, k /= ak] ++
                                     [ b | b@(k,Nothing) <- aclFromParcel parcel, k /= ak])
                let parcel' = aclIntoParcel parcel acllist
                lift $ logFromScript info ("added " ++ ak ++ " to " ++ aclType ++ " list for parcel in " ++ (show regionIndex))
                return parcel'
    continueWith VoidVal
            
removeFromLandACLList aclFromParcel aclIntoParcel info@(ScriptInfo _ _ _ pk _) ak = do
    whenParcelPermitted info pk $ \ regionIndex parcelIndex parcel -> do
        avs <- lift getWorldAvatars
        mlookup ak avs `catchError` (\ _ -> throwError ("unkown avatar " ++ ak))
        let acl = aclFromParcel parcel
        return $ aclIntoParcel parcel [ ac | ac@(k,_) <- acl, k /= ak ]

        
llResetLandBanList info@(ScriptInfo _ _ _ pk _) [] =
    do  whenParcelPermitted info pk (\ regionIndex parcelIndex parcel -> return $ parcel { parcelBanList = [] })
        continueWith VoidVal
llResetLandPassList info@(ScriptInfo _ _ _ pk _) [] =
    do  whenParcelPermitted info pk  (\ regionIndex parcelIndex parcel -> return $ parcel { parcelPassList = [] })
        continueWith VoidVal

llRemoveFromLandPassList info [KVal ak] =
    let aclFromParcel = parcelPassList
        aclIntoParcel = (\ parcel list -> parcel { parcelPassList = list})
    in do removeFromLandACLList aclFromParcel aclIntoParcel info ak
          lift $ logFromScript info ("llRemovFromLandPassList: removed " ++ ak)
          continueWith VoidVal

llRemoveFromLandBanList info [KVal ak] =
    let aclFromParcel = parcelBanList
        aclIntoParcel = (\ parcel list -> parcel { parcelBanList = list})
    in do removeFromLandACLList aclFromParcel aclIntoParcel info ak
          lift $ logFromScript info ("llRemovFromLandBanList: removed " ++ ak)
          continueWith VoidVal
              
llAddToLandPassList info args = 
    let aclFromParcel = parcelPassList
        aclIntoParcel = (\ parcel list -> parcel { parcelPassList = list })
    in addToLandACLList "pass" aclFromParcel aclIntoParcel info args
llAddToLandBanList info args =
    let aclFromParcel = parcelBanList
        aclIntoParcel = (\ parcel list -> parcel { parcelBanList = list })
    in addToLandACLList "ban" aclFromParcel aclIntoParcel info args
    
llGetLandOwnerAt info@(ScriptInfo _ _ _ pk _) [VVal x y z] = 
    do  regionIndex <- getPrimRegion pk
        getParcelByPosition regionIndex (x,y,z) >>= continueWith . KVal . parcelOwner . snd
    
llOverMyLand info@(ScriptInfo _ _ _ pk _) [KVal ak] =
    do  regionIndex <- getPrimRegion pk
        owner <- getPrimOwner pk
        avatars <- lift getWorldAvatars
        i <- case M.lookup ak avatars of
            Nothing -> lift $ logFromScript info "llOverMyLand: no such avatar" >> return 0
            Just av | avatarRegion av /= regionIndex -> lift $ logFromScript info "llOverMyLand: avatar not in sim" >> return 0
                    | otherwise -> do
                        parcel <- getParcelByPosition regionIndex (avatarPosition av)
                        return $ if owner == (parcelOwner . snd) parcel then 1 else 0
        continueWith (IVal i)
-------------------------------------------------------------------------------
-- EMAIL
llGetNextEmail info@(ScriptInfo _ _ sn pk _) [SVal addr, SVal subj] =
   do  emails <- getPrimPendingEmails pk
       case find match emails of
           Nothing -> return ()
           Just email -> do
                lift $ pushDeferredScriptEvent (Event "email" [SVal $ show $ emailTime email, SVal $ emailAddress email, SVal $ emailSubject email,
                                                SVal $ emailMessage email, IVal (length emails - 1)] M.empty) pk sn 0
                lift $ setPrimPendingEmails pk (filter (not . match) emails)
       continueWith VoidVal
   where match email = (addr == "" || addr == emailAddress email) &&
                       (subj == "" || subj == emailSubject email)

llEmail info@(ScriptInfo _ _ _ pk _) [SVal address, SVal subject, SVal message] =
    do  let suffix = "@lsl.secondlife.com" 
        let logit = lift $ logFromScript info ("llEmail: sending email to " ++ address)
        if suffix `isSuffixOf` address
           then let potentialKey = take (length address - length suffix) address in
               do pending <- (getPrimPendingEmails potentialKey) <||> throwError ("LSL destination addresses unknown prim")
                  time <- lift $ getUnixTime
                  lift $ setPrimPendingEmails pk (pending ++ [Email subject address message time])
                  logit
           else logit
        continueWith VoidVal -- should do a yield til... (20 second delay!)
-------------------------------------------------------------------------------
soundExists pk sound = do
    sounds <- getPrimSounds pk
    case findByInvName sound sounds of
        Nothing -> do
            result <- lift $ findAsset sound
            case result of
                Nothing -> return False
                Just v -> return $ if isSoundAsset v then True else False
        Just _ -> return True
       
llTriggerSound info@(ScriptInfo _ _ _ pk _) [SVal sound, FVal volume] =
    do  found <- soundExists pk sound
        let message = if found then "llTriggerSound: triggering sound " ++ sound else "llTriggerSound: sound not found - " ++ sound
        lift $ logFromScript info message
        continueWith VoidVal
        
llTriggerSoundLimited info@(ScriptInfo _ _ _ pk _) [SVal sound, FVal volume, VVal t n e, VVal b s w] =
    do  when (t <= b || n <= s || e <= w) $ lift $ logFromScript info ("llTriggerSoundLimited: bounding box has zero or negative volume")
        found <- soundExists pk sound
        let message = if found then "llTriggerSoundLimited: triggering sound " ++ sound else "llTriggerSoundLimited: sound no found - " ++ sound
        lift $ logFromScript info message
        continueWith VoidVal
    
llSound info@(ScriptInfo _ _ _ pk _) [SVal sound, FVal vol, IVal q, IVal loop] =
    do  found <- soundExists pk sound
        let message = if found then "llSound (deprecated): playing sound " ++ sound else "llSound (deprecated): sound not found - " ++ sound
        lift $ logFromScript info message
        continueWith VoidVal

llPlaySound info@(ScriptInfo _ _ _ pk _) [SVal sound, FVal vol] =
    do
        found <- soundExists pk sound
        let message = if found then "llPlaySound: playing sound " ++ sound else "llPlaySound: sound not found - " ++ sound
        lift $ logFromScript info message
        continueWith VoidVal
    
llCollisionSound info@(ScriptInfo _ _ _ pk _) [SVal sound, FVal vol] =
    do  found <- soundExists pk sound
        let message = if found then "llCollisionSound: setting collision sound to " ++ sound else "llCollisionSound: sound not found - " ++ sound
        lift $ logFromScript info message
        continueWith VoidVal
    
llPreloadSound info@(ScriptInfo _ _ _ pk _) [SVal sound] =
    do  found <- soundExists pk sound
        let message = if found then "llPreloadSound: loading sound " ++ sound else "llPreloadSound: sound not found - " ++ sound
        lift $ logFromScript info message
        continueWith VoidVal
    
llSoundPreload info@(ScriptInfo _ _ _ pk _) [SVal sound] =
    do  found <- soundExists pk sound
        let message = if found then "llSoundPreload: loading sound " ++ sound else "llSoundPreload: sound not found - " ++ sound
        lift $ logFromScript info message
        continueWith VoidVal
    
llPlaySoundSlave info@(ScriptInfo _ _ _ pk _) [SVal sound, FVal vol] =
    do  found <- soundExists pk sound
        let message = if found then "llPlaySoundSlave: playing sound " ++ sound else "llPlaySoundSlave: sound not found - " ++ sound
        lift $ logFromScript info message
        continueWith VoidVal

llLoopSoundSlave info@(ScriptInfo _ _ _ pk _) [SVal sound, FVal vol] =
    do  found <- soundExists pk sound
        let message = if found then "llLoopSoundSlave: playing sound " ++ sound else "llLoopSoundSlave: sound not found - " ++ sound
        lift $ logFromScript info message
        continueWith VoidVal

llLoopSound info@(ScriptInfo _ _ _ pk _) [SVal sound, FVal vol] =
    do  found <- soundExists pk sound
        let message = if found then "llLoopSound: playing sound " ++ sound else "llLoopSound: sound not found - " ++ sound
        lift $ logFromScript info message
        continueWith VoidVal

llLoopSoundMaster info@(ScriptInfo _ _ _ pk _) [SVal sound, FVal vol] =
    do  found <- soundExists pk sound
        let message = if found then "llLoopSoundMaster: playing sound " ++ sound else "llLoopSoundMaster: sound not found - " ++ sound
        lift $ logFromScript info message
        continueWith VoidVal

llStopSound info [] = lift $ logFromScript info "llStopSound: stopping sound" >> continueWith VoidVal
llSetSoundRadius info [FVal radius] = 
    lift $ logFromScript info "llSetSoundRadius: called. In real sim, this function has no effect." >> continueWith VoidVal
llSetSoundQueueing info [IVal bool] = 
    lift $ logFromScript info ("llSetSoundQueuiing: set to " ++ (if bool /= 0 then "TRUE" else "FALSE")) >> continueWith VoidVal

------------------------------------------------------------------------------
llCollisionSprite info@(ScriptInfo _ _ _ pk _) [SVal impactSprite] =
    do  tk <- findTexture pk impactSprite
        lift $ logFromScript info ("llCollisionSprite: set sprite to " ++ tk)
        continueWith VoidVal
--

llGetKey (ScriptInfo _ _ _ pk _) [] = continueWith $ KVal pk

llGetOwnerKey info@(ScriptInfo _ _ _ pk _) [KVal k] = 
    do  regionIndex <- getPrimRegion pk
        mRegionIndex  <- fromErrorT Nothing (getPrimRegion k >>= return . Just)
        key <- case mRegionIndex of 
            Nothing -> (lift $ logFromScript info "llGetOwnerKey: object key not found") >> return k
            Just regionIndex' | regionIndex /= regionIndex' -> (lift $ logFromScript info "llGetOwnerKey: object in different simulator") >> return k
                              | otherwise -> getPrimOwner k
        continueWith (KVal key)
    
llGetLinkNumber (ScriptInfo oid pid _ pk _) [] =
     if pid /= 0 then continueWith (IVal $ pid + 1) else
         do  LSLObject { primKeys = links } <- getObject oid
             continueWith $ IVal (if length links == 1 then 0 else 1)
        
llGetUnixTime (ScriptInfo _ _ _ _ _) [] = 
    lift (getUnixTime >>= continueWith . IVal)

llGetTimestamp (ScriptInfo _ _ _ _ _) [] = lift $ do
    cal <- getTimeOfDay >>= return . toUTCTime
    continueWith $ SVal $ printf "%04d-%02d-%02dT%02d:%02d:%02.6f"
         (ctYear cal) (1 + (fromEnum $ ctMonth cal)) (ctDay cal) (ctHour cal) (ctMin cal) 
         (((fromIntegral $ ctSec cal) / (10.0^12 * (fromIntegral $ ctPicosec cal))) :: Float)
         
llGetDate (ScriptInfo _ _ _ _ _) [] = lift (getUTCDate >>= continueWith . SVal)
llGetGMTclock (ScriptInfo _ _ _ _ _) [] = lift $ do
    cal <- getTimeOfDay >>= return . toUTCTime
    continueWith $ IVal $ (24 * ctHour cal) + (60 * ctMin cal) + (ctSec cal)
    
llGetWallclock (ScriptInfo _ _ _ _ _) [] = lift $ do
    cal <- getLocalTimeOfDay (-8) >>= return . toUTCTime
    continueWith $ IVal $ (24 * ctHour cal) + (60 * ctMin cal) + (ctSec cal)
llGetTimeOfDay (ScriptInfo _ _ _ _ _) [] = lift $ do
    t <- getTick
    continueWith $ FVal $ (ticksToDuration (t `mod` durationToTicks 4.0))
getUnixTime :: (Monad (StateT (World a) a), Monad a) => StateT (World a) a Int
getUnixTime = (liftM2 (+) getWorldZeroTime (getTick >>= return . floor . ticksToDuration)) >>= return . fromIntegral
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
     do  t <- lift getTick
         script <- lift getWorldScripts >>= mlookup (pk,sid)
         continueWith $ FVal $ ticksToDuration (t - scriptLastResetTick script)

getAndResetTick pk sid =
    do
        scripts <- lift getWorldScripts
        script <- mlookup (pk,sid) scripts
        let t = scriptLastResetTick script
        t' <- lift getTick
        lift $ setWorldScripts (M.insert (pk,sid) (script { scriptLastResetTick = t' } ) scripts)
        return $ t' - t
    
llGetAndResetTime (ScriptInfo _ _ sid pk _) [] =
    getAndResetTick pk sid >>= continueWith . FVal . ticksToDuration
    
llResetTime (ScriptInfo _ _ sid pk _) [] =
    getAndResetTick pk sid >> continueWith VoidVal
    
llSetText info [SVal text, VVal r g b, FVal alpha] = lift $ setText "llSetText" 254 info text
llSetSitText info [SVal text] = lift $ setText "llSetSitText" 9 info text
llSetTouchText info [SVal text] = lift $ setText "llSetTouchText" 9 info text

setText func lim info text =
    do logFromScript info (func ++ ": setting text to " ++ text)
       when (length text > lim) $ logFromScript info (func ++ ": text exceeds " ++ show lim ++ " character limit")
       continueWith VoidVal

llGetScriptName (ScriptInfo _ _ sid _ _) [] = continueWith (SVal sid)

llGetNumberOfNotecardLines (ScriptInfo _ _ sn pk _) [SVal name] =
    do  notecards <- getPrimNotecards pk
        key <- case find ((name==) . inventoryItemName) notecards of
            Nothing -> (lift $ putChat (Just 20.0) pk 0 ("Couldn't find notecard " ++ name)) >> return nullKey
            Just notecard -> do
                key <- lift newKey
                lift $ pushDataserverEvent pk sn key (show $ length $ invNotecardLines $ inventoryItemData notecard)
                return key
        continueWith (KVal key)
    
llGetNotecardLine (ScriptInfo _ _ sn pk _) [SVal name, IVal lineNumber] =
    do  notecards <- getPrimNotecards pk
        key <- case find ((name==) . inventoryItemName) notecards of
            Nothing -> (lift $ putChat (Just 20.0) pk cDebugChannel ("Couldn't find notecard " ++ name)) >> return nullKey
            Just notecard -> do
                key <- lift newKey
                let line = case lookupByIndex lineNumber $ invNotecardLines $ inventoryItemData notecard of
                        Nothing -> cEOF
                        Just v -> take 255 v
                lift $ pushDataserverEvent pk sn key line
                return key
        continueWith (KVal key)
 
llRequestInventoryData info@(ScriptInfo _ _ sn pk _) [SVal name] = do
    do t <- lift getTick
       landmarks <- getPrimLandmarks pk
       landmark <- maybe (throwError ("no landmark named " ++ name)) (\ l -> return l) (findByInvName name landmarks)
       let (_,v) = invLandmarkLocation $ inventoryItemData landmark
       key <- lift newKey
       lift $ pushDataserverEvent pk sn key (lslValString (vec2VVal v))
       return (YieldTil $ t + durationToTicks 1,KVal key)
    
llGetLinkName (ScriptInfo oid _ _ _ _) [IVal linkNumber] =
    do  LSLObject { primKeys = links } <- getObject oid
        name <- if length links == 0
           then if linkNumber == 0
               then getPrimName oid
               else return nullKey
           else case lookupByIndex (linkNumber - 1) links of
                Nothing -> return nullKey
                Just k -> getPrimName k
        continueWith (SVal name)
    
llGetStatus (ScriptInfo _ _ _ pk _) [IVal check] =
    do  status <- getPrimStatus pk
        continueWith $ IVal $ case mssb check of
            Nothing -> 0
            Just b -> if bit b .&. status /= 0 then 1 else 0
    where mssb i = foldl' (\ x y -> x `mplus` (if testBit i y then Just y else Nothing)) Nothing [31,30..0]

llSetStatus info@(ScriptInfo _ _ _ pk _) [IVal mask, IVal val] =
    do  status <- getPrimStatus pk
        let status' = if val == 0 
                          then status .&. (complement mask)
                          else status .|. mask
        lift $ setPrimStatus pk status'
        continueWith VoidVal
    
llPassTouches info@(ScriptInfo _ _ _ pk _) [IVal val] =
   lift $ setPrimPassTouches pk (if val /= 0 then True else False) >> continueWith VoidVal
   
llPassCollisions info@(ScriptInfo _ _ _ pk _) [IVal val] =
   lift $ setPrimPassCollisions pk (if val /= 0 then True else False) >> continueWith VoidVal
   
llGetRegionCorner (ScriptInfo _ _ _ pk _) [] = 
    do (x,y) <- getPrimRegion pk
       continueWith $ vec2VVal (256 * fromIntegral x, 256 * fromIntegral y, 0)

llGetRegionFlags info@(ScriptInfo _ _ _ pk _) [] =
    getPrimRegion pk >>= getRegion >>= return . regionFlags >>= continueWith . IVal
    
llGetSimulatorHostname (ScriptInfo _ _ _ pk _) [] =
    do (i,j) <- getPrimRegion pk
       (continueWith . SVal) (show i ++ "x" ++ show j ++ ".example.com")

llGetRegionName (ScriptInfo _ _ _ pk _) [] = 
    do  regionIndex <- getPrimRegion pk
        regions <- lift getWorldRegions
        region <- mlookup regionIndex regions
        continueWith $ SVal $ regionName region

llGetAgentSize info@(ScriptInfo _ _ _ pk _) [KVal ak] =
    do  region <- getPrimRegion pk
        avatars <- lift getWorldAvatars
        size <- case M.lookup ak avatars of
            Nothing -> lift $ logFromScript info ("llGetAgentSize: no such agent - " ++ ak) >> return (0,0,0)
            Just av -> if avatarRegion av == region 
                           then return $ (0.45,0.6,avatarHeight av)
                           else lift $ logFromScript info ("llGetAgentSize: agent not in sim") >> return (0,0,0)
        continueWith (vec2VVal size)
                          
llGetAgentInfo info@(ScriptInfo _ _ _ pk _) [KVal ak] =
    do  region <- getPrimRegion pk
        avatars <- lift getWorldAvatars
        i <- case M.lookup ak avatars of
            Nothing -> lift $ logFromScript info ("llGetAgentInfo: no such agent - " ++ ak) >> return 0
            Just av -> if avatarRegion av == region then return $ avatarState av
                                                    else lift $ logFromScript info ("llGetAgentInfo: agent not in sim") >> return 0
        continueWith (IVal i)
        
-- llDetected* functions ------------------------------------------------------
-- TODO: only setup for 1 detected thing at a time...
-- This is a problem especially for sensor testing

llDetectedKey (ScriptInfo _ _ _ pk mevent) [IVal i] =
    continueWith $ maybe (KVal nullKey) id $ do
        event <- mevent
        M.lookup ("key_" ++ show i) (eventInfo event)

llDetectedLinkNumber (ScriptInfo _ _ _ pk mevent) [IVal i] =
    continueWith $ maybe (IVal 0) id (mevent >>= M.lookup ("integer_" ++ (show i)) . eventInfo)
    
llDetectedGrab (ScriptInfo _ _ _ _ mevent) [IVal i] =
    continueWith $ maybe (VVal 0 0 0) id (mevent >>= M.lookup ("vector_" ++ show i) . eventInfo)
    
llDetectedPos (ScriptInfo _ _ _ _ mevent) [IVal i] =
    do  avatars <- lift getWorldAvatars
        Just event <- return mevent
        KVal key <- mlookup ("key_" ++ show i) (eventInfo event)
        v <- case M.lookup key avatars of
            Just av -> return $ avatarPosition av
            Nothing -> getRootPrim key >>= getObjectPosition
        continueWith (vec2VVal v)
    
llDetectedRot (ScriptInfo _ _ _ _ mevent) [IVal i] =
    do
        avatars <- lift getWorldAvatars
        Just event <- return mevent
        KVal key <- mlookup ("key_" ++ show i) (eventInfo event)
        r <- case M.lookup key avatars of
            Just av -> return $ avatarRotation av
            Nothing -> getRootPrim key >>= getObjectRotation
        continueWith (rot2RVal r)
    
llDetectedName (ScriptInfo _ _ _ _ mevent) [IVal i] =
    do  avatars <- lift getWorldAvatars
        Just event <- return mevent
        KVal key <- mlookup ("key_" ++ show i) (eventInfo event)
        name <- case M.lookup key avatars of
            Just av -> return $ avatarName av
            Nothing -> getPrimName key
        continueWith (SVal name)
   
llDetectedOwner (ScriptInfo _ _ _ _ mevent) [IVal i] =
    do  avatars <- lift getWorldAvatars
        Just event <- return mevent
        KVal key <- mlookup ("key_" ++ show i) (eventInfo event)
        k <- case M.lookup key avatars of
            Just av -> return key
            Nothing -> getPrimOwner key
        continueWith (KVal k)

llDetectedGroup info@(ScriptInfo _ _ _ _ mevent) [IVal i] =
    do  Just event <- return mevent
        KVal key <- mlookup ("key_" ++ show i) (eventInfo event) <||> throwError ("index " ++ show i ++ " out of range")
        avatars <- lift getWorldAvatars
        prims <- lift getPrims
        k <- (mlookup key avatars >>= return . fromMaybe nullKey . avatarActiveGroup) <||>
            (mlookup key prims >>= return . fromMaybe nullKey . primGroup) <||> throwError ("error: key not an avatar or an object!")
        continueWith (KVal k)
    
llDetectedVel _ [IVal i] = continueWith $ VVal 0 0 0 -- TODO: physics!!!

llDetectedType (ScriptInfo _ _ _ _ mevent) [IVal i] =
    do  Just event <- return mevent
        KVal key <- mlookup ("key_" ++ show i) (eventInfo event)
        avatars <- lift getWorldAvatars
        t <- case M.lookup key avatars of
            Just _ -> return cAgent
            Nothing -> determineActivePassiveScripted key
        continueWith (IVal t)
    
determineActivePassiveScripted key = do
            status <- getPrimStatus key
            case status of
               st | st .&. cStatusPhysics /= 0 -> return cActive
                  | otherwise -> do
                      scripts <- getPrimScripts key
                      return (if length scripts > 0 then cScripted else cPassive)

-------------------------------------------------------------------------------
-- animation functions

llGetAnimationList info@(ScriptInfo _ _ _ pk _) [KVal ak] =
    do  rp <- getPrimRegion pk
        avatars <- lift getWorldAvatars
        list <- case M.lookup ak avatars of
            Nothing -> lift $ logFromScript info ("llGetAnimationList: no such avatar: " ++ ak) >> return []
            Just av | avatarRegion av /= rp -> lift $ logFromScript info ("llGetAnimationList: avatar not in sim") >> return []
                    | otherwise -> do
                        now <- lift getTick
                        return [ KVal anim | (t,anim) <- avatarActiveAnimations av, maybe True (<now) t]
        continueWith (LVal list)
    
llGetAnimation info@(ScriptInfo _ _ _ pk _) [KVal ak] =
    do  rp <- getPrimRegion pk
        avatars <- lift getWorldAvatars
        nm <- case M.lookup ak avatars of
            Nothing -> lift $ logFromScript info ("llGetAnimation: no such avatar: " ++ ak) >> return ""
            Just av | avatarRegion av /= rp -> lift $ logFromScript info ("llGetAnimationList: avatar no in sim") >> return ""
                    | otherwise -> return "Standing" -- TODO: real animation state
        continueWith (SVal nm)
    
llStartAnimation info@(ScriptInfo _ _ sid pk _) [SVal anim] =
    do   avKey <- getPermittedAvKey info "llStartAnimation" cPermissionTriggerAnimation
         case avKey of
             Nothing -> return ()
             Just ak -> do
                avatars <- lift getWorldAvatars
                av <- mlookup ak avatars
                let anims = avatarActiveAnimations av
                now <- lift getTick
                result <- findAnimation pk anim
                case result of
                    Just (key,mduration) -> do
                        let anims' = updateAnims anims now mduration key
                        lift $ setWorldAvatars (M.insert ak (av { avatarActiveAnimations = anims' }) avatars)
                    Nothing -> lift $ logFromScript info ("llStartAnimation: animation not found - " ++ anim)
                where updateAnims anims now mduration key =
                          (expire,key):[(t,k) | (t,k) <- anims, (maybe True (<now) t) && key /= k]
                              where expire = fmap ((now+) . durationToTicks) mduration
         continueWith VoidVal

findAnimation pk anim = 
    do primAnims <- getPrimAnimations pk
       case find (\ item -> invName item == anim) primAnims of
           Just item -> do
             return $ Just (snd $ inventoryItemNameKey $ inventoryItemIdentification item, invAnimationDuration $ inventoryItemData item)
           Nothing ->
             case find (\ (name,_,_) -> name == anim) builtInAnimations of
                 Just (_,key,mduration) -> return $ Just (key,mduration)
                 Nothing -> return Nothing
                 
llStopAnimation info@(ScriptInfo _ _ sid pk _) [SVal anim] =
    do  avKey <- getPermittedAvKey info "llStartAnimation" cPermissionTriggerAnimation
        case avKey of
            Nothing -> return ()
            Just ak -> do
                avatars <- lift getWorldAvatars
                av <- mlookup ak avatars
                let anims = avatarActiveAnimations av
                result <- findAnimation pk anim
                case result of
                    Just (key,_) -> do
                       case find (\ (_,k) -> k == key) anims of
                           Nothing -> lift $ logFromScript info "llStopAnimation: animation not active"
                           Just _ -> do
                               let anims' = [(expire,k) | (expire,k) <- anims, k /= key]
                               lift $ setWorldAvatars $ M.insert ak (av { avatarActiveAnimations = anims' }) avatars
                    Nothing -> lift $ logFromScript info ("llStopAnimation: animation not found - " ++ anim)
        continueWith VoidVal

llSetClickAction info [IVal action] =
    lift $ (if action `elem` cClickActions then logFromScript info "llSetClickAction: setting click action"
                                           else logFromScript info "llSetClickAction: invalid click action") >> continueWith VoidVal
-------------------------------------------------------------------------------    
-- XML RPC related functions
llCloseRemoteDataChannel info@(ScriptInfo _ _ sn pk _) [KVal key] =
   do  -- according to LSL wiki this function has no effect,
       -- so we'll just log a message indicating whether or not the passed in key is valid
       (lookupScriptFromChan key) <||> throwError "key not an open channel"
       return ()
       continueWith VoidVal
   
llOpenRemoteDataChannel info@(ScriptInfo _ _ sn pk _) [] =
    do key <- lookupDataChannel (pk,sn) <||> lift newKey
       insertScriptChannelPair (pk,sn) key
       lift $ logFromScript info ("pushing remote data channel to script")
       lift $ pushDeferredScriptEvent (Event "remote_data" [llcRemoteDataChannel, KVal key, KVal nullKey, SVal "", IVal 0, SVal ""] M.empty) pk sn 0
       continueWith VoidVal
       
llSendRemoteData info [KVal channel, SVal dest, IVal idata, SVal sdata] =
    do  -- according to LSL Wiki, this function doesn't do anything... but we'll check
        -- for a valid channel and log a message.
        lookupScriptFromChan channel <||> throwError "key not an open channel"
        lift newKey >>= continueWith . KVal
    
llRemoteDataReply info@(ScriptInfo _ _ _ _ mevent) [KVal channel, KVal messageId, SVal sdata, IVal idata] =
    do  Just event <- return mevent
        when (eventName event /= "remote_data") $ throwError "no effect unless called within remote_data handler"
        let m = eventInfo event
        (KVal k) <- mlookup "requestKey" m <||> throwError ("problem: invalid or mistyped request key in event")
        lift $ putWorldEvent 0 (XMLReplyEvent k channel messageId sdata idata)
        lift $ logFromScript info ("llRemoteDataReply: (" ++ show channel ++ "," ++ show messageId ++ "," ++ show sdata ++ "," ++
                                                             show idata ++ ")")
        continueWith VoidVal
    
llRemoteDataSetRegion info [] = lift $ logFromScript info "llRemoteDataSetRegion: this function has no effect" >> continueWith VoidVal
-------------------------------------------------------------------------------

llDialog info@(ScriptInfo _ _ _ pk _) [KVal ak, SVal message, LVal buttons, IVal channel] =
    do  when (length message > 512) $ doErr "message too long, must be less than 512 characters"
        when (null message) $ doErr "must supply a message"
        when (not $ all (==LLString) $ map typeOfLSLValue buttons) $ doErr "button list must contain only strings"
        let buttons' = take 12 $ map ( \ (SVal s) -> s) buttons
        when (any ((24 <) . length) buttons') $ doErr "Button Labels cannot have more that 24 characters"
        when (any null buttons') $ doErr "all buttons must have label strings"
        (lift getWorldAvatars >>= mlookup ak) <||> throwError ("no such agent/avatar - " ++ ak)
        lift $ putWorldEvent 0 (DialogEvent ak message buttons' channel pk) -- deprecated!
        lift $ putWorldEvent 0 (AvatarInputEvent ak (AvatarDialog message buttons' channel pk))
        continueWith VoidVal
    where doErr msg = (lift $ putChat (Just 20) pk cDebugChannel ("llDialog: " ++ msg)) >> throwError msg
-------------------------------------------------------------------------------    
-- old (deprecated) functions for visual effects

llMakeExplosion info _ = lift $ logFromScript info "llMakeExplosion: deprecated" >> continueWith VoidVal
llMakeSmoke info _ = lift $ logFromScript info "llMakeSmoke: deprecated" >> continueWith VoidVal
llMakeFire info _ = lift $ logFromScript info "llMakeFire: deprecated" >> continueWith VoidVal
llMakeFountain info _ = lift $ logFromScript info "llMakeFountain: deprecated" >> continueWith VoidVal
    
--------------------------------------------------------------------------------
llTakeControls info@(ScriptInfo {scriptInfoPrimKey = pk, scriptInfoScriptName = sn}) [IVal controls, IVal accept, IVal pass] =
    do  allScripts <- lift getWorldScripts
        script <- mlookup (pk,sn) allScripts
        case scriptLastPerm script of
            Nothing -> lift $ logFromScript info "no persmission to take controls from any agent"
            Just ak -> do
                    av <- getWorldAvatar ak
                    maybe (throwError $ "no permission to take controls from " ++ avatarName av)
                          (\ perms -> if perms .&. cPermissionTakeControls /= 0 
                                          then when (accept /= 0) $ lift $ setWorldAvatar ak (av { avatarControlListener = Just acl })
                                          else (throwError $ "no permission to take controls from " ++ avatarName av))
                          (M.lookup ak $ scriptPermissions script)
                 where acl = AvatarControlListener { avatarControlListenerMask = controls, avatarControlListenerScript = (pk,sn) }
        continueWith VoidVal
--------------------------------------------------------------------------------

llVolumeDetect info@(ScriptInfo { scriptInfoObjectKey = oid }) [IVal i] =
    do  obj <- getObject oid
        lift $ setObject oid (obj { objectDynamics = (objectDynamics obj) { objectVolumeDetect = i /= 0 }})
        continueWith VoidVal
    
llCollisionFilter info@(ScriptInfo { scriptInfoPrimKey = pk, scriptInfoScriptName = sn }) [SVal name, KVal k, IVal i] =
    do  allScripts <- lift getWorldScripts
        script <- mlookup (pk,sn) allScripts
        lift $ setWorldScripts (M.insert (pk,sn) (script { scriptCollisionFilter = (name,k, i /= 0) }) allScripts)
        continueWith VoidVal

llTarget info@(ScriptInfo { scriptInfoPrimKey = pk, scriptInfoScriptName = sn}) [VVal x y z, FVal range] =
    do  allScripts <- lift getWorldScripts
        script <- mlookup (pk,sn) allScripts
        let index = scriptTargetIndex script
        let script' = script {scriptTargetIndex = index + 1,
                              scriptPositionTargets = IM.insert index ((x,y,z),range) (scriptPositionTargets script)
                             }
        lift $ setWorldScripts (M.insert (pk,sn) script' allScripts)
        continueWith $ IVal index

llTargetRemove info@(ScriptInfo { scriptInfoPrimKey = pk, scriptInfoScriptName = sn}) [IVal tnumber] =
    do  allScripts <- lift getWorldScripts
        script <- mlookup (pk,sn) allScripts
        let script' = script { scriptPositionTargets = IM.delete tnumber (scriptPositionTargets script) }
        lift $ setWorldScripts (M.insert (pk,sn) script' allScripts)
        continueWith VoidVal

llRotTarget info@(ScriptInfo { scriptInfoPrimKey = pk, scriptInfoScriptName = sn}) [RVal x y z s, FVal err] = 
    do  allScripts <- lift getWorldScripts
        script <- mlookup (pk,sn) allScripts
        let index = scriptTargetIndex script
        let script' = script { scriptTargetIndex = index + 1,
                               scriptRotationTargets = IM.insert index ((x,y,z,s),err) (scriptRotationTargets script)
                             }
        lift $ setWorldScripts (M.insert (pk,sn) script' allScripts)
        continueWith VoidVal
    
llRotTargetRemove info@(ScriptInfo { scriptInfoPrimKey = pk, scriptInfoScriptName = sn}) [IVal tnumber] =
    do  allScripts <- lift getWorldScripts
        script <- mlookup (pk,sn) allScripts
        let script' = script { scriptRotationTargets = IM.delete tnumber (scriptRotationTargets script) }
        lift $ setWorldScripts (M.insert (pk,sn) script' allScripts)
        continueWith VoidVal

--------------------------------------------------------------------------------
llGround info [VVal _ _ _] = continueWith (FVal 0) -- the world is flat!
llGroundContour info [VVal _ _ _] = continueWith (VVal 1 0 0) -- could be any vector
llGroundNormal info [VVal _ _ _] = continueWith (VVal 0 0 1) -- straight up!
llGroundSlope info [VVal _ _ _] = continueWith (VVal 0 1 0)
--------------------------------------------------------------------------------
llGetSunDirection info [] = lift $ do
    az <- getTick >>= return . (*(pi/7200)) . ticksToDuration
    let el = 80 * pi 
    continueWith $ vec2VVal (sin az * cos el, sin el, cos az * cos el)
    
--------------------------------------------------------------------------------
llLoadURL info [KVal ak, SVal msg, SVal url] = do
    getWorldAvatar ak
    lift $ putWorldEvent 0 (AvatarInputEvent ak (AvatarLoadURL msg url))
    continueWith VoidVal
    
llMapDestination info [SVal simName, (VVal x y z), (VVal _ _ _)] = do
        case mevent of
           Nothing -> do
               attachment <- getPrimAttachment oid
               case attachment of
                   Nothing -> lift $ logFromScript info ("prim neither attached nor touched, so call to llMapDestination is not valid")
                   Just (Attachment { attachmentKey = ak }) -> putIt ak
           Just (Event "touch" _ m) ->
               case M.lookup "key_0" m of
                   Just (SVal ak) -> putIt ak
                   _ -> throwError "invalid touch event!"
        continueWith VoidVal
    where oid = scriptInfoObjectKey info
          mevent = scriptInfoCurrentEvent info
          putIt ak = lift $ putWorldEvent 0 (AvatarInputEvent ak (AvatarMapDestination simName (x,y,z)))
--------------------------------------------------------------------------------
continueWith val = return (EvalIncomplete,val)

defaultPredefs :: Monad m => M.Map String (PredefFunc m)
defaultPredefs = M.fromList $ map (\(x,y) -> (x, defaultPredef x y)) [
        ("llAddToLandBanList",llAddToLandBanList),
        ("llAddToLandPassList",llAddToLandPassList),
        ("llAdjustSoundVolume",llAdjustSoundVolume),
        ("llAllowInventoryDrop",llAllowInventoryDrop),
        ("llApplyImpulse",llApplyImpulse),
        ("llApplyRotationalImpulse", llApplyRotationalImpulse),
        ("llAttachToAvatar",llAttachToAvatar),
        ("llAvatarOnSitTarget",llAvatarOnSitTarget),
        ("llBreakAllLinks",llBreakAllLinks),
        ("llBreakLink",llBreakLink),
        ("llClearCameraParams",llClearCameraParams),
        ("llCloseRemoteDataChannel",llCloseRemoteDataChannel),
        ("llCloud",llCloud),
        ("llCollisionFilter", llCollisionFilter),
        ("llCollisionSound",llCollisionSound),
        ("llCollisionSprite",llCollisionSprite),
        ("llCreateLink",llCreateLink),
        ("llDetachFromAvatar", llDetachFromAvatar),
        ("llDetectedGrab",llDetectedGrab),
        ("llDetectedGroup",llDetectedGroup),
        ("llDetectedKey", llDetectedKey),
        ("llDetectedLinkNumber", llDetectedLinkNumber),
        ("llDetectedName", llDetectedName),
        ("llDetectedOwner", llDetectedOwner),
        ("llDetectedPos", llDetectedPos),
        ("llDetectedRot", llDetectedRot),
        ("llDetectedType", llDetectedType),
        ("llDetectedVel", llDetectedVel),
        ("llDialog", llDialog),
        ("llDie",llDie),
        ("llEjectFromLand",llEjectFromLand),
        ("llEmail",llEmail),
        ("llFrand",llFrand),
        ("llGetAccel",llGetAccel),
        ("llGetAgentInfo",llGetAgentInfo),
        ("llGetAgentSize",llGetAgentSize),
        ("llGetAlpha",llGetAlpha),
        ("llGetAndResetTime", llGetAndResetTime),
        ("llGetAnimation", llGetAnimation),
        ("llGetAnimationList",llGetAnimationList),
        ("llGetAttached",llGetAttached),
        ("llGetBoundingBox",llGetBoundingBox),
        ("llGetCameraPos",llGetCameraPos),
        ("llGetCameraRot",llGetCameraRot),
        ("llGetColor",llGetColor),
        ("llGetCreator",llGetCreator),
        ("llGetDate",llGetDate),
        ("llGetForce", llGetForce),
        ("llGetGeometricCenter",llGetGeometricCenter),
        ("llGetGMTclock",llGetGMTclock),
        ("llGetInventoryCreator",llGetInventoryCreator),
        ("llGetInventoryKey",llGetInventoryKey),
        ("llGetInventoryName",llGetInventoryName),
        ("llGetInventoryNumber",llGetInventoryNumber),
        ("llGetInventoryPermMask", llGetInventoryPermMask),
        ("llGetInventoryType",llGetInventoryType),
        ("llGetKey",llGetKey),
        ("llGetLocalPos",llGetLocalPos),
        ("llGetNextEmail",llGetNextEmail),
        ("llGetNumberOfPrims",llGetNumberOfPrims),
        ("llGetNumberOfSides",llGetNumberOfSides),
        ("llGetLinkKey",llGetLinkKey),
        ("llGetLinkName",llGetLinkName),
        ("llGetLinkNumber", llGetLinkNumber),
        ("llGetLocalRot", llGetLocalRot),
        ("llGetLandOwnerAt", llGetLandOwnerAt),
        ("llGetMass",llGetMass),
        ("llGetNotecardLine", llGetNotecardLine),
        ("llGetNumberOfNotecardLines",llGetNumberOfNotecardLines),
        ("llGetObjectDesc", llGetObjectDesc),
        ("llGetObjectDetails", llGetObjectDetails),
        ("llGetObjectMass", llGetObjectMass),
        ("llGetObjectName", llGetObjectName),
        ("llGetObjectPermMask", llGetObjectPermMask),
        ("llGetObjectPrimCount",llGetObjectPrimCount),
        ("llGetOmega",llGetOmega),
        ("llGetOwner", llGetOwner),
        ("llGetOwnerKey",llGetOwnerKey),
        ("llGetParcelFlags", llGetParcelFlags),
        ("llGetParcelDetails", llGetParcelDetails),
        ("llGetPermissions",llGetPermissions),
        ("llGetPermissionsKey",llGetPermissionsKey),
        ("llGetPos", llGetPos),
        ("llGetPrimitiveParams", llGetPrimitiveParams),
        ("llGetRegionCorner", llGetRegionCorner),
        ("llGetRegionFlags", llGetRegionFlags),
        ("llGetRegionFPS", llGetRegionFPS),
        ("llGetRegionName", llGetRegionName),
        ("llGetRegionTimeDilation", llGetRegionTimeDilation),
        ("llGetRot",llGetRot),
        ("llGetRootPosition",llGetRootPosition),
        ("llGetRootRotation",llGetRootRotation),
        ("llGetScale", llGetScale),
        ("llGetScriptName", llGetScriptName),
        ("llGetScriptState", llGetScriptState),
        ("llGetSimulatorHostname", llGetSimulatorHostname),
        ("llGetStartParameter",llGetStartParameter),
        ("llGetStatus",llGetStatus),
        ("llGetSunDirection",llGetSunDirection),
        ("llGetTexture", llGetTexture),
        ("llGetTextureOffset", llGetTextureOffset),
        ("llGetTextureRot", llGetTextureRot),
        ("llGetTextureScale", llGetTextureScale),
        ("llGetTime",llGetTime),
        ("llGetTimeOfDay",llGetTimeOfDay),
        ("llGetTimestamp",llGetTimestamp),
        ("llGetTorque",llGetTorque),
        ("llGetUnixTime",llGetUnixTime),
        ("llGetWallclock",llGetWallclock),
        ("llGetVel",llGetVel),
        ("llGiveInventory", llGiveInventory),
        ("llGiveMoney",llGiveMoney),
        ("llGround", llGround),
        ("llGroundContour", llGroundContour),
        ("llGroundNormal", llGroundNormal),
        ("llGroundRepel",llGroundRepel),
        ("llGroundSlope",llGroundSlope),
        ("llHTTPRequest",llHTTPRequest),
        ("llInstantMessage",llInstantMessage),
        ("llKey2Name", llKey2Name),
        ("llListRandomize",llListRandomize),
        ("llListen", llListen),
        ("llListenControl",llListenControl),
        ("llListenRemove",llListenRemove),
        ("llLoadURL",llLoadURL),
        ("llLookAt", llLookAt),
        ("llLoopSound",llLoopSound),
        ("llLoopSoundSlave",llLoopSoundSlave),
        ("llLoopSoundMaster", llLoopSoundMaster),
        ("llMakeExplosion",llMakeExplosion),
        ("llMakeFire",llMakeFire),
        ("llMakeFountain",llMakeFountain),
        ("llMakeSmoke",llMakeSmoke),
        ("llMapDestination", llMapDestination),
        ("llMessageLinked", llMessageLinked),
        ("llMoveToTarget", llMoveToTarget),
        ("llOffsetTexture", llOffsetTexture),
        ("llOpenRemoteDataChannel", llOpenRemoteDataChannel),
        ("llOverMyLand", llOverMyLand),
        ("llOwnerSay", llOwnerSay),
        ("llPassCollisions", llPassCollisions),
        ("llPassTouches", llPassTouches),
        ("llPlaySound", llPlaySound),
        ("llPlaySoundSlave", llPlaySoundSlave),
        ("llPointAt",llPointAt),
        ("llPreloadSound", llPreloadSound),
        ("llRefreshPrimURL", llRefreshPrimURL),
        ("llRegionSay", llRegionSay),
        ("llReleaseCamera", llReleaseCamera),
        ("llRemoteDataReply", llRemoteDataReply),
        ("llRemoteDataSetRegion", llRemoteDataSetRegion),
        ("llRemoteLoadScript", llRemoteLoadScript),
        ("llRemoveFromLandPassList",llRemoveFromLandPassList),
        ("llRemoveFromLandBanList",llRemoveFromLandBanList),
        ("llRemoveInventory",llRemoveInventory),
        ("llRemoveVehicleFlags",llRemoveVehicleFlags),
        ("llRequestAgentData", llRequestAgentData),
        ("llRequestInventoryData",llRequestInventoryData),
        ("llRequestPermissions",llRequestPermissions),
        ("llRequestSimulatorData",llRequestSimulatorData),
        ("llResetLandPassList",llResetLandPassList),
        ("llResetLandBanList",llResetLandBanList),
        ("llResetOtherScript",llResetOtherScript),
        ("llResetScript",llResetScript),
        ("llResetTime",llResetTime),
        ("llRezAtRoot",llRezAtRoot),
        ("llRezObject",llRezObject),
        ("llRotLookAt",llRotLookAt),
        ("llRotTarget",llRotTarget),
        ("llRotTargetRemove",llRotTargetRemove),
        ("llRotateTexture", llRotateTexture),
        ("llSameGroup", llSameGroup),
        ("llSay",llSay),
        ("llScaleTexture", llScaleTexture),
        ("llSensor",llSensor),
        ("llSensorRemove",llSensorRemove),
        ("llSensorRepeat",llSensorRepeat),
        ("llSendRemoteData",llSendRemoteData),
        ("llSetAlpha", llSetAlpha),
        ("llSetBuoyancy", llSetBuoyancy),
        ("llSetClickAction", llSetClickAction),
        ("llSetColor", llSetColor),
        ("llSetForce", llSetForce),
        ("llSetForceAndTorque", llSetForceAndTorque),
        ("llSetHoverHeight", llSetHoverHeight),
        ("llSetLinkAlpha",llSetLinkAlpha),
        ("llSetLinkColor",llSetLinkColor),
        ("llSetLinkPrimitiveParams",llSetLinkPrimitiveParams),
        ("llSetLinkTexture",llSetLinkTexture),
        ("llSetLocalRot",llSetLocalRot),
        ("llSetObjectName",llSetObjectName),
        ("llSetObjectDesc",llSetObjectDesc),
        ("llSetParcelMusicURL",llSetParcelMusicURL),
        ("llSetPayPrice",llSetPayPrice),
        ("llSetPos",llSetPos),
        ("llSetPrimitiveParams",llSetPrimitiveParams),
        ("llSetPrimURL",llSetPrimURL),
        ("llSetRemoteScriptAccessPin", llSetRemoteScriptAccessPin),
        ("llSetRot",llSetRot),
        ("llSetScale",llSetScale),
        ("llSetScriptState",llSetScriptState),
        ("llSetSoundQueueing",llSetSoundQueueing),
        ("llSetSoundRadius",llSetSoundRadius),
        ("llSetSitText",llSetSitText),
        ("llSetStatus",llSetStatus),
        ("llSetText",llSetText),
        ("llSetTexture",llSetTexture),
        ("llSetTorque",llSetTorque),
        ("llSetTouchText",llSetTouchText),
        ("llSetVehicleFlags",llSetVehicleFlags),
        ("llSitTarget",llSitTarget),
        ("llShout",llShout),
        ("llSleep", llSleep),
        ("llSetTimerEvent",llSetTimerEvent),
        ("llSound",llSound),
        ("llSoundPreload", llSoundPreload),
        ("llStartAnimation", llStartAnimation),
        ("llStopAnimation", llStopAnimation),
        ("llStopHover",llStopHover),
        ("llStopLookAt",llStopLookAt),
        ("llStopMoveToTarget", llStopMoveToTarget),
        ("llStopPointAt", llStopPointAt),
        ("llStopSound",llStopSound),
        ("llTakeCamera",llTakeCamera),
        ("llTakeControls",llTakeControls),
        ("llTarget",llTarget),
        ("llTargetRemove",llTargetRemove),
        ("llTeleportAgentHome",llTeleportAgentHome),
        ("llTriggerSound",llTriggerSound),
        ("llTriggerSoundLimited",llTriggerSoundLimited),
        ("llUnSit",llUnSit),
        ("llVolumeDetect", llVolumeDetect),
        ("llWhisper",llWhisper),
        ("llWind",llWind),
        ("llWater",llWater)
    ] ++ map (\ (n,f) -> (n, defaultPredef n (\ i args -> lift $ f i args))) internalLLFuncs
    
allFuncs = (map (\ (name,_,_) -> name) funcSigs)
implementedFuncs = (map fst $ M.toList (defaultPredefs::(M.Map String (PredefFunc Maybe))))
unimplementedFuncs = S.toList (S.difference (S.fromList allFuncs) (S.fromList implementedFuncs))

---------------------------------------------------------------------------------------------------
logFromScript :: Monad m => ScriptInfo -> String -> WorldM m ()
logFromScript scriptInfo msg = logAMessage LogInfo (infoToLogSource scriptInfo) msg

infoToLogSource info = (scriptInfoPrimKey info ++ ": " ++ scriptInfoScriptName info)

addScript prim invScript = prim { primInventory = (invScript:(primInventory prim)) }

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
                           Just (LSLObject { primKeys = plist }) ->
                               case elemIndex pkey plist of
                                   Nothing -> return Nothing
                                   Just i -> return $ Just (p1Key, i, primName p)

pushEvent e key sid =
    do scripts <- getWorldScripts
       case M.lookup (key,sid) scripts of
           Nothing -> logAMessage LogWarn "sim" ("no such script: " ++ (show (key, sid)) ++ ", event: " ++ show e)
           Just script -> setWorldScripts (M.insert (key,sid) (script { scriptEventQueue = (scriptEventQueue script) ++ [e] } ) scripts)

pushEventToPrim e key =
    runErrPrim key () (getPrimScripts key >>= mapM_ (lift . pushEvent e key) . (map invName))
           
pushEventToObject e key =
    do objects <- getObjects
       case M.lookup key objects of
           Nothing -> logAMessage LogWarn "sim" ("no such object: " ++ key)
           Just o ->  mapM_ (pushEventToPrim e) (primKeys o)

pushDataserverEvent pk sn key val = pushDeferredScriptEvent (Event "dataserver" [KVal key, SVal val] M.empty) pk sn 0
pushChangedEventToObject oid val = pushDeferredScriptEventToObject (Event "changed" [IVal val] M.empty) oid 0
pushAttachEvent pk k = pushDeferredScriptEventToPrim (Event "attach" [KVal k] M.empty) pk 0

pushDeferredScriptEvent event pk sn delay = 
    putWorldEvent delay (DeferredScriptEvent event (DeferredScriptEventScriptTarget (pk,sn)))
pushDeferredScriptEventToPrim event pk delay =
    putWorldEvent delay (DeferredScriptEvent event (DeferredScriptEventPrimTarget pk))
pushDeferredScriptEventToObject event oid delay =
    putWorldEvent delay (DeferredScriptEvent event (DeferredScriptEventObjectTarget oid))
    
putHTTPTimeoutEvent pk sn key delay = 
    pushDeferredScriptEvent (Event "http_response" [KVal key, IVal 499, LVal [], SVal ""] M.empty) pk sn delay
putHTTPResponseEvent pk sn key status metadata body delay =
    pushDeferredScriptEvent (Event "http_response" [KVal key, IVal status, LVal metadata, body] M.empty) pk sn delay
    
getObject name = lift getObjects >>= mlookup name
setObject oid obj = getObjects >>= setObjects . M.insert oid obj
getObjectDynamics k = getObject k >>= return . objectDynamics
getObjectPosition k = getObjectDynamics k >>= return . objectPosition
getObjectRotation k = getObjectDynamics k >>= return . objectRotation

newWorld slice maxt iq lib scripts avatars objs prims activeScripts regions keyIndex webHandling eventHandler log = World {
               sliceSize = slice,
               maxTick = maxt,
               nextPause = slice,
               wqueue = iq,
               wlisteners = IM.empty,
               nextListenerId = 0,
               wobjects = objs,
               wprims = prims,
               worldScripts = activeScripts,
               inventory = [],
               tick = 0,
               msglog = [LogMessage 0 LogWarn "init" s | s <- log ],
               predefs = defaultPredefs,
               randGen = mkStdGen 1,
               wlibrary = lib,
               wscripts = scripts,
               worldEventHandler = fmap (flip (,) []) eventHandler,
               worldAvatars = avatars,
               worldBreakpointManager = emptyBreakpointManager,
               worldSuspended = Nothing,
               worldRegions = regions,
               worldZeroTime = 0,
               worldKeyIndex = keyIndex,
               worldWebHandling = webHandling,
               worldOutputQueue = [],
               worldPendingHTTPRequests = [],
               worldOpenDataChannels = (M.empty,M.empty),
               worldXMLRequestRegistry = M.empty,
               worldPhysicsTime = 0,
               worldTargetCheckTime = 0,
               worldLastPositions = M.empty,
               worldCollisions = S.empty,
               worldLandCollisions = S.empty,
               worldTouches = M.empty,
               worldTouchCheckTime = 0
           }
           
putWorldEvent delay we = 
    do weq <- getWQueue
       t <- getTick
       setWQueue $ putWQ (t + max (durationToTicks delay) 1) we weq

checkBp bp sm =
    do  bpm <- getWorldBreakpointManager
        let (result,bpm',sm') = checkBreakpoint bp bpm sm
        setWorldBreakpointManager bpm'
        return (result,sm')
       
updatePrim f key =
    do  prims <- getPrims
        case M.lookup key prims of
            Nothing -> logAMessage LogWarn "sim" ("prim " ++ key ++ " not found")
            Just p -> do p' <- f p
                         setPrims (M.insert key p' prims)

registerListener listener =
   do listeners <- getListeners
      id <- getNextListenerId
      let id' = id + 1
      setNextListenerId id'
      setListeners $ IM.insert id (listener,True) listeners
      return id
unregisterListener pk sname id = 
    do listeners <- lift getListeners
       (listener,_) <- ilookup id listeners
       when (listenerScriptName listener /= sname && listenerPrimKey listener /= pk)
            (throwError ("listener " ++ show id ++ " not registered for calling script"))
       lift $ setListeners $ IM.delete id listeners
updateListener pk sname id active =
    do listeners <- lift getListeners
       (listener,_) <- ilookup id listeners
       when (listenerScriptName listener /= sname && listenerPrimKey listener /= pk)
            (throwError ("listener " ++ show id ++ " not registered for calling script"))
       lift $ setListeners $ IM.insert id (listener,active) listeners
              
processEvents ::  (Monad m) => WorldM m ()
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
    
processEvent (DeferredScriptEvent event (DeferredScriptEventScriptTarget (pk,sn))) = pushEvent event pk sn
processEvent (DeferredScriptEvent event (DeferredScriptEventPrimTarget pk)) = pushEventToPrim event pk
processEvent (DeferredScriptEvent event (DeferredScriptEventObjectTarget oid)) = pushEventToObject event oid
processEvent (CreatePrim name key) =
    do worldPrims <- getPrims
       objects <- getObjects
       let prim = emptyPrim name key
       setPrims (M.insert key prim worldPrims)
       setObjects (M.insert key (LSLObject [key] defaultDynamics) objects)
processEvent (AddScript (name,script) key active) =
       do scripts <- getWScripts
          t <- getTick
          case lookup script scripts of
              Nothing -> logAMessage LogWarn "sim" ("no such script: " ++ script)
              Just (Left s) -> do
                  scriptKey <- newKey
                  updatePrim (\ p -> return $ addScript p (scriptInventoryItem name scriptKey script)) key
--                   scripts <- getWorldScripts
--                   setWorldScripts (M.insert (key,name) ((mkScript (Invalid s)) { scriptActive = False, scriptStartTick = t, scriptLastResetTick = t, scriptEventQueue = []}) scripts)
              Just (Right code) -> do
                  scriptKey <- newKey
                  updatePrim (\ p -> return $ addScript p (scriptInventoryItem name scriptKey script)) key
                  let sstate = initLSLScript code
                  scripts <- getWorldScripts
                  setWorldScripts (M.insert (key,name) ((mkScript sstate) { scriptStartTick = t, scriptLastResetTick =  t }) scripts)
processEvent chat@(Chat chan name key msg location range) =
    do listeners <- getListeners 
       locatedListeners <- mapM locateListener (IM.elems listeners)
       let listeners'= [ l | (l,_,_) <- filter (matchListener chat) locatedListeners]
       -- event goes to all UNIQUE addresses in list
       let addresses = nub $ map listenAddress listeners'
       mapM_ (\ (key',sid) -> pushEvent (Event "listen" [IVal chan, SVal name, KVal key, SVal msg] M.empty) key' sid) addresses
       when (chan == 0) $ case range of
           Nothing -> return ()
           Just dist -> pushChatToAvatars location dist
    where locateListener (listener,active) = do
              (VVal x y z) <- getPos (listenerPrimKey listener)
              region <- getRegionIndex (listenerPrimKey listener)
              return (listener,active,(region,(x,y,z)))
          pushChatToAvatars location range = do
              avatars <- getWorldAvatars >>= return . M.elems
              mapM_ (pushChatToAvatar location range) avatars
          pushChatToAvatar (region,pos) range avatar =
              when (region == avatarRegion avatar && dist3d2 pos (avatarPosition avatar) <= range^2) $ do
                  putWorldEvent 0 $ (AvatarInputEvent (avatarKey avatar) (AvatarHearsChat name key msg))
processEvent (WorldSimEvent name args) = 
    case M.lookup name eventDescriptors of
        Nothing -> logAMessage LogWarn "sim" ("event " ++ name ++ " not understood")
        Just def -> handleSimInputEvent def args
processEvent (PermissionRequestEvent pk sname ak mask) = 
    runAndLogIfErr ("can't find prim/script: " ++ pk ++ "/" ++ sname) () $ do
        scripts <- lift getWorldScripts
        script <- mlookup (pk,sname) scripts
        let script' = script { scriptPermissions = M.insert ak mask (scriptPermissions script), scriptLastPerm = Just ak }
        lift $ setWorldScripts (M.insert (pk,sname) script' scripts)
        lift $ pushEvent (Event "run_time_permissions" [IVal mask] M.empty) pk sname
processEvent evt@(TimerEvent interval (primKey,scriptName)) =
    do pushEvent (Event "timer" [] M.empty) primKey scriptName
       putWorldEvent interval evt
processEvent evt@(SensorEvent (pk,sn) name key stype range arc rpt) =
    runAndLogIfErr "problem processing sensor even" () $ do 
       t <- lift getTick
       maybe (return ()) (\ interval -> lift $ putWorldEvent interval evt) rpt
       sensedObjects <- senseObjects
       sensedAvatars <- senseAvatars
       let list = take 16 (sensedObjects ++ sensedAvatars)
       if null list
           then lift $ pushEvent (Event "no_sensor" [] M.empty) pk sn
           else lift $ pushEvent (Event "sensor" [(IVal $ length list)] (M.fromList (zipWith (\ i k -> ("key_" ++ show i,KVal k)) [0..] list))) pk sn       
    where senseObjects = do
              pos <- getGlobalPos pk
              rot <- getGlobalRot pk
              root <- getRootPrim pk
              let fwd = fwdFromRot rot
              prims <- rootPrims root
              poss <- mapM getGlobalPos (map primKey prims)
              let pprims = zip poss prims
              statuses <- mapM objectStatus (map primKey prims)
              return [ primKey p | ((pos,p),stat) <- zip pprims statuses, withinSensorArea pos fwd range arc pos, stat .&. stype /= 0]
          senseAvatars = 
              if stype .&. cAgent /= 0 then do
                      pos <- getGlobalPos pk
                      rot <- getGlobalRot pk
                      attachKey <- getPrimAttachment pk >>= return . liftM attachmentKey
                      let fwd = fwdFromRot rot
                      avs <- allAvs attachKey
                      return [ avatarKey av | av <- avs, withinSensorArea pos fwd range arc (avatarPosition av)]
                  else return []
          fwdFromRot rot = let ((x,_,_),(y,_,_),(z,_,_)) = quaternionToMatrix rot in (x,y,z)
          angleBetweenV (x0,y0,z0) (x1,y1,z1) = acos (x0*x1 + y0*y1 + z0*z1)
          withinSensorArea origin direction range arc pos =
              let v = diff3d pos origin
                  m = mag3d v 
                  v' = norm3d v
              in if m == 0 
                     then True
                     else m <= range && (angleBetweenV v' direction) <= arc
          rootPrims exclude = do
              objs <- lift getObjects
              mapM getPrim [ k | LSLObject { primKeys = (k:_) } <- M.elems objs, exclude /= k]
          objectStatus root = do
              LSLObject { primKeys = pks } <- getObject root
              stat <- foldM (\ x k -> determineActivePassiveScripted k >>= return . (x .|.)) 0 pks
              if (stat .&. cActive /= 0 && stat .&. cPassive /= 0) 
                 then return (stat .&. complement cPassive)
                 else return stat
          allAvs exclude = do
              avs <- lift getWorldAvatars
              return [ av | av <- M.elems avs, exclude /= Just (avatarKey av)]
processEvent (HTTPRequestEvent (pk,sn) key url method mimetype maxlength verify body) = do
        logAMessage LogDebug "sim" "processing an HTTP request event"
        handling <- getWorldWebHandling
        case handling of
            WebHandlingByDoingNothing -> putHTTPTimeoutEvent pk sn key 60 
            WebHandlingByFunction -> runAndLogIfErr "problem with handler" () $ do
                (moduleName, state) <- lift getWorldEventHandler >>= maybe (throwError "no handler defined") return
                lib <- lift getWLibrary
                -- invoke the function to get the results
                case simFunc lib (moduleName,"httpRequest") state [SVal url, SVal method, SVal mimetype, IVal maxlength, IVal verify, SVal body] of
                    Left s -> throwError s
                    Right (SVal msg,results) | not (null msg) -> do
                        lift $ setWorldEventHandler (Just (moduleName, results))
                        throwError msg
                                             | otherwise ->
                        let IVal status = maybe (IVal 200) id (lookup "outHttpStatus" results >>= ( \ val -> if isIVal val then Just val else Nothing))
                            LVal metadata = maybe (LVal []) id (lookup "outHttpMetadata" results >>= ( \ val -> if isLVal val then Just val else Nothing))
                            body = maybe (SVal "") id (lookup "outHttpBody" results >>= ( \ val -> if isSVal val then Just val else Nothing))
                            events = maybe (LVal []) id (lookup "outEvents" results)
                        in do lift $ putHTTPResponseEvent pk sn key status metadata body 2
                              lift $ processEventsList events
                              lift $ setWorldEventHandler (Just (moduleName, results))
                    Right _ -> throwError ("invalid web handling function, must return a string")
            WebHandlingByInternet timeout -> do
                let event = SimEvent "http_request" [
                                SimEventArg "url" url,
                                SimEventArg "prim" pk,
                                SimEventArg "script" sn,
                                SimEventArg "requestKey" key,
                                SimEventArg "HTTP_METHOD" method,
                                SimEventArg "HTTP_MIMETYPE" mimetype,
                                SimEventArg "HTTP_BODY_MAXLENGTH" (show maxlength),
                                SimEventArg "HTTP_VERIFY_CERT" (show verify),
                                SimEventArg "body" body] 0
                (getWorldOutputQueue >>= return . (event:) >>= setWorldOutputQueue)
                (getWorldPendingHTTPRequests >>= return . (key:) >>= setWorldPendingHTTPRequests)
                -- put a timeout event on the queue.  'real' time and sim time are much different, so the 
                -- 'real' world timeout is much different
                putHTTPTimeoutEvent pk sn key timeout
processEvent (XMLRequestEvent source channel idata sdata) = 
    runAndLogIfErr "invalid xml request" () $ do
        lift $ logAMessage LogInfo "sim" "processing XMLRequestEvent"
        (pk,sn) <- lookupScriptFromChan channel <||> throwError ("no such channel" ++ channel)
        key <- lift newKey
        lift getWorldXMLRequestRegistry >>= return . (M.insert key source) >>= lift . setWorldXMLRequestRegistry
        lift $ pushEvent (Event "remote_data" [llcRemoteDataRequest, KVal channel, KVal nullKey, SVal "", IVal idata, SVal sdata]
                                              (M.fromList [("requestKey",KVal key)])) pk sn
processEvent (XMLReplyEvent key channel messageId sdata idata) = do
    runAndLogIfErr "invalid xml reply" () $ do
        source <- (lift getWorldXMLRequestRegistry >>= mlookup key) <||> throwError "missing source for XML request"
        lift (getWorldXMLRequestRegistry >>= return . M.delete key >>= setWorldXMLRequestRegistry)
        case source of
            XMLRequestInternal tag -> do
                (moduleName, state) <- lift getWorldEventHandler >>= maybe (throwError "no handler defined") return
                lib <- lift getWLibrary
                case simFunc lib (moduleName, "xmlReply") state [SVal tag, SVal channel, SVal sdata, IVal idata] of
                    Left s -> throwError s
                    Right (SVal msg, results) | not (null msg) -> do
                        lift $ setWorldEventHandler (Just (moduleName, results))
                        throwError msg
                                              | otherwise ->
                        let events = maybe (LVal []) id (lookup "outEvents" results) in do
                            lift $ processEventsList events
                            lift $ setWorldEventHandler (Just (moduleName, results))
            XMLRequestExternal tag -> do
                let event = SimEvent "xml_reply" [
                        SimEventArg "tag" tag,
                        SimEventArg "channel" channel,
                        SimEventArg "sdata" sdata,
                        SimEventArg "idata" (show idata)] 0
                lift $ (getWorldOutputQueue >>= return . (event:) >>= setWorldOutputQueue)
processEvent (DialogEvent agent message buttons channel source) =
    runAndLogIfErr "invalid dialog event" () $ do
        lift getWorldEventHandler >>= (\ mhandler -> case mhandler of
            Nothing -> return ()
            Just (moduleName,state) -> do
                lib <- lift getWLibrary
                avName <- (lift getWorldAvatars >>= mlookup agent >>= return . avatarName) <||> throwError "problem finding avatar"
                objName <- getPrimName source <||> throwError "problem finding object"
                case hasFunc lib (moduleName,"dialog") of
                    Left s -> throwError s
                    Right False -> return ()
                    Right True -> 
                        case simFunc lib (moduleName, "dialog") state 
                               [SVal avName, SVal message, LVal (map SVal buttons), IVal channel, SVal objName] of
                            Left s -> throwError s
                            Right (SVal msg, results) | not (null msg) -> do
                                warn
                                lift $ setWorldEventHandler (Just (moduleName, results))
                                throwError msg
                                                      | otherwise ->
                                let (IVal selection) = maybe (IVal (-1)) id (lookup "outDialogButtonSelected" results)
                                    events = maybe (LVal []) id (lookup "outEvents" results)
                                in do
                                    warn
                                    when (selection >= 0 && selection < length buttons) $ 
                                        lift $ putChat (Just 20.0) agent channel (buttons !! selection)
                                    lift $ processEventsList events
                                    lift $ setWorldEventHandler (Just (moduleName, results)))
                            where warn = lift $ logAMessage LogWarn "sim" 
                                    "the 'dialog' entry point for the world event handler is deprecated, use the avatar event handler instead"

processEvent (RezObjectEvent links pos vel rot start pk copy atRoot) =
    runAndLogIfErr "invalid rez object event" () $ do
        when (null links) $ throwError "empty link set!"
        -- reset positions relative to root
        let geomCenter = scale3d ( 1 / fromIntegral (length links)) 
                                 (foldl' add3d (0,0,0) (map ((flip diff3d (0,0,0)) . primPosition) links))
        let pos' = if atRoot then pos else (pos `add3d` (neg3d geomCenter))
        let update = if copy then (\ p -> updateKeys p) else return
        -- TODO: rotation and velocity
        links' <- mapM update links
        mapM activateScripts links'
        let rootKey = primKey (head links')
        lift (getObjects >>= return . M.insert rootKey (LSLObject (map primKey links') defaultDynamics { objectPosition = pos' }) >>= setObjects)
        lift $ (getPrims >>= (\ m -> return (foldl' (\ m l -> M.insert (primKey l) l m) m links')) >>= setPrims)
        lift $ pushEventToObject (Event "on_rez" [IVal start] M.empty) rootKey
        lift $ pushEventToPrim (Event "object_rez" [KVal rootKey] M.empty) pk
    where activateScripts prim = do
              let scripts = [ item | item <- primInventory prim, isInvScriptItem item ]
              let activateScriptItem item = 
                      let InvScript id state = inventoryItemData item
                          name = inventoryItemName item in
                              activateScript (primKey prim) name id state start
              mapM activateScriptItem scripts
          updateKeys prim = do
              key <- lift newKey
              newInventory <- mapM updateItemKeys (primInventory prim)
              return $ prim { primKey = key, primInventory = newInventory }
          updateItemKeys item = do
              key <- lift newKey
              let name = inventoryItemName item
              newData <- if isInvObjectItem item
                  then mapM updateKeys (invObjectPrims $ inventoryItemData item) >>= return . InvObject
                  else return (inventoryItemData item)
              return $ item { inventoryItemIdentification = InventoryItemIdentification (name,key), inventoryItemData = newData }
processEvent (ResetScriptEvent pk sn) =
     runAndLogIfErr "invalid reset script event" () (resetScript pk sn)
processEvent (DetachCompleteEvent oid ak) =
     runAndLogIfErr "invalid detach complete event" () $ do
         -- need to remove the object and it's prims and its active scripts from the live lists, but
         -- save the script states
         avatars <- lift getWorldAvatars
         avatar <- mlookup ak avatars
         LSLObject { primKeys = links } <- getObject oid
         prims <- mapM passivatePrim links
         when (null prims) $ throwError "object has no prims!"
         let root = head prims
         let object = InventoryItem (InventoryItemIdentification (primName root,primKey root)) 
                                    (InventoryInfo (primCreator root) (0x0008e000,0x0008e000,0x0008e000,0x0008e000,0x0008e000))
                                    (InvObject prims)
         let avInv = avatarInventory avatar
         lift $ setWorldAvatars (M.insert ak (avatar { avatarInventory = object:avInv }) avatars)
         objects <- lift getObjects
         lift $ setObjects (M.delete oid objects)
     where passivatePrim pk = do
               scriptItems <- getPrimScripts pk
               mapM_ (moveScriptStateToPrim pk) (map inventoryItemName scriptItems)
               prims <- lift getPrims
               prim <- mlookup pk prims
               lift $ setPrims $ M.delete pk prims
               return prim
           moveScriptStateToPrim pk sn = do
               scripts <- lift getWorldScripts
               script <- mlookup (pk,sn) scripts
               let img = scriptImage script
               primInv <- getPrimInventory pk
               let (xs,ys) = break (\ item -> sn == inventoryItemName item) primInv
               when (null ys) $ throwError ("script inexplicably not in inventory")
               (y:ys') <- return ys
               let scriptData = inventoryItemData y
               lift $ setPrimInventory pk (((y { inventoryItemData = scriptData { invScriptState = Just img } }) : xs) ++ ys)
               lift $ setWorldScripts (M.delete (pk,sn) scripts)
processEvent (GiveAvatarInventoryEvent ak item) = logAMessage LogInfo "sim" "giving avatar inventory: not implemented"
processEvent (AvatarOutputEvent ak avEv) = processAvatarOutputEvent ak avEv
processEvent (AvatarInputEvent ak avEv) = processAvatarInputEvent ak avEv
processEvent _ = error "not implemented"

processEventsList (LVal []) = return ()
processEventsList (LVal ((IVal i):l)) = do
    processEventList (take i l)
    processEventsList (LVal (drop i l))
processEventsList _ = logAMessage LogWarn "sim" ("user supplied event handler has invalid type for 'outEvents' variable")
    
processEventList [SVal "xml_request", SVal sourceId, KVal channel, IVal idata, SVal sdata, FVal delay] = do
    putWorldEvent delay (XMLRequestEvent (XMLRequestInternal sourceId) channel idata sdata)
processEventList l = logAMessage LogWarn "sim" ("Invalid event from event handler: " ++ lslShowVal (LVal l))

processAvatarOutputEvent k (AvatarTouch {avatarTouchPrimKey = pk, avatarTouchDuration = secs}) = do
        touches <- getWorldTouches
        now <- getTick
        setWorldTouches (M.alter (alt now) pk touches)
    where alt start Nothing = Just [newTouch start]
          alt start (Just ts) = Just (newTouch start:ts)
          newTouch start = Touch {touchAvatarKey = k, touchPrimKey = pk, touchStartTick = start, touchEndTick = start + durationToTicks secs}
processAvatarOutputEvent k (AvatarWhisper {avatarChatChannel = chan, avatarChatMessage = msg}) = avChat 10 msg k chan
processAvatarOutputEvent k (AvatarSay {avatarChatChannel = chan, avatarChatMessage = msg}) = avChat 20 msg k chan
processAvatarOutputEvent k (AvatarShout {avatarChatChannel = chan, avatarChatMessage = msg}) = avChat 100 msg k chan
processAvatarOutputEvent k (AvatarPay {avatarPayPrimKey = pk, avatarPayAmount = val}) = runAndLogIfErr "problem handling avatar payment" () $
    primHasActiveHandler k "money" >>= flip when (lift $ pushEventToPrim (Event "money" [KVal pk, IVal val] M.empty) pk)
processAvatarOutputEvent k (AvatarControl { avatarNewControlBits = newBits }) = runAndLogIfErr "problem processing control change" () $ do
    av <- getWorldAvatar k
    let prev = avatarControls av
    lift $ setWorldAvatar k (av { avatarControls = newBits })
    flip (maybe (return ())) (avatarControlListener av) $ 
        \ (AvatarControlListener {avatarControlListenerMask = mask, avatarControlListenerScript = (pk,sn)}) ->
            lift $ pushEvent (Event "control" [KVal k, IVal newBits, IVal (newBits `xor` prev)] M.empty) pk sn
--processAvatarOutputEvent _ _ = error "not yet implemented(1)"

avChat range msg key chan = runAndLogIfErr "problem processing chat" () $ do
    av <- getWorldAvatar key
    lift $ putWorldEvent 0 $ Chat chan (avatarName av) key msg (avatarRegion av,avatarPosition av) (Just range)

processAvatarInputEvent k inputEvent = runAndLogIfErr "problem processing event for avatar" () $ do
        av <- getWorldAvatar k
        flip (maybe (return ())) (avatarEventHandler av) $ (\ (moduleName,state) -> do
            lib <- lift getWLibrary
            case callAvatarEventProcessor k moduleName inputEvent lib state of
                Nothing -> return ()
                Just (Left s) -> throwError s
                Just (Right (LVal events,result)) -> do
                    mapM_ (putAvatarOutputEvent k) events
                    lift $ setWorldAvatar k (av { avatarEventHandler = Just (moduleName,result) }))

avEventProcCallInfo (AvatarOwnerSay key msg) = ("onOwnerSay",[SVal key, SVal msg])
avEventProcCallInfo (AvatarHearsChat name key msg) = ("onChat",[SVal name, SVal key, SVal msg])
avEventProcCallInfo (AvatarDialog msg buttons chan source) = ("onDialog",[SVal msg, LVal $ map SVal buttons, IVal chan, SVal source])
avEventProcCallInfo (AvatarLoadURL msg url) = ("onLoadURL",[SVal msg, SVal url])
avEventProcCallInfo (AvatarMapDestination simName position) = ("onMapDestination",[SVal simName, vec2VVal position])
callAvatarEventProcessor k moduleName avEvent lib state = 
    case hasFunc lib (moduleName,funcName) of
        Left s -> Just (Left s)
        Right True -> Just $ simFunc lib (moduleName, funcName) state args
        Right False -> Nothing
    where (funcName,args) = avEventProcCallInfo avEvent
    
putAvatarOutputEvent k (SVal s) =
    case reads s of
       ((ev,_):_) -> lift $ putWorldEvent 0 (AvatarOutputEvent k ev)
       [] -> lift $ logAMessage LogWarn "sim" ("avatar event handler for " ++ k ++ " returned invalid event: " ++ show s)
putAvatarOutputEvent k v = lift $ logAMessage LogWarn "sim" ("avatar event handler for " ++ k ++ " returned invalid event: " ++ lslShowVal v)
 
activateScript pk sn scriptId Nothing startParam =
    do wscripts <- lift getWScripts
       worldScripts <- lift getWorldScripts
       case lookup scriptId wscripts of
           Nothing -> lift $ logAMessage LogWarn "sim" ("script " ++ scriptId ++ " not found")
           Just (Left s) -> lift $ logAMessage LogWarn "sim" ("script " ++ scriptId ++ " not valid")
           Just (Right code) ->
               lift $ setWorldScripts 
                   (M.insert (pk,sn) (mkScript (initLSLScript code)) worldScripts)
activateScript pk sn _ (Just image) startParam = do
    worldScripts <- lift getWorldScripts
    lift $ setWorldScripts
        (M.insert (pk,sn) ((mkScript image) { scriptStartParameter = startParam }) worldScripts)
        
matchListener (Chat chan' sender' key' msg' (region,position) range) ((Listener pkey sid chan sender key msg),active,(region',position')) =
    active &&
    region == region' &&
    chan == chan' &&
    key' /= pkey &&
    (sender == "" || sender == sender') &&
    (key == "" || key == nullKey || key == key') &&
    (msg == "" || msg == msg') &&
    (case range of
        Nothing -> True
        Just dist -> dist3d2 position position' <= dist^2)

listenAddress l = (listenerPrimKey l, listenerScriptName l)

nextActivity :: (Monad (StateT (World a) a), Monad a) => StateT (World a) a (Maybe Int)
nextActivity = 
    do  scripts <- getWorldScripts
        t <- getTick >>= return . (+1)
        let ns = foldl' (calcNxt t) Nothing $ M.elems scripts
        wq <- getWQueue
        let ne = case wq of
                     [] -> Nothing
                     ((i,x):_) -> Just $ max t i
        return $ mmin ne ns
    where calcNxt t n (Script { scriptActive = False }) = n
          calcNxt t n (Script { scriptImage = img , scriptEventQueue = q }) =
              case executionState img of
                  Executing -> (Just t)
                  Suspended _ -> (Just t)
                  Waiting -> if null q then n else (Just t)
                  WaitingTil i -> if isNothing n then Just (max t i) else fmap (max t . min i) n 
                  SleepingTil i -> if isNothing n then Just (max t i) else fmap (max t . min i) n
                  _ -> n
          mmin Nothing Nothing = Nothing
          mmin Nothing (Just i) = (Just i)
          mmin (Just i) Nothing = (Just i)
          mmin (Just i) (Just j) = Just (min i j)
                  
runScripts :: Monad m => WorldM m ()
runScripts =
    do scripts <- getWorldScripts
       mapM_ findAndRunScript (M.keys scripts)
       
findAndRunScript scriptKey =
    do scripts <- getWorldScripts
       case M.lookup scriptKey scripts of
          Nothing -> return ()
          Just script -> checkAndRunScript scriptKey script
checkAndRunScript _ (Script { scriptActive = False }) = return ()
checkAndRunScript k@(pkey,sname) (Script {scriptImage = img, scriptEventQueue = q}) =
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
                       let script' = script { scriptImage = img'', scriptEventQueue = q' }
                       setWorldScripts (M.insert (pkey,sname) script' scripts)
                       case executionState img'' of
                           Suspended bp -> setWorldSuspended (Just (pkey,sname))
                           _ -> setWorldSuspended Nothing


handleTouches :: Monad m => WorldM m ()
handleTouches = do
    now <- getTick
    before <- getWorldTouchCheckTime
    when (now - before > durationToTicks 0.05) $ do
        touchLists <- getWorldTouches >>= return . M.toList
        runAndLogIfErr "problem handling touches" () $ do
            starts <- collectTouches "touch_start" M.empty $ 
                         concatMap (\ (_,tl) -> [ touch | touch@(Touch { touchStartTick = t }) <- tl, t > before]) touchLists
            ongoings <- collectTouches "touch" M.empty $ 
                concatMap 
                (\ (_,tl) -> [ touch | touch@(Touch { touchStartTick = ts, touchEndTick = te }) <- tl, te >= now && ts <= before]) touchLists
            ends <- collectTouches "touch_end" M.empty $ 
                       concatMap (\ (_,tl) -> [ touch | touch@(Touch { touchEndTick = te }) <- tl, te <= now]) touchLists
            lift $ mapM_ (uncurry (sendTouch "touch_start")) $ M.toList starts
            lift $ mapM_ (uncurry (sendTouch "touch")) $ M.toList ongoings
            lift $ mapM_ (uncurry (sendTouch "touch_end")) $ M.toList ends
        let nonends = [ (pk,[ t | t@(Touch { touchEndTick = te }) <- list, te > now]) | (pk,list) <- touchLists ]
        setWorldTouchCheckTime now
        setWorldTouches (M.fromList nonends)
        return ()

sendTouch ttype k tl =
     pushEventToPrim (Event ttype [IVal $ length tl] payload) k
     where payload = foldl' ins M.empty (zip [0..] tl)
           ins m (i,(Touch { touchAvatarKey = ak },link)) = 
               (M.insert ("vector_" ++ show i) (VVal 0 0 0)) .
               (M.insert ("key_" ++ show i) (KVal ak)) .
               (M.insert ("integer_" ++ show i) (IVal link)) $ m
               
collectTouches touchType acc [] = return acc
collectTouches touchType acc (t@(Touch { touchPrimKey = pk }):ts) = do
    lneeds <- primHasActiveHandler pk touchType
    linkNum <- getPrimLinkNum pk
    rk <- getRootPrim pk
    if pk == rk && lneeds then collectTouches touchType (M.alter (alt (t,linkNum)) pk acc) ts
              else do
                  pass <- getPrimPassTouches pk
                  rneeds <- primHasActiveHandler rk touchType
                  let r = if rneeds && pass then [(rk,(t { touchPrimKey = rk }, linkNum))] else []
                  let p = if lneeds then [(pk,(t, linkNum))] else []
                  collectTouches touchType (foldl' ( \ m (k,d) -> M.alter (alt d) k m) acc (r ++ p)) ts
    where alt i Nothing = Just [i]
          alt i (Just l)  = Just (i:l)
runPhysics :: Monad m => WorldM m ()
runPhysics = do
        t0 <- getWorldPhysicsTime
        t1 <- getTick
        os <- getObjects
        let tlist = [t0,(t0+10)..t1] ++ if (t1 - t0) `mod` 10 > 0 then [t1] else []
        let intervals = zip tlist $ tail tlist
        forM_ intervals $ \ (t0,t1) -> do
            forM_ (M.toList os) $ \ (pk,obj) -> runErrPrim pk () $ do
                prim <- getPrim pk
                obj <- getObject pk
                when ((flip testBit) primPhysicsBit $ primStatus prim) $ do
                    mass <- objectMass pk
                    let dyn = objectDynamics obj
                    let pos0@(_,_,z0) = objectPosition dyn
                    let vel0@(_,_,vz0) = objectVelocity dyn
                    let force = (fst (objectForce dyn) `rot3d` rotation) `add3d` (0,0,mass * (objectBuoyancy dyn) * (-gravC))
                            where rotation = if snd $ objectForce dyn then objectRotation dyn else (0,0,0,1)
                    let impulse = ((fst . fst . objectImpulse) dyn `rot3d` rotation, (snd . objectImpulse) dyn)
                            where rotation = if (snd . fst . objectImpulse) dyn then objectRotation dyn else (0,0,0,1)
                    let impulseFNow = if snd impulse >= t1 then fst impulse else (0,0,0)
                    let heightDampF tau z = (0,0, dampZForce tau z z0 mass vz0 fz0)
                            where (_,_,fz0) = force `add3d` impulseFNow
                    let dampF = case objectPositionTarget dyn of
                                     Nothing -> (0,0,0)
                                     Just (PositionTarget { positionTargetTau = tau, positionTargetLocation = l }) ->
                                         dampForce tau l pos0 mass vel0 (force `add3d` impulseFNow)
                                     Just (Repel { positionTargetTau = tau, positionTargetHeight = z }) ->
                                             if z0 <= 0.5 * z then heightDampF tau z else (0,0,0)
                                     Just (Hover { positionTargetTau = tau, positionTargetHeight = z}) -> heightDampF tau z
                    let (pos,vel) = kin t0 t1 ticksToDuration 0 mass pos0 vel0 (force `add3d` dampF) impulse
                    -- TODO: local vs global rotations in torque
                    -- TODO: rotational impulses
                    radius <- objRadius pk
                    let torque = totalTorque t0 t1 ticksToDuration
                                     [(fst $ objectTorque dyn,t1),(fst $ fst $ objectRotationalImpulse dyn, snd $ objectRotationalImpulse dyn)]
                    let dampingTorque = case objectRotationTarget dyn of
                                            Nothing -> (0,0,0)
                                            Just rt@(RotationTarget {rotationTargetTau = tau, 
                                                                     rotationTargetStrength = strength,
                                                                     rotationTarget = target}) ->
                                                dampTorque tau strength target
                                                           (objectRotation dyn) mass radius (objectOmega dyn) torque
                    let (rot,omega) = rotDyn (ticksToDuration (t1 - t0)) radius mass (objectRotation dyn) (objectOmega dyn)
                                             (torque `add3d` dampingTorque)
                    lift $ setObject pk $! (obj { objectDynamics = (objectDynamics obj) 
                                                      { objectVelocity = vel, 
                                                        objectPosition = pos,
                                                        objectRotation = rot,
                                                        objectOmega = omega }})
                    return ()
        handleCollisions
        handleMovers
        handleTargets
        setWorldPhysicsTime $! t1

handleMovers :: Monad m => WorldM m ()
handleMovers = runAndLogIfErr "problem determining which objects moved" () $
    do lastPositions <- lift $ getWorldLastPositions
       let lastKeys = S.fromList $ M.keys lastPositions
       curPositions <- lift getObjects >>= return . M.keys >>= mapM (\ k -> getObjectPosition k >>= return . ((,)k))
       let curKeys = S.fromList $ map fst curPositions
       let newKeys = curKeys `S.difference` lastKeys
       let keysFromBefore = curKeys `S.difference` newKeys
       m <- return . M.fromList =<< mapM ( \ (k,pos) -> if k `S.member` keysFromBefore
                                                        then do (moving,lpos) <- mlookup k lastPositions
                                                                let moving' = lpos /= pos
                                                                when (moving' && not moving) $ 
                                                                    lift $ pushEventToPrim (Event "moving_start" [] M.empty) k
                                                                when (moving && not moving') $ 
                                                                    lift $ pushEventToPrim (Event "moving_end" [] M.empty) k
                                                                return (k,(moving', pos))
                                                        else return (k,(False,pos))) curPositions
       lift $ setWorldLastPositions m

handleObjectCollisions prims = do
        -- look at all non-phantom prims
        primBoxes <- mapM (\ (k,p) -> getPrimBox k >>= return . ((,) k ) . ((,)p) ) prims
        --let voldtct = map fst $ filter (primVolumeDetect . snd) prims
        voldtct <- flip filterM prims (\ (_,prim) ->
            let f k = (getObject k >>= return . objectVolumeDetect . objectDynamics) in
                case primParent prim of
                    Nothing -> f (primKey prim)
                    Just k -> f k
            ) >>= return . map fst
        let cmp x y = compare (fst x) (fst y)
        let curCollisions = S.fromList [ (k0,k1) | ((k0,(p0,_)),(k1,(p1,_))) <- checkIntersections (snd . snd) cmp primBoxes, 
                                                   primParent p0 /= Just k1 &&
                                                   primParent p1 /= Just k0 &&
                                                   not (primParent p0 == primParent p1 && isJust (primParent p0))]
        oldCollisions <- lift getWorldCollisions
        let formerCollisions = oldCollisions `S.difference` curCollisions
        let newCollisions = curCollisions `S.difference` oldCollisions
        toObjCollisions newCollisions "collision_start" >>= mapM_ (send "collision_start")
        (do
            objCollisions <- toObjCollisions curCollisions "collision"
            -- filter out volume detect objects, except for those objects that are attatched
            let attachment k = do
                    parent <- getPrimParent k
                    case parent of
                        Nothing -> getPrimAttachment k
                        Just ok -> getPrimAttachment ok
            objCollisions' <- (mapM (\ v@(k,_) -> (attachment k >>= return . ((,) v))) objCollisions) 
                >>= return . (filter (\ ((k,_),mv) -> (k `notElem` voldtct) || isJust mv))
                >>= return . (map fst)
            mapM_ (send "collision") objCollisions')
        toObjCollisions formerCollisions "collision_end" >>= mapM_ (send "collision_end")
        lift $ setWorldCollisions curCollisions
    where send :: Monad m => String -> (String,[(Int,String)]) -> ErrorT String (WorldM m) ()
          send hn (pk,oinfo) =  collisionScripts >>= mapM_ (sendScript hn pk oinfo)
              where collisionScripts = do
                        scripts <- getPrimScripts pk >>= return . (map invName)
                        filterM (\ sn -> scriptHasActiveHandler pk sn hn) scripts
          sendScript :: Monad m => String -> String -> [(Int,String)] -> String -> ErrorT String (WorldM m) ()
          sendScript hn pk oinfo sn = do
              scripts <- lift $ getWorldScripts
              return ()
              case M.lookup (pk,sn) scripts of
                  Nothing -> throwError "can't find script"
                  Just (Script { scriptCollisionFilter = (name,key,accept) })
                          | null name && (null key || nullKey == key) && accept -> pushit oinfo
                          | null name && (null key || nullKey == key) && not accept -> return ()
                          | accept -> filterM (matchesIn name key) oinfo >>= pushit
                          | otherwise -> filterM (matchesOut name key) oinfo >>= pushit
              where matchesIn name key (_,k) = if k == key 
                                                   then return True 
                                                   else (if null key || key == nullKey 
                                                             then (getPrimName k >>= return . (name==))
                                                             else return False)
                    matchesOut name key (_,k) = if k == key 
                                                    then return False 
                                                    else (if null key || key == nullKey
                                                              then (getPrimName k >>= return . not . (name==))
                                                              else return True)
                    pushit oinfo = unless (null oinfo) $ lift (pushEvent ev pk sn)
                        where ev = Event hn [IVal $ length oinfo] 
                                      (M.fromList $
                                          (zipWith (\ i (_,k) -> ("key_" ++ show i, KVal k)) [0..] oinfo) ++
                                          (zipWith (\ i (n,_) -> ("integer_" ++ show i, IVal n)) [0..] oinfo))
          toObjCollisions collisionsS hn = do
                  passed <- passes primColliders
                  colliders' <- mapM (\ (k,cs) -> (getPrimLinkNum k >>= \ i -> return (k,i,cs))) primColliders
                  colliders'' <- mapM pk2ok (colliders' ++ passed)
                  colliders''' <-  mapM (\ (k,i,cs) -> filterM (objectHasVolDetect >=> (return . not)) cs >>= return . ((,,) k i)) colliders''
                  return $ M.toList (foldl' combine M.empty (map dist colliders'''))
              where collisions = S.toList collisionsS
                    dist (pk,i,cs) = (pk, zip (repeat i) cs)
                    combine m (k,lv) = case M.lookup k m of
                            Nothing -> M.insert k lv m
                            Just lv' -> M.insert k (lv ++ lv') m
                    --prims = fromListOfPairs collisions
                    primColliders = map (\ k -> (k, collectColliders collisions k)) (S.toList $ fromListOfPairs collisions)
                    passes [] = return []
                    passes ((pk,cs):pcs) = do
                        passed <- pass hn pk
                        if passed
                            then do
                                parent <- getPrimParent pk >>= maybe (throwError "can't get parent") return
                                rest <- passes pcs
                                num <- getPrimLinkNum pk
                                return ((parent,num,cs):rest)
                            else passes pcs
                    pk2ok (pk,i,cs) = primsToObjects cs >>= return . ((,,) pk i)
                    primsToObjects ks = mapM getPrimParent ks >>= (\ parents -> (return (nub $ zipWith fromMaybe ks parents)))
                    pass hn k = do
                        parent <- getPrimParent k
                        case parent of
                            Nothing -> return False
                            Just pk -> do
                                passOverride <- getPrimPassTouches k
                                pass <- (k `primHasActiveHandler` hn) >>= return . not
                                return (pass || passOverride)
                    fromListOfPairs [] = S.empty
                    fromListOfPairs ((x,y):ps) = (S.insert x . S.insert y) (fromListOfPairs ps)
                    collectColliders intsns pk = [ j | (Just j) <- map (other pk) intsns]
                    objectHasVolDetect ok = do
                         attach <- getPrimAttachment ok
                         if isJust attach
                             then return False
                             else getObject ok >>= return . objectVolumeDetect . objectDynamics
                    other k (x,y) | k == x = Just y
                                  | k == y = Just x
                                  | otherwise = Nothing

handleLandCollisions prims = do
        primBoxes <- mapM (\ (k,p) -> getPrimBox k >>= return . ((,) k )) prims
        let curCollisions = S.fromList [ k | (k,((_,_,z),_)) <- primBoxes, z <= 0]
        oldCollisions <- lift getWorldLandCollisions
        let formerCollisions = oldCollisions `S.difference` curCollisions
        let newCollisions = curCollisions `S.difference` oldCollisions
        mapM_ (send "land_collision_end") $ S.toList formerCollisions
        mapM_ (send "land_collision") $ S.toList curCollisions
        mapM_ (send "land_collision_start") $ S.toList newCollisions
        lift $ setWorldLandCollisions curCollisions
    where send hn pk = do
                  pass <- getPrimPassCollisions pk
                  pos <- getGlobalPos pk >>= ( \ (x,y,_) -> return (x,y,0))
                  liveScripts <- getActualPrimScripts pk >>= 
                      (\ ss -> return [ s | s@(_,Script { scriptActive = True }) <- ss])
                  when (not $ null liveScripts) $ send' pos pk
                  when (null liveScripts || pass) $ getRootPrim pk >>= send' pos
              where send' pos k = do
                        scripts <- getPrimScripts k >>= return . (map invName) >>=
                                   filterM (\ sn -> scriptHasActiveHandler k sn hn)
                        forM_ scripts $ (lift . pushEvent (Event hn [vec2VVal pos] M.empty) k)
                     
handleCollisions :: (Monad m) => WorldM m ()
handleCollisions = runAndLogIfErr "can't handle collisions" () $ do
    prims <- getNonPhantomPrims
    handleObjectCollisions prims
    handleLandCollisions prims
    
getNonPhantomPrims :: (Monad m) => ErrorT String (WorldM m) [(String,Prim)]
getNonPhantomPrims = lift getPrims >>= return . M.assocs >>= (return . filter ((==0) . (.&. cStatusPhantom) . primStatus . snd))
   
handleTargets :: Monad m => WorldM m ()
handleTargets = do
    t <- getTick
    t0 <- getWorldTargetCheckTime
    when (t - t0 > durationToTicks 0.1) $ do
        getObjects >>= return . M.keys >>= mapM_ (\ k -> do
            runErrPrim k () $ do
                pos <- getGlobalPos k
                rot <- getGlobalRot k
                scripts <- getActualPrimScripts k
                forM_ scripts (\ ((_,sn),script) -> do
                        forM_ (IM.toList $ scriptPositionTargets script) $ (\ (i,(target,range)) -> 
                            let image = scriptImage script
                                inRange = (dist3d2 target pos <= range^2) in
                            if hasActiveHandler image "at_target" && inRange
                                then lift $ pushEvent (Event "at_target" [IVal i, vec2VVal target, vec2VVal pos] M.empty) k sn
                                else if hasActiveHandler image "not_at_target" && not inRange
                                         then lift $ pushEvent (Event "not_at_target" [] M.empty) k sn
                                         else return ()
                            )
                        forM_ (IM.toList $ scriptRotationTargets script) $ (\ (i,(target,range)) ->
                            let image = scriptImage script
                                inRange = (angleBetween target rot <= range) in
                            if hasActiveHandler image "at_rot_target" && inRange
                                then lift $ pushEvent (Event "at_rot_target" [IVal i, rot2RVal target, rot2RVal rot] M.empty) k sn
                                else if hasActiveHandler image "not_at_rot_target" && not inRange
                                         then lift $ pushEvent (Event "not_at_rot_target" [] M.empty) k sn
                                         else return ()
                            )
                    )                        
                    
            )

simulate :: Monad m => WorldM m ()
simulate =
    do processEvents
       runScripts
       runPhysics
       handleTouches
       mn <- nextActivity
       t <- getTick
       let t' = case mn of
                    Nothing -> t + 10
                    Just n -> min (t + 10) n
       setTick t'
       --setTick (t + 1)
       pauseTime <- getNextPause
       suspendInfo <- getWorldSuspended
       if t' >= pauseTime || isSuspended suspendInfo then return () else simulate
    where isSuspended Nothing = False
          isSuspended _ = True

linkSet = -1::Int
linkAllOthers = -2::Int
linkAllChildren = -3::Int
linkThis = -4::Int

-- etrace :: Monad m => String -> m ()
-- etrace val = trace val $ return ()
-- trace1 s val = trace (s ++ ": " ++ show val) val

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
        simStateInfoAvatars :: [(String,String)],
        simStateInfoScripts :: [(String,String)]
    } deriving (Show)
    
nullSimState = SimStateInfo 0 [] [] []

stateInfoFromWorld world =
    SimStateInfo {
        simStateInfoTime = tick world,
        simStateInfoPrims = map (\ p -> (primKey p, primName p)) (M.elems $ wprims world),
        simStateInfoAvatars = map (\ (_,a) -> (avatarKey a, avatarName a)) (M.toList $ worldAvatars world),
        simStateInfoScripts = M.keys (worldScripts world)
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
     
initWorld def scripts lib = worldFromFullWorldDef newWorld def lib scripts

simStep init@(Left (worldDef, scripts, lib)) command =
--     let world = initWorld worldDef scripts lib in simStep (Right world) command
    case initWorld worldDef scripts lib of
        Left s -> -- initialization failed
            (SimEnded ("Error initializing: " ++ s) [LogMessage 0 LogError "sim" ("Error initializing: " ++ s)] nullSimState, init)
        Right world -> simStep (Right world) command
simStep (Right world) command =
    let t = tick world
        events = simCmdEvents command
        newBreakpointManager = replaceBreakpoints (simCmdBreakpoints command) (worldBreakpointManager world)
        slice = sliceSize world
        newScripts = case worldSuspended world of
             Nothing -> worldScripts world
             Just (k,s) ->
                 let Just script = M.lookup (k,s) (worldScripts world) 
                     img = scriptImage script
                     stepMgr = stepManager img
                     img' = case command of
                                SimStep _ _ -> img { stepManager = setStepBreakpoint stepMgr }
                                SimStepOver _ _ -> img { stepManager = setStepOverBreakpoint stepMgr }
                                SimStepOut _ _ -> img { stepManager = setStepOutBreakpoint stepMgr }
                                _ -> img
                 in M.insert (k,s) (script { scriptImage = img' }) (worldScripts world)
        simEventToWorldEvent (SimEvent name args delay) = (t + delay, WorldSimEvent name args)
        wq' = putManyWQ (map simEventToWorldEvent events) (wqueue world)
        world' = world { wqueue = wq', 
                         nextPause = t + slice,
                         worldBreakpointManager = newBreakpointManager,
                         worldScripts = newScripts }
        (_,world'') = runIdentity $ runStateT simulate world'
        log = msglog world''
        outputEvents = reverse $ worldOutputQueue world''
        world''' = world'' { msglog = [], worldOutputQueue = [] } in
        if (tick world''') < (maxTick world''') 
            then case worldSuspended world''' of
                Nothing ->
                     (SimInfo { simStatusEvents = outputEvents, 
                                simStatusLog = log,
                                simStatusState = stateInfoFromWorld world''' }, Right world''')
                Just (k,s) ->
                     let Just script = M.lookup (k,s) (worldScripts world''') -- TODO: non-exhaustive but MUST succeed
                         img = scriptImage script
                         (Suspended bp) = executionState img -- TODO: non-exhaustive but MUST succeed
                         executionInfo = ExecutionInfo (breakpointFile bp) (breakpointLine bp) (frameInfo img)
                     in
                     (SimSuspended { simStatusEvents = outputEvents, 
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

handleSimInputEvent def args = 
  case checkEventArgs def args of
      Left s -> logAMessage LogWarn "sim" s
      Right argValues -> (simInputEventHandler def) (simInputEventName def) argValues

mkTouchStartEvent pk nd ak ln = 
    WorldSimEvent "touch_start" [SimEventArg "Prim Key" pk, SimEventArg "num_detected" nd,
                                 SimEventArg "Avatar key" ak, SimEventArg "Link Number" (show ln)]
mkTouchEvent pk nd ak ln = 
    WorldSimEvent "touch" [SimEventArg "Prim Key" pk, SimEventArg "num_detected" nd, 
                           SimEventArg "Avatar key" ak, SimEventArg "Grab vector" "<0.0,0.0,0.0>",
                           SimEventArg "Link Number" (show ln)]
mkTouchEndEvent pk nd ak ln = 
    WorldSimEvent "touch_end" [SimEventArg "Prim Key" pk, SimEventArg "num_detected" nd,
                               SimEventArg "Avatar key" ak, SimEventArg "Link Number" (show ln)]
                                 
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
            let f _ [KVal pk, KVal ak, FVal duration] = runAndLogIfErr "can't find prim? " () $ do
                        -- should we pass touches?
                        prim <- getPrim pk
                        linkNum <- getPrimLinkNum pk                        
--                         linkNum <- (case primParent prim of
--                                         Nothing -> return 0
--                                         Just parent -> do
--                                             LSLObject { primKeys = links } <- getObject parent
--                                             Just i <- return $ elemIndex pk links
--                                             return (i + 1)) <||>
--                                                (do lift $ logAMessage LogWarn "sim" ("can't figure out link number of prim: " ++ pk)
--                                                    return 0)
                        needstouches <- liftM or $ mapM (primHasActiveHandler pk) ["touch","touch_start","touch_end"]
                        let touchList = (if needstouches then [pk] else []) ++
                                        (if (primPassTouches prim || not needstouches) then maybe [] return (primParent prim) else [])
                        lift $ mapM_ (doTouch linkNum) touchList
                    where doTouch linkNum k = do
                            putWorldEvent 0 (mkTouchStartEvent k "1" ak linkNum)
                            let tdur = floor (1000 * duration)
                            let tTimes = map ((/1000.0) . fromIntegral) [1,(1 + 500) .. tdur]
                            mapM_ ((flip putWorldEvent) (mkTouchEvent pk "1" ak linkNum)) tTimes 
                            putWorldEvent duration (mkTouchEndEvent pk "1" ak linkNum)                             
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
                    avatars <- getWorldAvatars
                    case M.lookup ak avatars of
                        Nothing -> logAMessage LogWarn "sim" ("avatar with key " ++ ak ++ " not found")
                        Just av -> do
                            putWorldEvent 0 $ Chat chan (avatarName av) ak message (avatarRegion av,avatarPosition av) (Just 20.0)
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
               EventDeliveryScript -> [SimParam "Script" "name of script to deliver to" SimParamScript,
                                       SimParam "Prim Key" "key of prim to deliver to" SimParamPrim]
               EventDeliveryObject -> [SimParam "Object Key" "key of object to deliver to" SimParamRootPrim]
               EventDeliveryRoot -> [SimParam "Object Key" "key of object to deliver to" SimParamRootPrim]
               EventDeliveryPrim -> [SimParam "Prim Key" "key of prim to deliver to" SimParamPrim]
               ) ++
            (flip map additionalData (\ d -> case d of
                EventAdditionalInts name description -> SimParam name description (SimParamLSLValue LLInteger)
                EventAdditionalKeys name description -> SimParam name description SimParamKey
                EventAdditionalAvatarKeys name description -> SimParam name description SimParamAvatar
                EventAdditionalVectors name description -> SimParam name description (SimParamLSLValue LLVector))) ++
            map ( \ (t,name) -> SimParam name "" (SimParamLSLValue t)) params,
        simInputEventHandler =
            let f _ list = 
                    let lenAddl = length additionalData
                        (df,rest) = case (delivery,list) of
                               (EventDeliveryScript,SVal sn:KVal pk:vals) -> (\ e -> pushEvent e pk sn,vals)
                               (EventDeliveryObject,KVal key:vals) -> (\ e -> pushEventToObject e key,vals)
                               (EventDeliveryRoot, KVal key:vals) -> ((\ e -> pushEventToPrim e key), vals)
                               (EventDeliveryPrim, KVal key:vals) -> ((\ e -> pushEventToPrim e key), vals)
                               _ -> (\ _ -> logAMessage LogWarn "sim" "invalid user event - bad script address",[])
                    in df (Event name (drop lenAddl rest) (M.fromList (zipWith mkInfo additionalData (take lenAddl rest))) )
                       where mkInfo (EventAdditionalVectors _ _) val = ("vector_0",val)
                             mkInfo (EventAdditionalInts _ _)    val = ("integer_0",val)
                             mkInfo _                            val = ("key_0",val)
            in f
    }
