{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fwarn-unused-binds -fwarn-unused-imports #-}
-------------------------------------------------------------------------------
-- |
-- Module        : Language.Lsl.Internal.SimLL
-- 
-- This module contains implementations of the impure LSL 'll' functions for
-- the LSL Plus Sim.  There are several hundred of these functions (hence the
-- length of this module!); most of them have reasonable implementations, some
-- simply log that they've been called because no reasonable implementation
-- is possible, and a few have no implementation, which causes the Sim to fall
-- back on the a default behavior, based on the functions type.
-------------------------------------------------------------------------------
module Language.Lsl.Internal.SimLL(
    doPredef,
    defaultPredefs,
    determineActivePassiveScripted,
    getGlobalPos,
    getGlobalRot,
    getPrimBox,
    objectMass,
    putChatE,
    resetScript,
    sayRange,
    unimplementedFuncs
    ) where

import Control.Applicative hiding (optional)
import Control.Monad(
    MonadPlus(..),foldM,forM_,liftM,liftM2,unless,when,ap)
import Control.Monad.State(StateT(..),lift)
import Control.Monad.Error(MonadError(..),ErrorT,Error)
import Data.List(find,foldl',isSuffixOf)
import Data.Bits((.&.),(.|.),bit,clearBit,complement,setBit,shiftL,testBit)
import Data.Int()
import Data.Map(Map)
import Data.Maybe(fromMaybe,isNothing)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM

import Language.Lsl.Internal.Animation(builtInAnimations)
import Language.Lsl.Internal.AvEvents(AvatarInputEvent(..))
import Language.Lsl.Internal.CodeHelper(renderCall)
import Language.Lsl.Internal.Constants
import Language.Lsl.Internal.Evaluation(ScriptInfo(..),Event(..),
    EvalResult(..))
import Language.Lsl.Internal.Exec(ExecutionState(..),ScriptImage(..),hardReset)
import Language.Lsl.Internal.FuncSigs(funcSigs)
import Language.Lsl.Internal.InternalLLFuncs(internalLLFuncs)
import Language.Lsl.Internal.Key(nullKey)
import Language.Lsl.Internal.Log(LogLevel(..))
import Language.Lsl.Internal.Physics(calcAccel,primMassApprox)
import Language.Lsl.Syntax(Ctx(..),FuncDec(..),predefFuncs)
import Language.Lsl.Internal.Type(LSLValue(..),LSLType(..),defaultValue,
    lslBool,lslValString,rVal2Rot,rot2RVal,typeOfLSLValue,vVal2Vec,vec2VVal,
    onI,onS)
import Language.Lsl.Internal.Util(add3d,diff3d,fac,findM,fromInt,
    generatePermutation,ilookup,lookupByIndex,mag3d,mlookup,norm3d,
    quaternionMultiply,rot3d,rotationBetween,scale3d,tuplify,lift2,lift3,lift4,
    (<||>),(<??>),rotL,optional,whenJust)
import Language.Lsl.WorldDef(Attachment(..),Avatar(..),
    AvatarControlListener(..),Email(..),Flexibility(..),InventoryInfo(..),
    InventoryItem(..),InventoryItemData(..),InventoryItemIdentification(..),
    LightInfo(..),LSLObject(..),ObjectDynamics(..),Parcel(..),Prim(..),
    PrimFace(..),PrimType(..),PositionTarget(..),Region(..),RotationTarget(..),
    Script(..),TextureInfo(..),defaultCamera,defaultDynamics,findByInvKey,
    findByInvName,inventoryInfoPermValue,inventoryItemName,isInvAnimationItem,
    isInvBodyPartItem,isInvClothingItem,isInvGestureItem,isInvNotecardItem,
    isInvObjectItem,isInvScriptItem,isInvSoundItem,isInvTextureItem,
    primPhantomBit,primPhysicsBit)
import Language.Lsl.Internal.WorldState

import System.Time(ClockTime(..),CalendarTime(..),TimeDiff(..),addToClockTime,
    toUTCTime)
import Text.Printf(PrintfType(..),printf)

-- an orphan instance...
instance (Monad m, Error e) => Applicative (ErrorT e m) where
   pure  = return
   (<*>) = ap

-- just for fun
(<<=) = (=<<)

infixl 1 <<=

-- some logging functions
slog :: Monad m => ScriptInfo Float -> String -> WorldM m ()
slog = logAMessage LogInfo . infoToLogSource
    where infoToLogSource info = scriptInfoPrimKey info ++ ": " ++ scriptInfoScriptName info

slogE = lift2 slog

-- execute a predefined ('ll') function
doPredef :: Monad m => String -> ScriptInfo Float -> [LSLValue Float] -> WorldM m (EvalResult,LSLValue Float)
doPredef name info@(ScriptInfo oid pid sid pkey event) args =
   (maybe doDefault runIt . tryPredefs name) =<< getPredefFuncs
    where runIt (t,f) = runErrFunc info name (defaultValue t) args f
          doDefault = do
               (_,rettype,argtypes) <- findM (\ (x,y,z) -> x == name) funcSigs
               logAMessage LogDebug (pkey ++ ":" ++ sid) 
                   ("unimplemented predefined function called: " ++ 
                   renderCall name args)
               continueWith $ defaultValue rettype
    
tryPredefs name predefs = fmap f (M.lookup name predefs)
    where f (PredefFunc _ t predef) = (t,predef)
        
defaultPredef name predef =
    case find (\ (FuncDec n t ps) -> name == ctxItem n) predefFuncs of
        Nothing -> error ("undefined predef " ++ name)
        Just (FuncDec _ t _) -> PredefFunc name t predef

runErrFunc info fname defval args f =
    evalErrorT (f info args) >>= either
        (\ s -> slog info (fname ++ ": " ++ s) >> continueWith defval) return
           

required a = a >>= maybe (throwError "required") return
prohibited a = maybe (return ()) (const $ throwError "prohibited") =<< a

primErr k _ = throwError ("problem - can't find prim " ++ k)

withAvatar ak notFound found =
    maybe notFound found =<< (optional $ getWorldAvatar ak)
    
pushDataserverEvent pk sn key val = 
    pushDeferredScriptEvent (Event "dataserver" [KVal key, SVal val] M.empty) pk sn 0
pushDataserverEventE = lift4 pushDataserverEvent
   
--- Web related functions ----------------------------------------------------

llHTTPRequest info@(ScriptInfo _ _ sn pk _) [SVal url, LVal params, SVal body] = 
    do  t <- getTickE
        -- verify the parameters
        when (length params `mod` 2 /= 0 || length params > 8) 
            (throwError "invalid parameter list")
        let params' = tuplify params
        unless (all (\ (x,_) -> x `elem` [llcHTTPMethod,llcHTTPMimetype,llcHTTPBodyMaxlength,llcHTTPVerifyCert]) params')
            (throwError "invalid parameter list")
        method <- maybe (return "GET") 
            (\ v -> if v `notElem` map SVal ["GET","POST","PUT","DELETE"]
              then throwError ("invalid HTTP method " ++ lslValString v)
              else let SVal s = v in return s) 
            (lookup llcHTTPMethod params')
        mimetype <- maybe (return "text/plain;charset=utf-8")
              (onS return (throwError "invalid mimetype"))
              (lookup llcHTTPMimetype params')
        maxlength <- maybe (return 2048)
              (onI return (throwError "invalid body length"))
              (lookup llcHTTPBodyMaxlength params')
        verify <- maybe (return 1)
              (onI return (throwError "invalid verify cert value"))
              (lookup llcHTTPVerifyCert params')
        -- parameters ok
        key <- newKeyE
        putWorldEventE 0 (HTTPRequestEvent (pk,sn) key url method mimetype maxlength verify body)
        continueK key

--- Sensor related functions -------------------------------------------------

llSensor info@(ScriptInfo _ _ sn pk _) [SVal name, KVal key, IVal etype, FVal range, FVal arc] =
    putWorldEventE 0 (SensorEvent (pk,sn) name key etype range arc Nothing) >> continueV
llSensorRepeat info@(ScriptInfo _ _ sn pk _) [SVal name, KVal key, IVal etype, FVal range, FVal arc, FVal interval] =
    putWorldEventE interval (SensorEvent (pk,sn) name key etype range arc (Just interval')) >> continueV
    where interval' = max 0 interval
llSensorRemove info@(ScriptInfo _ _ sn pk _) [] =
    do q <- getWQueueE
       setWQueueE [ qentry | qentry@(_,event) <- q, not (isSensorEvent event) || sensorAddress event /= (pk,sn)]
       continueV
------------------------------------------------------------------------------

llRequestAgentData info@(ScriptInfo _ _ sn pk _) [KVal ak, IVal d] =
    do key <- newKeyE
       av <- optional $ getWorldAvatar ak
       dataVal <- case av of
           Nothing -> do
               slogE info ("llRequestAgentData: no such avatar - " ++ ak)
               return "0"
           Just av -> case d of
                d | d == cDataBorn -> return "2006-01-01"
                  | d == cDataOnline -> return "1"
                  | d == cDataRating -> return "0,0,0,0,0"
                  | d == cDataName -> return $ avatarName av
                  | d == cDataPayinfo -> return "3"
                  | otherwise -> throwError ("invalid data requested: " ++ show d)
       pushDataserverEventE pk sn key dataVal
       continueK key

llRequestSimulatorData info@(ScriptInfo _ _ sn pk _) [SVal simName, IVal d] =
    do regions <- lift getWorldRegions
       let findRegion = findM ((==simName) . regionName . snd) (M.toList regions)
       let simErr = throwError ("unknown simulator " ++ simName)
       val <- case d of
                  d | d == cDataSimStatus -> (findRegion >> return "up") <||> return "unknown"
                    | d == cDataSimRating -> (findRegion >> return "PG") <||> simErr
                    | d == cDataSimPos -> (do ((x,y),_) <- findRegion
                                              return $ lslValString $ vec2VVal (256 * fromIntegral x, 256 * fromIntegral y, 0))
                                          <||> simErr
                    | otherwise -> throwError ("invalid data requested: " ++ show d)
       key <- newKeyE
       pushDataserverEventE pk sn key val
       continueK key
--- INVENTORY related functions ----------------------------------------------

getPrimInventoryInfo pk invType =
    case invType of
        i | i == llcInventoryAll ->       sortit getPrimInventory
          | i == llcInventoryAnimation -> sortit getPrimAnimations
          | i == llcInventoryBodyPart ->  sortit getPrimBodyParts
          | i == llcInventoryClothing ->  sortit getPrimClothing
          | i == llcInventoryGesture ->   sortit getPrimGestures
          | i == llcInventoryNotecard ->  sortit getPrimNotecards
          | i == llcInventoryObject ->    sortit getPrimObjects
          | i == llcInventoryScript ->    sortit getPrimScripts
          | i == llcInventorySound ->     sortit getPrimSounds
          | i == llcInventoryTexture ->   sortit getPrimTextures
          | otherwise -> 
              throwError ("invalid inventory type: " ++ lslValString invType)
    where sortit f = sortByInvName <$> (f pk)
                            
llGetInventoryNumber info@(ScriptInfo _ _ _ pk _) [invType@(IVal _)] =
    getPrimInventoryInfo pk invType >>= continueI . length

sayRange = Just 20.0

llGetInventoryCreator info@(ScriptInfo _ _ _ pk _) [SVal name] =
    do  all <- getPrimInventory pk
        continueK =<< case findByInvName name all of
            Nothing -> putChatE sayRange pk 0 ("no item named '" ++ name ++ "'") >> return nullKey
            Just item -> return $ inventoryInfoCreator $ inventoryItemInfo  item

llGetInventoryPermMask info@(ScriptInfo _ _ _ pk _) [SVal name, IVal mask] =
    do  all <- getPrimInventory pk
        continueI =<< case findByInvName name all of
            Nothing -> putChatE sayRange pk 0 ("No item named '" ++ name ++ "'") >> throwError ("No item named '" ++ name ++ "'")
            Just item -> inventoryInfoPermValue mask (inventoryInfoPerms $ inventoryItemInfo item)
    
llGetInventoryKey info@(ScriptInfo _ _ _ pk _) [SVal name] =
    do  all <- getPrimInventory pk
        continueK =<< case findByInvName name all of
            Nothing -> return nullKey
            Just (InventoryItem id invInfo _) -> 
                let Right perms =  inventoryInfoPermValue cMaskOwner (inventoryInfoPerms invInfo) in
                    if perms .&. cFullPerm == cFullPerm then
                        return $ snd (inventoryItemNameKey id)
                    else return nullKey
       
llGetInventoryName info@(ScriptInfo _ _ _ pk _) [invType@(IVal _), IVal index] =
    (map inventoryItemName) <$> (getPrimInventoryInfo pk invType) >>= 
        continueS . fromMaybe "" . lookupByIndex index
    
llGetInventoryType info@(ScriptInfo _ _ _ pk _) [SVal name] =
        getPrimInventory pk >>= continueI . classify . findByInvName name
    where classifications = [(isInvAnimationItem,cInventoryAnimation),
                             (isInvBodyPartItem,cInventoryBodyPart),
                             (isInvClothingItem,cInventoryClothing),
                             (isInvGestureItem,cInventoryGesture),
                             (isInvNotecardItem,cInventoryNotecard),
                             (isInvObjectItem,cInventoryObject),
                             (isInvSoundItem,cInventorySound),
                             (isInvTextureItem,cInventoryTexture),
                             (isInvScriptItem,cInventoryScript)]
          classify Nothing = (-1)
          classify (Just item) = 
              foldl' (\ cur (f,i) -> if (cur < 0) && f item then i else cur) (-1) classifications

llRemoveInventory info@(ScriptInfo _ _ _ pk _) [SVal name] =
    do  inv <- getPrimInventory pk
        maybe (sayErr pk ("Missing inventory item '" ++ name ++ "'"))
              (\  _ -> let inv' = [ item | item <- inv, name /= inventoryItemName item] in
                  setPrimInventoryE pk inv')
              (findByInvName name inv)
        continueV

llGiveInventory info@(ScriptInfo _ _ _ pk _) [KVal k, SVal inv] =
    do  inventory <- getPrimInventory pk
        item <- maybe (throwError ("inventory item " ++ inv ++ "not found")) (return . id) (findByInvName inv inventory)
        prim <- optional $ getPrimE k
        case prim of
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
                       setPrimInventoryE k (case findByInvName inv inventory' of
                               Nothing -> item' : inventory'
                               Just _ -> changeName item' newName : inventory'
                                   where newName = findAName 1 inv)
                       -- TODO: are scripts handled right?
                       pushDeferredScriptEventToPrimE
                                  (Event "changed" [if hasModifyPerm then llcChangedInventory else llcChangedAllowedDrop] M.empty) k 0
                   else throwError "can't modify/drop inventory on target"
            Nothing -> do
                av <- getWorldAvatar k <||> throwError ("no object or avatar found with key " ++ k)
                lift (putWorldEvent 0 (GiveAvatarInventoryEvent k item)) >>
                         slogE info ("llGiveInventory: avatar " ++ k ++ " given option of accepting inventory")
        continueV
    where
        changeName (InventoryItem (InventoryItemIdentification (name,key)) info dat) newName =
            InventoryItem (InventoryItemIdentification (newName,key)) info dat
        copyInventoryItem newOwner newGroup item = case item of
            item | isInvObjectItem item -> do
                      let copyLink prim = do
                             k <- newKeyE
                             inventory <- mapM (copyInventoryItem newOwner newGroup) (primInventory prim)
                             return (prim { primKey = k, primInventory = inventory, primOwner = newOwner, primGroup = newGroup })
                      InvObject links <- return $ inventoryItemData item
                      links' <- mapM copyLink links
                      when (null links') $ throwError "empty linkset!"
                      let info = inventoryItemInfo item
                      return (InventoryItem (InventoryItemIdentification (inventoryItemName item,primKey $ head links'))
                                             info (InvObject links'))
                 | otherwise -> do
                      k <- newKeyE
                      let info = inventoryItemInfo item
                      let dat = inventoryItemData item
                      return (InventoryItem (InventoryItemIdentification (inventoryItemName item, k)) info dat)

llSetRemoteScriptAccessPin info@(ScriptInfo _ _ _ pk _) [IVal pin] =
    lift2 setPrimRemoteScriptAccessPin pk pin >> continueV
llRemoteLoadScript info@(ScriptInfo _ _ _ pk _) [KVal _,SVal _, IVal _, IVal _] =
    sayErr pk  "Deprecated.  Please use llRemoteLoadScriptPin instead." >> continueV
       
llRezObject = rezObject False 
llRezAtRoot = rezObject True

rezObject atRoot info@(ScriptInfo _ _ _ pk _) [SVal inventory, VVal px py pz, VVal vx vy vz, RVal rx ry rz rs, IVal param] =
    do  objects <- getPrimObjects pk
        maybe (do putChatE (Just 20) pk 0 ("Couldn't find object '" ++ inventory ++ "'")
                  throwError ("no such inventory item - " ++ inventory))
              (\ item -> do
                  let copy = isCopyable item
                  let InvObject linkSet = inventoryItemData item
                  putWorldEventE 0 (RezObjectEvent linkSet (px,py,pz) (vz,vy,vz) (rz,ry,rz,rs) param pk copy atRoot)
                  unless copy $ do -- delete!
                     wholeInventory <- getPrimInventory pk
                     setPrimInventoryE pk
                         [ item | item <- wholeInventory, inventory /= inventoryItemName item]
              )
              (findByInvName inventory objects)
        continueV
    
isCopyable item = (cPermCopy .&. perm /= 0) && (not (isInvObjectItem item) || all primCopyable links)
    where (_,perm,_,_,_) = inventoryInfoPerms $ inventoryItemInfo item
          InvObject links = inventoryItemData item
          primCopyable = all isCopyable . primInventory
                
llResetScript info@(ScriptInfo _ _ sn pk _) [] =
    putWorldEventE 0 (ResetScriptEvent pk sn) >> doneV
       
llResetOtherScript info@(ScriptInfo _ _ sn pk _) [SVal sn'] =
    do  when (sn == sn') $ throwError "trying to reset the current script - must use llResetScript for this"
        scriptActive <$> (getScriptE pk sn) >>= flip unless (throwError "can't reset a stopped script")
        resetScript pk sn'
        continueV
    
resetScript pk sn = do
         logAMessageE LogDebug "sim" "resetting a script"
         script <- getScriptE pk sn
         t <- getTickE
         let img = scriptImage script
         let doReset execState =
                 setScriptE pk sn script {
                            scriptImage = (hardReset img) {executionState = execState},
                            scriptActive = True,
                            scriptPermissions = M.empty,
                            scriptLastPerm = Nothing,
                            scriptLastResetTick = t,
                            scriptEventQueue = [Event "state_entry" [] M.empty] }
                 in case executionState img of
                     Executing -> doReset Waiting
                     Waiting -> doReset Waiting
                     SleepingTil i -> doReset (WaitingTil i)
                     WaitingTil i -> doReset (WaitingTil i)
                     _ -> throwError "can't reset script that is crashed or suspended"
                     
llGetScriptState info@(ScriptInfo _ _ _ pk _) [SVal sn] =
    do  script <- getScriptE pk sn
        continueI $ if not $ scriptActive script then 0
                else case executionState (scriptImage script) of
                            Halted -> 0
                            Erroneous _ -> 0
                            Crashed _ -> 0
                            _ -> 1 
    
llSetScriptState info@(ScriptInfo _ _ sn pk _) [SVal sn', IVal state] =
    do  when (sn == sn') $ throwError "can't change your own state"
        script <- getScriptE pk sn'
        if state == 0 
            then when (scriptActive script) 
                    $ setScriptE pk sn' script { scriptActive = False }
            else case executionState (scriptImage script) of
                    Crashed _ -> throwError "can't change state of crashed script"
                    Erroneous _ -> throwError "can't change state of erroneous script"
                    _ -> if scriptActive script
                             then throwError "script is already running"
                             else resetScript pk sn'
        continueV
------------------------------------------------------------------------------
llSetPayPrice info@(ScriptInfo _ _ sn pk _) [IVal price, LVal l] =
    let toI (IVal i) = if i < (-2) then (-1) else i
        toI _ = (-1)
        [i1,i2,i3,i4] = take 4 $ map toI l ++ replicate 4 (-1) in
    do  hasMoney <- scriptHasActiveHandler pk sn "money"
        unless hasMoney $ throwError "no money handler, pay price info ignored"
        lift $ setPrimPayInfo pk (price,i1,i2,i3,i4)
        continueV
    
llGetStartParameter info@(ScriptInfo _ _ sn pk _) [] =
    getScriptE pk sn >>= continueI . scriptStartParameter
------------------------------------------------------------------------------
llCloud info _ =
    slogE info "llCloud: returning default density" >>  continueF 0

llWind info _ =
    slogE info "llWind: returning default density" >> continueVec (0,0,0)

llWater info _ =
    slogE info "llWater: returning default water elevation" >> continueF 0

llSitTarget (ScriptInfo _ _ _ pk _) [v@(VVal _ _ _),r@(RVal _ _ _ _)] =
    lift $ setPrimSitTarget pk (Just (vVal2Vec v, rVal2Rot r)) >> continueV
    
llAvatarOnSitTarget info@(ScriptInfo _ _ _ pk _) [] =
    do  sitTarget <- getPrimSitTarget pk
        when (isNothing sitTarget) (throwError "no sit target")
        (fromMaybe nullKey) <$> (getPrimSittingAvatar pk) >>= continueK

llUnSit info@(ScriptInfo _ _ _ pk _) [KVal k] = 
    do  val <- getPrimSittingAvatar pk
        maybe (throwError "no avatar sitting on prim (land unsit not implemented")
              (\ ak -> if ak == k then lift $ setPrimSittingAvatar pk Nothing
                                  else throwError "unsit of avatar not sitting on prim attempted (land unsit not implemented)")
              val
        continueV

llMessageLinked info@(ScriptInfo oid pid sid pkey _) [IVal link,IVal val,SVal msg,KVal key] =
    do  LSLObject { primKeys = links } <- getObjectE oid <||> throwError "object not found!"
        when (null links) $ throwError "object is has no links!"
        let sender = if length links > 1 then pid + 1 else pid
        let targetLinkIndices = targetLinks (length links) link pid
        let targetLinks = map (links !!) targetLinkIndices
        let event = (Event "link_message" [IVal sender, IVal val, SVal msg, KVal key] M.empty)
        mapM_ (\ pk -> pushDeferredScriptEventToPrimE event pk 0) targetLinks
        continueV

targetLinks n link pid =
    case link of
        l | l == (-1) -> [0 .. (n - 1)] -- whole link set
          | l == (-2) -> [0..(pid - 1)] ++ [(pid + 1).. (n - 1)] -- others
          | l == (-3) -> [1..(n - 1)] -- children
          | l == (-4) -> [pid] -- this prim
          | otherwise -> [link - 1]
          
-- the key to 'sleep' is that instead of returning 'EvalIncomplete' as its EvalResult, it returns
-- 'YieldTil <some-tick>', which puts the script into a sleep state.
-- other functions which have a built in delay should use this mechanism as well.
llSleep _ [FVal f] = yieldV . (+ durationToTicks f) =<< getTickE

llInstantMessage info@(ScriptInfo _ _ _ pk _) [KVal ak, SVal message] =
    do tick <- getTickE
       name <- getPrimName pk
       avname <- avatarName <$> getWorldAvatar ak
       logAMessageE LogInfo "sim" ("IM to " ++ avname ++ " from " ++ name ++ ": " ++ message)
       yieldV $ tick + durationToTicks 2
       
llSay = chatE sayRange
llWhisper = chatE (Just 10.0)
llShout = chatE (Just 100.0)
llRegionSay info params@[IVal chan, SVal message] =
    if chan == 0
       then do logAMessageE LogWarn (scriptInfoPrimKey info ++ ": " ++ scriptInfoScriptName info) "attempt to llRegionSay on channel 0"
               return (EvalIncomplete,VoidVal)
       else chatE Nothing info params
       
chat range info@(ScriptInfo oid pid sid pkey event) [IVal chan, SVal message] =
    do slog info $ concat ["chan = ", show chan, ", message = ", message]
       putChat range pkey chan message
       continueV
chatE = lift3 chat

putChat range k chan message = do
    prims <- getPrims
    case M.lookup k prims of
        Just prim -> runErrPrim k () $ do
            region <- (getPrimRegion k)
            pos <- getObjectPosition =<< getRootPrim k
            putWorldEventE 0 $ Chat chan (primName prim) k message (region, pos) range
        Nothing -> do
            avatars <- getWorldAvatars
            case M.lookup k avatars of
                Just avatar -> putWorldEvent 0 $ Chat chan (avatarName avatar) k message (avatarRegion avatar, avatarPosition avatar) range
                Nothing -> logAMessage LogWarn "sim" ("Trying to chat from unknown object/avatar with key " ++ k)
    
putChatE = lift4 putChat

sayErr k msg = putChatE sayRange k cDebugChannel msg

registerListener listener =
   do id <- getNextListenerId
      setNextListenerId (id + 1)
      setListeners . IM.insert id (listener,True) =<< getListeners
      return id
unregisterListener pk sname id = 
    do listeners <- getListenersE
       (listener,_) <- ilookup id listeners
       when (listenerScriptName listener /= sname && listenerPrimKey listener /= pk)
            (throwError ("listener " ++ show id ++ " not registered for calling script"))
       setListenersE $ IM.delete id listeners
updateListener pk sname id active =
    do listeners <- getListenersE
       (listener,_) <- ilookup id listeners
       when (listenerScriptName listener /= sname && listenerPrimKey listener /= pk)
            (throwError ("listener " ++ show id ++ " not registered for calling script"))
       setListenersE $ IM.insert id (listener,active) listeners

llListen (ScriptInfo oid pid sid pkey event) [IVal chan, SVal sender, KVal key, SVal msg] = 
    lift (registerListener $ Listener pkey sid chan sender key msg) >>=  continueI
    
llListenRemove (ScriptInfo _ _ sid pk _) [IVal id] =
    unregisterListener pk sid id >> continueV 

llListenControl (ScriptInfo _ _ sid pk _) [IVal id, IVal active] =
    updateListener pk sid id (active /= 0) >> continueV

llFrand _ [FVal maxval] = lift $ wrand >>= continueF . (maxval *)

llTeleportAgentHome info@(ScriptInfo _ _ _ pk _) [KVal ak] =
    do  owner <- getPrimOwner pk
        regionIndex <- getPrimRegion pk
        (_,_,parcel) <- getPrimParcel pk
        if parcelOwner parcel == owner
            then slogE info ("llTeleportAgentHome: user " ++ ak ++ " teleported home (in theory)")
            else slogE info "llTeleportAgentHome: not permitted to teleport agents from this parcel"
        continueV
        
llEjectFromLand info@(ScriptInfo oid _ _ pk _) [KVal user] =
    do  owner <- getPrimOwner pk
        regionIndex <- getPrimRegion pk
        (_,parcel) <- getParcelByPosition regionIndex =<< getObjectPosition oid
        if parcelOwner parcel == owner 
            then slogE info ("llEjectFromLand: user " ++ user ++ " ejected (in theory)")
            else slogE info "llEjectFromLand: not permitted to eject from this parcel"
        continueV
    
llBreakLink info@(ScriptInfo oid _ sid pk _) [IVal link] = 
    do  perm <- getPermissions pk sid
        if perm .&. cPermissionChangeLinks /= 0
            then do
                objects <- getObjectsE
                LSLObject { primKeys = links, objectDynamics = dynamics } <- mlookup oid objects
                prohibited (getPrimAttachment oid) <??> "can't change links of attached object"
                if link < 1 || link > length links then slogE info "llBreakLink: invalid link id"
                    else do
                        let (dyn1,dyn2) = if link == 1 then (defaultDynamics,dynamics) else (dynamics,defaultDynamics)
                        let (xs,y:ys) = splitAt (link - 1) links
                        let (linkSet1,linkSet2) = (xs ++ ys, [y])
                        let objects' = if null linkSet1 then objects else M.insert (head linkSet1) (LSLObject linkSet1 dyn1) objects
                        setObjectsE (M.insert (head linkSet2) (LSLObject linkSet2 dyn2) objects')
                        unless (null linkSet1) $ do
                            pushChangedEventToObjectE (head linkSet1) cChangedLink
                            setPrimParentE (head linkSet1) Nothing
                        pushChangedEventToObjectE (head linkSet2) cChangedLink
                        setPrimParentE (head linkSet2) Nothing
            else slogE info "llBreakLink: no permission"
        continueV

llBreakAllLinks info@(ScriptInfo oid _ sid pk _) [] =
    do  perm <- getPermissions pk sid
        if perm .&. cPermissionChangeLinks /= 0
            then do
                objects <- getObjectsE
                LSLObject links dynamics <- mlookup oid objects
                prohibited (getPrimAttachment oid) <??> "can't change links of attached object"
                when (length links > 1) $ do
                    -- order of parameters to union is vital: union is left biased, so we want the 
                    -- new objects (in the left argument) to replace the old (in the right) where
                    -- both exist (the root key of the old object is found in both)
                    let objects' = M.union 
                            (M.fromList $ map (\ k -> (k, LSLObject [k] (if k == oid then dynamics else defaultDynamics))) links) objects
                    setObjectsE objects'
                    mapM_ (\ link -> pushChangedEventToObjectE link cChangedLink >>
                        setPrimParentE link Nothing) links
            else slogE info "llBreakAllLinks: no permission"
        continueV
        
-- TODO: verify link order
llCreateLink info@(ScriptInfo oid _ sid pk _) [KVal target, IVal iparent] =
    do  perm <- getPermissions pk sid
        if perm .&. cPermissionChangeLinks /= 0
            then do
                objects <- getObjectsE
                LSLObject (link:links) dynamics <- mlookup oid objects
                case M.lookup target objects of
                    Nothing -> slogE info "llCreateLink: target not found"
                    Just (LSLObject (link':links') dynamics') -> do
                        prohibited (getPrimAttachment oid) <??> "can't change links of attached object"
                        prohibited (getPrimAttachment target) <??> "can't change links of attached object"
                        mask <- getObjectPermMask target cMaskOwner
                        ownerTarget <- getPrimOwner target
                        owner <- getPrimOwner oid
                        if mask .&. cPermModify /= 0 && owner == ownerTarget 
                           then do
                                let (newLinkset,deleteKey,newDynamics) = if parent 
                                        then ((link:link':links') ++ links, link',dynamics)
                                        else ((link':link:links) ++ links', link,dynamics')
                                setObjectsE (M.insert (head newLinkset) (LSLObject newLinkset newDynamics) $ M.delete deleteKey objects)
                                pushChangedEventToObjectE (head newLinkset) cChangedLink
                            else slogE info "llCreateLink: no modify permission on target"
            else slogE info "llCreateLink: no permission to change links"
        continueV
    where parent = iparent /= 0

llDie info@(ScriptInfo oid _ _ pk _) [] =
     do  prohibited (getPrimAttachment oid) <??> "attachment cannot die"
         objects <- getObjectsE
         LSLObject { primKeys = (links) } <- mlookup oid objects
         allScripts <- mapM getPrimScripts links
         let primsWithScriptNames = zip links (map (map inventoryItemName) allScripts)
         let skeys = concatMap ( \ (k,list) -> map ((,)k) list) primsWithScriptNames
         mapM_ (uncurry delScriptE) skeys
         mapM_ (\ link -> liftM (M.delete link) getPrimsE >>= setPrimsE) links
         setObjectsE (M.delete oid objects)
         slogE info "object, and therefore this script, is dying"
         doneV
        
llAttachToAvatar info@(ScriptInfo oid _ sid pk _) [IVal attachPoint] =
    if attachPoint `elem` validAttachmentPoints then
        do  script <- getScriptE pk sid
            prohibited (getPrimAttachment oid) <??> "already attached"
            case scriptLastPerm script of
                Nothing -> throwError "no permission to attach"
                Just k -> do
                    perm <- mlookup k (scriptPermissions script)
                    when (perm .&. cPermissionAttach == 0) $ throwError "no permission to attach"
                    owner <- getPrimOwner pk
                    if k /= owner
                        then putChatE sayRange pk 0 "Script trying to attach to someone other than owner!"
                        else do 
                            avatars <- getWorldAvatarsE
                            av <- getWorldAvatar k
                            let attachments = avatarAttachments av
                            rotL maybe (\ _ -> throwError "attachment point already occupied") 
                                (IM.lookup attachPoint attachments) $
                                do setWorldAvatarE k av { avatarAttachments = IM.insert attachPoint oid attachments }
                                   setPrimAttachmentE oid (Just $ Attachment k attachPoint)
                                   pushAttachEventE pk k
            continueV
        else throwError ("invalid attachment point: " ++ show attachPoint)

llDetachFromAvatar info@(ScriptInfo oid _ sid pk _) [] =
    do  when (oid /= pk) $ throwError "can't detach from within child prim"
        script <- getScriptE pk sid
        attachment <- getPrimAttachment oid
        Attachment k attachPoint <- maybe (throwError "not attached") return attachment
        perm <- mlookup k (scriptPermissions script) <||> throwError "no permission to detach"
        when (perm .&. cPermissionAttach == 0) $ throwError "no permission to attach"
        avatar <- getWorldAvatar k
        let attachments = avatarAttachments avatar
        setWorldAvatarE k avatar { avatarAttachments = IM.delete attachPoint attachments }
        setPrimAttachmentE oid Nothing
        pushAttachEventE oid nullKey
        putWorldEventE 1 (DetachCompleteEvent oid k)
        continueV
    
llGetAttached info@(ScriptInfo oid _ _ _ _) [] = 
    (maybe 0 attachmentPoint) <$> (getPrimAttachment oid) >>= continueI

llGiveMoney info@(ScriptInfo _ _ sn pk _) [KVal ak, IVal amount] =
    do  script <- getScriptE pk sn
        permKey <- maybe (throwError "no permission") (return . id) (scriptLastPerm script)
        perm <- mlookup permKey (scriptPermissions script)
        when (perm .&. cPermissionAttach == 0) $ throwError "no permission"
        slogE info ("llGiveMoney: pretending to give " ++ show amount ++ " to avatar with key " ++ ak)
        continueI 0
    
llGetPermissionsKey (ScriptInfo _ _ sid pk _) [] =
    getScriptE pk sid >>= continueK . fromMaybe nullKey . scriptLastPerm
    
llGetPermissions (ScriptInfo _ _ sid pk _) [] = getPermissions pk sid >>= continueI

getPermissions pk s = do
    script <- getScriptE pk s
    maybe (return 0) (flip mlookup (scriptPermissions script)) (scriptLastPerm script)

getPermittedAvKey info@(ScriptInfo _ _ s pk _) fname perm = do
    script <- getScriptE pk s
    case scriptLastPerm script of
        Nothing -> slogE info (fname ++ ": not permitted") >> return Nothing
        Just k -> case M.lookup k (scriptPermissions script) of
            Nothing -> slogE info (fname ++ ": internal error getting permissions") >> return Nothing
            Just i | i .&. perm /= 0 -> return (Just k)
                   | otherwise -> return Nothing 
                   
llRequestPermissions (ScriptInfo _ _ sid pk _) [KVal k, IVal mask] = do
    maybe (logAMessageE LogInfo (pk ++ ":" ++ sid) ("Invalid permissions request: no such avatar: " ++ k))
       (const $ putWorldEventE 0 (PermissionRequestEvent pk sid k mask)) =<< (optional $ getWorldAvatar k)
    continueV

llClearCameraParams info@(ScriptInfo _ _ sid pk _) [] =
    do  avKey <- getPermittedAvKey info "llClearCameraParams" cPermissionControlCamera
        case avKey of
            Nothing -> return ()
            Just k -> do
                avatars <- getWorldAvatarsE
                av <- mlookup k avatars
                setWorldAvatarsE (M.insert k (av { avatarCameraControlParams = defaultCamera }) avatars)
        continueV
    
getAvParamsE f k = f <$> (getWorldAvatarsE >>= mlookup k)

llGetCameraPos info@(ScriptInfo _ _ sid pk _) [] =
    do  avKey <- getPermittedAvKey info "llGetCameraPos" cPermissionTrackCamera
        maybe (return (0,0,0)) (getAvParamsE avatarCameraPosition) avKey >>=
            continueVec

llGetCameraRot info@(ScriptInfo _ _ sid pk _) [] =
    do  avKey <- getPermittedAvKey info "llGetCameraRot" cPermissionTrackCamera
        maybe (return (0,0,0,1)) (getAvParamsE avatarCameraRotation) avKey
            >>= continueRot
 
llTakeCamera info _ = slogE info "llTakeCamera: deprecated!" >> continueV
llReleaseCamera info _ = slogE info "llRelaaseCamera: deprecated!" >> continueV
llPointAt info _ = slogE info "llPointAt: deprecated!" >> continueV
llStopPointAt info _ = slogE info "llStopPointAt: deprecated!" >> continueV

llSetPrimURL info _ = slogE info "llSetPrimURL: unimplemented!" >> continueV
llRefreshPrimURL info _ = slogE info "llRefreshPrimURL: unimplemted!" >> continueV

llGetRot info@(ScriptInfo oid _ _ pk _) [] = getGlobalRot pk >>= continueRot

llGetLocalRot info@(ScriptInfo oid _ _ pk _) [] =
    (if oid == pk then getObjectRotation oid  else getPrimRotation pk) 
        >>= continueRot

llGetRootRotation info@(ScriptInfo oid _ _ pk _) [] = 
    getObjectRotation oid >>= continueRot
    
--TODO: handle attachments...
--TODO: confirm order of rotations in both of these
llSetRot info@(ScriptInfo oid _ _ pk _) [r@(RVal _ _ _ _)] = 
    (if oid == pk then setRootRotation oid rot else setChildRotation oid pk rot) >> continueV
    where rot = rVal2Rot r

-- TODO: fix this!!
setChildRotation rk pk rot = do
    rootRot <- getPrimRotation rk
    setPrimRotationE pk (rot `quaternionMultiply` rootRot `quaternionMultiply` rootRot)

setRootRotation rk rot = do
    d <- getObjectDynamics rk
    setObjectDynamics rk d { objectRotation = rot }
        
llSetLocalRot info@(ScriptInfo oid _ _ pk _) [r@(RVal _ _ _ _)] =
    (if oid == pk then setRootRotation oid rot else setPrimRotationE pk rot) >> continueV
    where rot = rVal2Rot r
    
llSetPos info@(ScriptInfo oid pid sid pk event) [val] = 
    (if oid == pk then setRootPos oid v else setChildPos pk v) >> continueV
    where v = vVal2Vec val

setChildPos pk = lift . setPrimPosition pk
                
-- updates the world coordinates of object
-- does NOT consider region boundaries (TODO: fix!)
setRootPos oid v =
   do v0 <- getObjectPosition oid
      let vec = diff3d v v0
      let dist = mag3d vec
      let vec' = if dist <= 10.0 
              then vec
              else scale3d 10.0 $ norm3d vec
      d <- getObjectDynamics oid
      setObjectDynamics oid d { objectPosition = v0 `add3d` vec' }

llGetOwner info [] = 
    let k = scriptInfoPrimKey info in getPrimOwner k >>= continueK
llGetCreator info [] =
    let k = scriptInfoPrimKey info in getPrimCreator k >>= continueK
llSameGroup info@(ScriptInfo _ _ _ pk _) [KVal k] =
    do  group <- avGroup <||> getPrimGroup k <||> throwError ("no such object or avatar: " ++ k)
        myGroup <- getPrimGroup pk <||> throwError ("can't find " ++ pk)
        continueI (lslBool (group == myGroup))
    where avGroup = getAvParamsE avatarActiveGroup k

getLinkKey oid link = 
    lookupByIndex (link - 1) =<< primKeys <$> getObjectE oid

llGetLinkKey (ScriptInfo oid _ _ _ _) [IVal link] = 
    continueK =<< getLinkKey oid link

-- TODO: should check for av/prim in same region
-- TODO: no concept of online/offline for av           
llKey2Name info [KVal k] = --lift $
    (continueS =<< avatarName <$> getWorldAvatar k) <||>
    (continueS =<< primName <$> getPrimE k) <||> continueS ""

llOwnerSay info [SVal s] =
    do slogE info ("Owner Say: " ++ s)
       owner <- getPrimOwner (scriptInfoObjectKey info)
       putWorldEventE 0 (AvatarInputEvent owner (AvatarOwnerSay (scriptInfoPrimKey info) s))
       continueV
       
llGetGeometricCenter info@(ScriptInfo oid _ _ _ _) [] =
    do  links <- primKeys <$> getObjectE oid
        when (null links) $ throwError "error: empty linkset!"
        rootPos <- getObjectPosition oid
        foldM (\ v -> liftM (add3d v) . getGlobalPos) (0,0,0) links 
            >>= continueVec . scale3d (1 / fromIntegral (length links))

llGetMass info@(ScriptInfo oid _ _ _ _) [] = objectMass oid >>= continueF    

objectMass oid = do
    links <- primKeys <$> getObjectE oid
    foldM (\ v k -> getPrimE k >>= ( \ prim -> return $ v + primMassApprox prim)) 0.0 links
    
llGetObjectMass info [KVal k] =
    ((getWorldAvatar k >> return 80.0) <||> objectMass k) >>= continueF
    
llGetVel info@(ScriptInfo oid _ _ _ _) [] =
    continueVec =<< getObjectVelocity oid

llGetForce info@(ScriptInfo oid _ _ _ _) [] =
    getObjectDynamics oid >>= continueVec . fst . objectForce
        
llSetForce info@(ScriptInfo oid _ _ _ _) [v@(VVal x y z), IVal local] =
    do  d <- getObjectDynamics oid
        setObjectDynamics oid d { objectForce = ((x,y,z), local /= 0) }
        continueV

llSetBuoyancy info@(ScriptInfo { scriptInfoObjectKey = oid }) [FVal buoy] =
   do  d <- getObjectDynamics oid
       setObjectDynamics oid d { objectBuoyancy = buoy }
       continueV 
   
llApplyImpulse info@(ScriptInfo oid _ _ _ _) [v@(VVal x y z), IVal local] =
   do t <- getTickE
      d <- getObjectDynamics oid
      setObjectDynamics oid d { objectImpulse = (((x,y,z), local /= 0), t + durationToTicks 1) }
      continueV
        
llGetAccel info@(ScriptInfo oid _ _ _ _) [] = do
    prim <- getPrimE oid
    dyn <- getObjectDynamics oid
    let force = fst (objectForce dyn) `rot3d` rotation 
            where rotation = if snd $ objectForce dyn then objectRotation dyn else (0,0,0,1)
    let impulse = ((fst . fst . objectImpulse) dyn `rot3d` rotation, (snd . objectImpulse) dyn)
            where rotation = if (snd . fst . objectImpulse) dyn then objectRotation dyn else (0,0,0,1)
    continueVec =<< calcAccel <$> getTickE <*> pure 0 <*> objectMass oid <*> getObjectPosition oid <*>
        pure force <*> pure impulse
        
llSetTorque info@(ScriptInfo { scriptInfoObjectKey = oid }) [VVal x y z, IVal local] =
    do  d <- getObjectDynamics oid
        setObjectDynamics oid d { objectTorque = ((x,y,z),local /= 0) }
        continueV

llSetForceAndTorque info@(ScriptInfo { scriptInfoObjectKey = oid}) [VVal fx fy fz, VVal tx ty tz, IVal local] =
    do  d <- getObjectDynamics oid
        let loc = local /= 0
        setObjectDynamics oid d { 
              objectForce = ((fx,fy,fz),loc), objectTorque = ((tx,ty,tz),loc) }
        continueV

llGetTorque info@(ScriptInfo { scriptInfoObjectKey = oid }) [] =
    getObjectDynamics oid >>= continueVec . fst . objectTorque

llGetOmega info@(ScriptInfo { scriptInfoObjectKey = oid }) [] =
    getObjectDynamics oid >>= continueVec . objectOmega
    
llApplyRotationalImpulse info@(ScriptInfo { scriptInfoObjectKey = oid }) [VVal x y z, IVal local] =
    do  d <- getObjectDynamics oid
        t <- getTickE
        setObjectDynamics oid d { objectRotationalImpulse = (((x,y,z),local /= 0), t + durationToTicks 1) }
        continueV
   
llGetPos info@(ScriptInfo _ _ _ pk _) [] = getGlobalPos pk >>= continueVec 
llGetRootPosition info@(ScriptInfo oid _ _ _ _) [] = getObjectPosition oid >>= continueVec

getGlobalPos pk = do
        oid <- getRootPrim pk
        root <- getObjectPosition oid
        rel <- getPrimPosition pk
        rot <- getObjectRotation oid
        return (root `add3d` rot3d rel rot)

getGlobalRot pk =
    quaternionMultiply <$> getPrimRotation pk <*> (getObjectRotation =<< getRootPrim pk)
    
llRotLookAt info@(ScriptInfo oid _ _ _ _) [RVal x y z s, FVal strength, FVal tau] =
    rotLookAt oid (x,y,z,s) strength tau >> continueV

llLookAt info@(ScriptInfo oid _ _ _ _) [VVal x y z, FVal strength, FVal tau] =
    do  pos <- getObjectPosition oid
        rot <- getGlobalRot oid
        let v = diff3d (x,y,z) pos
        let rot1 = rot `quaternionMultiply` rotationBetween (rot3d (0,0,1) rot) v
        rotLookAt oid rot1 strength tau
        continueV
        
rotLookAt oid (x,y,z,s) strength tau = do
    d <- getObjectDynamics oid
    setObjectDynamics oid d { 
            objectRotationTarget = Just RotationTarget { 
                rotationTarget = (x,y,z,s),
                rotationTargetStrength = strength,
                rotationTargetTau = tau }}

llStopLookAt info@(ScriptInfo oid _ _ _ _) [] =
    do  d <- getObjectDynamics oid
        setObjectDynamics oid d { objectRotationTarget = Nothing }
        continueV
                                                                                     
llGetLocalPos info@(ScriptInfo oid _ _ pk _) [] =
    getPrimPosition pk >>= continueVec

llMoveToTarget info@(ScriptInfo { scriptInfoObjectKey = oid, scriptInfoScriptName = sn, scriptInfoPrimKey = pk }) [VVal x y z, FVal tau] =
    do  d <- getObjectDynamics oid
        case objectPositionTarget d of
            Nothing -> setTarg d
            Just (PositionTarget { positionTargetSetBy = scriptId }) 
                | scriptId == (pk,sn) -> setTarg d
                | otherwise -> logAMessageE LogInfo "llMoveToTarget" ("target already set by another script: " ++ show scriptId)
            Just _ -> setTarg d
        continueV                        
    where setTarg d = setObjectDynamics oid d { 
              objectPositionTarget = Just PositionTarget { 
                  positionTargetTau = tau,
                  positionTargetLocation = (x,y,z),
                  positionTargetSetBy = (pk,sn) }}

llStopMoveToTarget info@(ScriptInfo { scriptInfoObjectKey = oid }) [] = do
    d <- getObjectDynamics oid
    setObjectDynamics oid d { objectPositionTarget = Nothing }
    continueV
    
llGroundRepel info@(ScriptInfo { scriptInfoObjectKey = oid }) [FVal height, IVal water, FVal tau] =
    do  d <- getObjectDynamics oid
        setObjectDynamics oid d { 
                objectPositionTarget = Just Repel {
                    positionTargetTau = tau,
                    positionTargetOverWater = water /= 0,
                    positionTargetHeight = height }}
        continueV

llSetHoverHeight info@(ScriptInfo { scriptInfoObjectKey = oid }) [FVal height, IVal water, FVal tau] =
    do  d <- getObjectDynamics oid
        setObjectDynamics oid d { 
                objectPositionTarget = Just Hover {
                    positionTargetTau = tau,
                    positionTargetOverWater = water /= 0,
                    positionTargetHeight = height }}
        continueV

llStopHover info@(ScriptInfo { scriptInfoObjectKey = oid }) [] = do  
    d <- getObjectDynamics oid
    maybe (logAMessageE LogWarn "llStopHover" "not hovering")
        (const $ setObjectDynamics oid d { objectPositionTarget = Nothing })
            (objectPositionTarget d)
    continueV
    
runFace k i action = action <||> throwError ("face " ++ show i ++ " or prim " ++ k ++ " not found")

llGetAlpha (ScriptInfo _ _ _ pkey _) [IVal side] =
    computeAlpha >>= continueF
    where computeAlpha = if side /= -1 
            then runFace pkey side (getPrimFaceAlpha pkey side)
            else runFace pkey side $ do
                faces <- getPrimFaces pkey
                let n = length faces
                return $ if n > 0 then sum (map faceAlpha faces) / fromInt n
                                  else 1.0

llSetAlpha (ScriptInfo _ _ _ pkey _) [FVal alpha, IVal face] =
    setAlpha alpha face pkey

setAlpha alpha face pkey = lift $
    if face == -1
        then runErrFace pkey face (EvalIncomplete,VoidVal) $ do 
                faces <- getPrimFaces pkey
                let faces' = map (\ face -> face { faceAlpha = alpha }) faces
                setPrimFacesE pkey faces'
                continueV
        else setPrimFaceAlpha pkey face alpha >> continueV

llSetLinkAlpha (ScriptInfo oid pid _ _ _) [IVal link, FVal alpha, IVal face] = do
    mapM_ (setAlpha alpha face) =<< getTargetPrimKeysE oid link pid
    continueV

getTargetPrimKeysE oid link pid = lift $ do
    prims <- primKeys <$> 
        (runAndLogIfErr ("can't find object " ++ oid) (LSLObject [] defaultDynamics) $ getObjectE oid)
    let targetList = targetLinks (length prims) link pid
    mapM (flip lookupByIndex prims) targetList
    
llGetColor (ScriptInfo _ _ _ pkey _) [IVal side] =
    computeColor >>= continueVec
    where computeColor = if side /= -1
            then runFace pkey side (getPrimFaceColor pkey side)
            else runFace pkey side $ do
                faces <- getPrimFaces pkey
                let n = length faces
                return $ if n > 0 then scale3d (1.0/ fromInt (length faces) ) 
                                           (foldr add3d (0.0,0.0,0.0) (map faceColor faces))
                                  else (1.0,1.0,1.0)

llSetColor (ScriptInfo _ _ _ pkey _) [color, IVal face] = setColor color face pkey

setColor color face pkey =
    if face == -1
        then runFace pkey face $ do
                faces <- getPrimFaces pkey
                let colorVal = vVal2Vec color
                let faces' = map (\ face -> face { faceColor = colorVal }) faces
                setPrimFacesE pkey faces'
                continueV
        else lift $ setPrimFaceColor pkey face (vVal2Vec color) >> continueV

llSetLinkColor (ScriptInfo oid pid _ _ _) [IVal link, color, IVal face] = do
    mapM_ (setColor color face) =<< getTargetPrimKeysE oid link pid
    continueV

runFaceTextureQuery pk face query =
    runFace pk face (query <$> (getPrimFaceTextureInfo pk face'))
    where face' = if face == -1 then 0 else face
    
llGetTextureOffset (ScriptInfo _ _ _ pkey _) [IVal face] =
    continueVec =<< runFaceTextureQuery pkey face textureOffsets
llGetTextureScale (ScriptInfo _ _ _ pkey _) [IVal face] =
    continueVec =<< runFaceTextureQuery pkey face textureRepeats
llGetTextureRot (ScriptInfo _ _ _ pkey _) [IVal face] =
    continueF =<< runFaceTextureQuery pkey face textureRotation
        
llSetTexture info@(ScriptInfo _ _ _ pk _) [SVal texture,IVal face] = 
    do  tk <- findTexture pk texture
        setTexture tk face pk
        continueV
       
-- TODO: worry about texture permissions
llGetTexture (ScriptInfo _ _ _ pkey _) [IVal face] =
    let face' = if face == -1 then 0 else face in
        runFace pkey face (do
            info <- getPrimFaceTextureInfo pkey face'
            invTextures <- getPrimTextures pkey
            let tKey = textureKey info
            case findByInvKey tKey invTextures of
                 Nothing -> return tKey
                 Just item -> return $ inventoryItemName item) >>= continueS
                 
llSetLinkTexture info@(ScriptInfo oid pid _ pk _) [IVal link, SVal texture,IVal face] = do
    tk <- findTexture pk texture
    mapM_ (setTexture tk face) =<< getTargetPrimKeysE oid link pid
    continueV
    
updateTextureInfo update pk faceIndex =
    runFace pk faceIndex (
        if faceIndex == -1
            then do
                faces <- getPrimFaces pk
                let faces' = map (\ face ->
                        let info = faceTextureInfo face in
                            face { faceTextureInfo = update info }) faces
                setPrimFacesE pk faces'
            else do
                f <- getPrimFaces pk >>= lookupByIndex faceIndex
                let tInfo = update $ faceTextureInfo f
                updatePrimFace pk faceIndex (\ f -> f { faceTextureInfo = tInfo })
    ) >> continueV

llScaleTexture info@(ScriptInfo _ _ _ pk _) [FVal h,FVal v,IVal faceIndex] = 
    updateTextureInfo update pk faceIndex 
    where update info = info { textureRepeats = (h,v,0) }
llOffsetTexture (ScriptInfo _ _ _ pk _) [FVal h, FVal v, IVal faceIndex] =
    updateTextureInfo update pk faceIndex 
    where update info = info { textureOffsets = (h,v,0) }
llRotateTexture (ScriptInfo _ _ _ pk _) [FVal rot, IVal faceIndex] =
    updateTextureInfo update pk faceIndex 
    where update info = info { textureRotation = rot }
        
groupList _ [] = []
groupList n l | n > 0 = take n l : groupList n (drop n l)
              | otherwise = groupList 1 l

genNum :: [Int] -> Integer -> Integer
genNum rands maxval = 
    let rands' :: [Integer]
        rands' = map ((0x80000000-).toInteger) rands
        topval :: Rational
        topval = fromInteger $ 
            foldl' (.|.) 0 $ zipWith shiftL (replicate (length rands') 0xffffffff) [0,32..]
        randval :: Rational
        randval = fromInteger $ foldl' (.|.) 0 $ zipWith shiftL rands' [0,32]
    in floor ((randval / topval) * fromInteger maxval)
        
    
llListRandomize _ [LVal list, IVal stride] =
    let l1 = groupList stride list
        n = fac (toInteger $ length l1)
        randsNeeded = ceiling $ logBase 2 (fromInteger n) / 32
        wrands = replicate randsNeeded wrand
    in  lift $ do 
           rands <- sequence wrands
           let permutation = genNum rands n
           continueL $ generatePermutation list permutation
        
llGetNumberOfPrims (ScriptInfo oid _ _ pkey _) [] = 
    do (LSLObject { primKeys = prims }) <- getObjectE oid
       continueI (length prims)

llGetObjectPrimCount info@(ScriptInfo oid _ _ pkey _) [KVal k] =
    do  region <- getPrimRegion oid
        p <- getPrimE k
        region' <- getPrimRegion k
        if region /= region' 
            then continueI 0
            else getRootPrim k >>= primCount >>= continueI
    where primCount oid = do
            (LSLObject { primKeys = l}) <- getObjectE oid
            return $ length l

llGetNumberOfSides (ScriptInfo _ _ _ pkey _) [] =
     getPrimFaces pkey >>= continueI . length
     
llGetObjectDesc (ScriptInfo _ _ _ pkey _) [] =
    getPrimDescription pkey >>= continueS
llGetObjectName (ScriptInfo _ _ _ pkey _) [] =
    getPrimName pkey >>= continueS 

llSetObjectName (ScriptInfo _ _ _ pkey _) [SVal name] =
    lift $ setPrimName pkey (take 255 name) >> continueV
    
llSetObjectDesc (ScriptInfo _ _ _ pkey _) [SVal desc] =
    lift $ setPrimDescription pkey (take 127 desc) >> continueV
    
llGetObjectPermMask (ScriptInfo oid _ _ _ _) [IVal maskId] =
    getObjectPermMask oid maskId >>= continueI

getObjectPermMask oid maskId = do
       masks <- getPrimPermissions oid
       let base = if null masks then 0x0008e000 else head masks
       let n = length masks
       return (if maskId `elem` [0..(n-1)] then masks !! maskId else 
                      if maskId == 3 then 0 else base)
          
llGetScale (ScriptInfo _ _ _ pkey _) [] = continueVec =<< getPrimScale pkey
    
llSetScale (ScriptInfo _ _ _ pk _) [scale] =
    if tooSmall then continueV else lift2 setPrimScale pk clippedVec >> continueV
    where (x,y,z) = vVal2Vec scale
          tooSmall = x < 0.01 || y < 0.01 || z < 0.01
          clippedVec = (min x 10.0, min y 10.0, min z 10.0) 
  
llGetBoundingBox info [KVal k] =
    do  logAMessageE LogInfo "sim" "note: llGetBoundingBox does not return accurate results (yet)"
        withAvatar k notFound found
    where tup2l (x,y) = [x,y]
          found avatar = let pos = avatarPosition avatar in
                continueL $ map (vec2VVal . add3d pos) [(-1.0,-1.0,-1.0),(1.0,1.0,1.0)]
          notFound = getRootPrim k >>= getPrimBox >>= continueL . map vec2VVal . tup2l
    
getPrimBox pk = do
    pos <- getGlobalPos pk
    scale <- getPrimScale pk
    let (xs,ys,zs) = scale3d 0.5 scale
    return (add3d pos (-xs, -ys, -zs),add3d pos (xs,ys,zs))

llSetTimerEvent (ScriptInfo _ _ sn pk _) [FVal interval] =
    -- TODO: this may not accurately reflect buggy behavior in SL
    do lift2 removePendingTimerEvent pk sn
       when (interval > 0) $ putWorldEventE interval (TimerEvent interval (pk,sn))
       continueV
    where removePendingTimerEvent pk sn = do
              wq <- getWQueue
              let wq' = flip filter wq $ \ e -> case e of
                      (_,TimerEvent _ (pk',sn')) -> pk /= pk' || sn /= sn'
                      _ -> True
              setWQueue wq'
              
llSetVehicleFlags info@(ScriptInfo _ _ _ pk _) [IVal flagsToSet] =
    do  flags <- getPrimVehicleFlags pk `catchError` primErr pk
        lift $ setPrimVehicleFlags pk (flags .|. flagsToSet)
        continueV
    
llRemoveVehicleFlags info@(ScriptInfo _ _ _ pk _) [IVal flagsToClear] =
    do  flags  <- getPrimVehicleFlags pk `catchError` primErr pk
        lift $ setPrimVehicleFlags pk (flags .&. complement flagsToClear)
        continueV
    
----------------------------------------------------------------------
-- get/set prim parameters
llGetPrimitiveParams (ScriptInfo _ _ _ pk _) [LVal l] = getPrimParameters pk l >>= continueL
llSetPrimitiveParams (ScriptInfo _ _ _ pk _) [LVal l] = setPrimParameters pk l >> continueV

llSetLinkPrimitiveParams (ScriptInfo oid pid _ _ _) [IVal link,LVal l] =
    do  pks <- primKeys <$> getObjectE oid
        let targetList = targetLinks (length pks) link pid
        pkList <- mapM (flip lookupByIndex pks) targetList
        mapM_ (flip setPrimParameters l) pkList
        continueV
    
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
    where queryAndDoRest q = liftM2 (++) (q pk) . getPrimParameters pk

queryPrimTempOnRez = liftM ((:[]) . IVal . lslBool) . getPrimTempOnRez
queryPrimRotation = liftM ((:[]) . rot2RVal) . getGlobalRot
queryPrimPosition = liftM ((:[]) . vec2VVal) . getGlobalPos
queryPrimScale = liftM ((:[]) . vec2VVal) . getPrimScale
queryPrimFlexible k = 
    getPrimFlexibility k >>= (\ flex -> case flex of
        Nothing -> return [IVal 0, IVal 0, FVal 0, FVal 0, FVal 0, FVal 0, VVal 0.0 0.0 0.0]
        Just flex' -> return $ map ($ flex') [const (IVal 1),IVal . flexSoftness, FVal . flexGravity, FVal . flexFriction,
                                             FVal . flexWind, FVal . flexTension, vec2VVal . flexForce])
queryPrimLight k =
    getPrimLight k >>= (\ light -> case light of
        Nothing -> return [IVal 0, VVal 0 0 0, FVal 0, FVal 0, FVal 0]
        Just light' -> return $ map ($ light')
            [const (IVal 1), vec2VVal . lightColor, FVal . lightIntensity, FVal . lightRadius, FVal . lightFalloff])
    
queryPrimMaterial = liftM ((:[]) . IVal) . getPrimMaterial
queryPrimStatus bit =  liftM ((:[]) . IVal . (\ i -> lslBool (testBit i bit))) . getPrimStatus
queryPrimBumpShiny side =  queryFaceVals bumpShiny side
    where bumpShiny face = [IVal $ faceShininess face, IVal $ faceBumpiness face]
queryPrimColor side = queryFaceVals colorAlpha side
    where colorAlpha face = [vec2VVal $ faceColor face, FVal $ faceAlpha face]
queryPrimTexture side = queryFaceVals textureInfo side
    where textureInfo face = let tinfo = faceTextureInfo face in map ($ tinfo) 
                                   [SVal .textureKey,vec2VVal . textureRepeats,vec2VVal . textureOffsets,FVal . textureRotation]
queryPrimTexgen = queryFaceVals (return . IVal . faceTextureMode)
queryPrimFullbright = queryFaceVals (\ face -> if faceFullbright face then [IVal 1] else [IVal 0])

queryFaceVals f (IVal side) k = 
    if side == -1
        then (concatMap f) <$> (getPrimFaces k)
        else getPrimFaces k >>= fromErrorT [] . (liftM f . lookupByIndex side)

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
       i | IVal i `elem` [llcPrimTypeBox,llcPrimTypeCylinder,llcPrimTypePrism] -> 
                      return [IVal i,IVal holeshape, vec2VVal cut, FVal hollow, vec2VVal twist, vec2VVal taper, vec2VVal topshear]
         | IVal i == llcPrimTypeSphere -> return [IVal i,IVal holeshape, vec2VVal cut, FVal hollow, vec2VVal twist, vec2VVal advancedcut]
         | IVal i `elem` [llcPrimTypeRing,llcPrimTypeTorus,llcPrimTypeTube] -> 
                      return [IVal i,IVal holeshape, vec2VVal cut, FVal hollow, vec2VVal twist, vec2VVal holesize, vec2VVal topshear, 
                    vec2VVal advancedcut, vec2VVal taper, FVal revs, FVal roffset, FVal skew]
         | IVal i == llcPrimTypeSculpt -> return [IVal i,SVal $ fromMaybe "" sculpt, IVal sculptType]
         | otherwise -> return []

queryPrimTypeOldSkool k = do
    (PrimType version typecode holeshape cut twist holesize topshear hollow taper advancedcut roffset revs skew sculpt sculptType) <- 
        getPrimTypeInfo k
    case typecode of
       i | IVal i `elem` [llcPrimTypeBox,llcPrimTypeCylinder,llcPrimTypePrism] -> 
                      return [IVal i, vec2VVal cut, FVal hollow, FVal $ yOf twist, vec2VVal taper, vec2VVal topshear]
         | IVal i == llcPrimTypeSphere -> return [IVal i,vec2VVal cut, FVal hollow,vec2VVal advancedcut]
         | IVal i == llcPrimTypeTorus -> 
                      return [IVal i, vec2VVal cut, FVal hollow, FVal $ yOf twist, FVal $ yOf taper, vec2VVal topshear, vec2VVal advancedcut]
         | IVal i == llcPrimTypeTube -> 
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
          | otherwise -> throwError "incorrect parameter"
    updatePrimParameters pk result
    
badParms = throwError . ("insufficient or incorrect parameters for " ++)

updatePrimType pk vals = getPrimE pk >>= flip updatePrimType' vals >>= (\ (prim,rest) -> setPrimE pk prim >> return rest)
updatePrimType' prim (primCode@(IVal i):rest) | primCode == llcPrimTypeSphere = updatePrimTypeSphere prim rest
                                              | primCode `elem` [llcPrimTypeBox,llcPrimTypeCylinder,llcPrimTypePrism] =
                                                  updatePrimTypeBoxCylPrism i prim rest
                                              | primCode `elem` [llcPrimTypeTorus,llcPrimTypeTube,llcPrimTypeRing] =
                                                  updatePrimTypeRingTorusTube i prim rest
                                              | primCode == llcPrimTypeSculpt = updatePrimTypeSculpt prim rest
                                              | otherwise = badParms "PRIM_TYPE"
updatePrimType' _ _ = badParms "PRIM_TYPE"

updatePrimTypeOldSkool pk vals = getPrimE pk >>= flip updatePrimTypeOldSkool' vals >>= (\ (prim,rest) -> setPrimE pk prim >> return rest)
updatePrimTypeOldSkool' prim (primCode@(IVal i):rest) | primCode == llcPrimTypeSphere = updatePrimTypeSphereOld prim rest
                                                      | primCode `elem` [llcPrimTypeBox,llcPrimTypeCylinder,llcPrimTypePrism] =
                                                          updatePrimTypeBoxCylPrismOld i prim rest
                                                      | primCode == llcPrimTypeTorus = updatePrimTypeTorusOld prim rest
                                                      | primCode == llcPrimTypeTube = updatePrimTypeTubeOld prim rest
                                                      | otherwise = badParms "deprecated prim type"
updatePrimTypeOldSkool' _ _ = badParms "deprecated prim type"

updatePrimTempOnRez pk (IVal tempOnRez:rest) = lift (setPrimTempOnRez pk (tempOnRez /= 0)) >> return rest
updatePrimTempOnRez _ _ = badParms "PRIM_TEMP_ON_REZ"

updatePrimMaterial pk (IVal material:rest) = lift (setPrimMaterial pk material) >> return rest
updatePrimMaterial _ _ = throwError "insufficient or incorrect parameters for PRIM_MATERIAL"

updatePrimPhantom = updatePrimStatus "insufficient or incorrect parameters for PRIM_PHANTOM" primPhantomBit
updatePrimPhysics = updatePrimStatus "insufficient or incorrect parameters for PRIM_PHYSICS" primPhysicsBit

updatePrimStatus _ bit pk (IVal i:rest) = 
    getPrimStatus pk >>= lift . setPrimStatus pk . (if i == 0 then flip clearBit bit
                                                              else flip setBit bit) >> return rest
updatePrimStatus fMsg _ _ _ = throwError fMsg

updatePrimPosition pk (pos@(VVal _ _ _):rest) =
    do  getPrimParent pk >>= \ mok -> case mok of
                Nothing -> setRootPos pk v
                Just ok -> setChildPos pk v
        return rest
    where v = vVal2Vec pos
updatePrimPosition _ _ = badParms "PRIM_POSITION"

updatePrimRotation pk (rot@(RVal _ _ _ _):rest) =
    do getPrimParent pk >>= \ mok -> case mok of
            Nothing -> setRootRotation pk r
            Just rk -> setChildRotation rk pk r
       return rest
    where r = rVal2Rot rot
updatePrimRotation _ _ = badParms "PRIM_ROTATION"
updatePrimScale pk (scale@(VVal _ _ _):rest) = lift2 setPrimScale pk (vVal2Vec scale) >> return rest
updatePrimScale _ _ = badParms "PRIM_SIZE"

updatePrimFlexible pk (IVal flex:IVal soft:FVal gravity:FVal friction:FVal wind:FVal tension:VVal fx fy fz:rest) =
    lift (setPrimFlexibility pk (if flex == 0 then Nothing else Just (Flexibility soft gravity friction wind tension (fx,fy,fz)))) >> return rest
updatePrimFlexible _ _ = badParms "PRIM_FLEXIBLE"

updatePrimLight pk (IVal light:VVal r g b:FVal intensity:FVal radius:FVal falloff:rest) =
    lift2 setPrimLight pk (if light == 0 then Nothing else Just (LightInfo (r,g,b) intensity radius falloff)) >> 
    return rest
updatePrimLight _ _ = badParms "PRIM_POINT_LIGHT"

updatePrimBumpShiny pk params =
    let extract (IVal face:IVal bump:IVal shiny:rest) = return (face, [IVal bump, IVal shiny], rest)
        extract _ = badParms "PRIM_BUMP_SHINY"
        update [IVal bump, IVal shiny] face = face { faceBumpiness = bump, faceShininess = shiny }
    in updatePrimFaceParams pk params extract update
updatePrimColor pk params =
    let extract (IVal face:VVal r g b:FVal alpha:rest) = return (face,[VVal r g b, FVal alpha], rest)
        extract _ = badParms "PRIM_COLOR"
        update [color, FVal alpha] face = face { faceColor = vVal2Vec color, faceAlpha = alpha }
    in updatePrimFaceParams pk params extract update
updatePrimTexture pk params =
    let extract (IVal face:name@(SVal _):repeats@(VVal _ _ _):offsets@(VVal _ _ _):rotation@(FVal _):rest) = 
            return (face,[name,repeats,offsets,rotation],rest)
        extract _ = badParms "PRIM_TEXTURE"
        update [SVal name,repeats,offsets,FVal rotation] face = 
            face { faceTextureInfo = TextureInfo name (vVal2Vec repeats) (vVal2Vec offsets) rotation }
    in updatePrimFaceParams pk params extract update
updatePrimTexgen pk params =
    let extract (IVal face:IVal mode:rest) = return (face,[IVal mode],rest)
        extract _ = badParms "PRIM_TEXGEN"
        update [IVal mode] face = face { faceTextureMode = mode }
    in updatePrimFaceParams pk params extract update
updatePrimFullbright pk params =
    let extract (IVal face:IVal fullbright:rest) = return (face,[IVal fullbright],rest)
        extract _ = badParms "PRIM_FULLBRIGHT"
        update [IVal fullbright] face = face { faceFullbright = fullbright /= 0 }
    in updatePrimFaceParams pk params extract update
    
updatePrimFaceParams pk params extract update = do 
    (face, faceParams, rest) <- extract params -- this can fail
    prim <- getPrimE pk
    let prim' = if face == -1
                   then prim { primFaces = map (update faceParams) $ primFaces prim }
                   else let (xs,ys) = splitAt face (primFaces prim) in
                       if null ys then prim else prim { primFaces = xs ++ [(update faceParams $ head ys)] ++ tail ys }
    setPrimE pk prim'
    return rest
    
updatePrimTypeBoxCylPrism ptype prim (IVal holeshape:VVal cx cy cz:FVal hollow:VVal twx twy twz:VVal tx ty tz:VVal sx sy sz:rest) =
    return (prim { primTypeInfo = (primTypeInfo prim) { primTypeCode = ptype, primHoleshape = holeshape, primCut = (cx,cy,cz),
                                                primHollow = hollow, primTwist = (twx,twy,twz), primTaper = (tx,ty,tz),
                                                primTopshear = (sx,sy,sz) }}, rest)
updatePrimTypeBoxCylPrism _ _ _ = 
    badParms "PRIM_TYPE (PRIM_TYPE_PRISM, PRIM_TYPE_CYLINDER, or PRIM_TYPE_BOX)"
updatePrimTypeSphere prim (IVal holeshape:VVal cx cy cz:FVal hollow:VVal tx ty tz:VVal ax ay az:rest) =
    return (prim { primTypeInfo = (primTypeInfo prim) { primTypeCode = cPrimTypeSphere, primHoleshape = holeshape, primCut = (cx,cy,cz),
                                                primHollow = hollow, primTwist = (tx,ty,tz), primAdvancedCut = (ax,ay,az) } }, rest)
updatePrimTypeSphere _ _ = badParms "PRIM_TYPE (PRIM_TYPE_SPHERE)"
updatePrimTypeRingTorusTube ptype prim (IVal holeshape:VVal cx cy cz:FVal hollow:VVal twx twy twz:VVal hx hy hz:VVal sx sy sz:
                                        VVal ax ay az:VVal tx ty tz:FVal revs:FVal roffset:FVal skew:rest) =
    return (prim { primTypeInfo = (primTypeInfo prim) { primTypeCode = ptype, primHoleshape = holeshape, primCut = (cx,cy,cz),
                                                primHollow = hollow, primTwist = (twx,twy,twz), primHolesize = (hx,hy,hz),
                                                primTopshear = (sx,sy,sz), primAdvancedCut = (ax,ay,az), primTaper = (tx,ty,tz),
                                                primRevolutions = revs, primRadiusOffset = roffset, primSkew = skew } }, rest)
updatePrimTypeRingTorusTube _ _ _ = 
    badParms "PRIM_TYPE (PRIM_TYPE_RING, PRIM_TYPE_TORUS, or PRIM_TYPE_TUBE)"
updatePrimTypeSculpt prim (SVal name:IVal sculptType:rest) =
    return (prim { primTypeInfo = 
        (primTypeInfo prim) { 
            primTypeCode = cPrimTypeSculpt, primSculptTexture = Just name, primSculptType = sculptType } }, rest)
updatePrimTypeSculpt _ _ = badParms "PRIM_TYPE (PRIM_TYPE_SCULPT)"

updatePrimTypeBoxCylPrismOld ptype prim (VVal cx cy cz:FVal hollow:FVal twisty:VVal tx ty tz:VVal sx sy sz:rest) =
    let info = primTypeInfo prim in
    return (prim { primTypeInfo = info { primTypeCode = ptype, primCut = (cx,cy,cz), primHollow = hollow,
                                         primTwist = let (x,_,z) = primTwist info in (x,twisty,z), primTaper = (tx,ty,tz),
                                         primTopshear = (sx,sy,sz) } }, rest)
updatePrimTypeBoxCylPrismOld _ _ _ = 
    badParms "deprecated prim type (PRIM_TYPE_BOX, PRIM_TYPE_CYLINDER, or PRIM_TYPE_PRISM)"

updatePrimTypeSphereOld prim (VVal cx cy cz:FVal hollow:VVal ax ay az:rest) =
    return (prim { primTypeInfo = (primTypeInfo prim) { primTypeCode = cPrimTypeSphere, primCut = (cx,cy,cz), primHollow = hollow,
                                                        primAdvancedCut = (ax,ay,az) }}, rest)
updatePrimTypeSphereOld _ _ = badParms "deprecated prim type (PRIM_TYPE_SPHERE)"

updatePrimTypeTorusOld prim (VVal cx cy cz:FVal hollow:FVal twisty:FVal tapery:VVal sx sy sz:VVal ax ay az:rest) =
    let info = primTypeInfo prim in
    return (prim { primTypeInfo = info { primTypeCode = cPrimTypeTorus, primCut = (cx,cy,cz), primHollow = hollow,
                                         primTwist = let (x,_,z) = primTwist info in (x,twisty,z),
                                         primTaper = let (x,_,z) = primTaper info in (x,tapery,z),
                                         primTopshear = (sx,sy,sz), primAdvancedCut = (ax,ay,az) } }, rest)
updatePrimTypeTorusOld _ _ = badParms "deprecated prim type (PRIM_TYPE_TORUS)"

updatePrimTypeTubeOld prim (VVal cx cy cz:FVal hollow:FVal twisty:FVal shearx:rest) =
    let info = primTypeInfo prim in
    return (prim { primTypeInfo = info { primTypeCode = cPrimTypeTube, primCut = (cx,cy,cz), primHollow = hollow,
                                         primTwist = let (x,_,z) = primTwist info in (x,twisty,z),
                                         primTopshear = let (_,y,z) = primTopshear info in (shearx,y,z) } }, rest)
updatePrimTypeTubeOld _ _ = badParms "deprecated prim type (PRIM_TYPE_TUBE)"

llGetObjectDetails _ [KVal k, LVal params] =
     -- TODO: this doesn't take into account multiple regions
     (getAvatarDetails k params <||> getPrimDetails k params) >>= continueL
     where getAvatarDetails k params = 
               (map . avq) <$> (getWorldAvatar k) <*> pure params
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
               do  prim <- getPrimE k
                   pos <- getObjectPosition k
                   rot <- getObjectRotation k
                   vel <-  getObjectVelocity k
                   return $ map (primq prim pos rot vel) params
               where primq prim pos rot vel i | i == llcObjectName = SVal $ primName prim
                                              | i == llcObjectDesc = SVal $ primDescription prim
                                              | i == llcObjectPos = vec2VVal pos
                                              | i == llcObjectRot = rot2RVal rot
                                              | i == llcObjectVelocity = vec2VVal vel
                                              | i == llcObjectOwner = KVal $ primOwner prim
                                              | i == llcObjectGroup = KVal nullKey            -- TODO: prim groups
                                              | i == llcObjectCreator = KVal $ primOwner prim -- TODO: prim creators
                                              | otherwise = llcObjectUnknownDetail

--------------------------------------------------------------------------------------------------------------
llAllowInventoryDrop info@(ScriptInfo _ _ _ k _) [IVal add] = do
    (\ p -> p { primAllowInventoryDrop = add /= 0 }) <$> (getPrimE k) >>= setPrimE k
    slogE info ("drop is now " ++ (if add == 0 then "not " else "") ++ "allowed")
    continueV
        
llAdjustSoundVolume info [FVal f] = do
    slogE info ("llAdjustSoundVolume: volume " ++ 
        if f < 0.0 || f > 1.0 then show f ++ " out of range." else "adjusted to " ++ show f)
    continueV
    
-----------------------------------------------------------------------------------------------------------------
-- Parcel related functions                                  

llSetParcelMusicURL info@(ScriptInfo _ _ _ pk _) [SVal url] =
     -- parcel music URL is a write-only value, so we won't worry about setting anything....
     do  -- make sure object owner == parcel owner
        (_,_,parcel) <- getPrimParcel pk
        owner <- getPrimOwner pk
        when (parcelOwner parcel /= owner) $ throwError "prim doesn't have permission to modify parcel"
        slogE info ("setting music url for parcel " ++ parcelName parcel ++ " to " ++ url)
        continueV
        
llGetParcelFlags info@(ScriptInfo _ _ _ pk _) [VVal x y z] =
    (continueI . parcelFlags . snd) =<< 
        (getParcelByPosition <<= getPrimRegion pk <<= return (x,y,z))
   
llGetParcelDetails info@(ScriptInfo _ _ _ pk _) [VVal x y z, LVal details] =
     do  parcel <- snd <$> (getParcelByPosition =<< getPrimRegion pk) (x,y,z)
         detailList <- mapM (getDetail parcel) details
         continueL [ d | Just d <- detailList]
     where getDetail parcel (IVal detail) 
               | detail == cParcelDetailsName = return $ Just $ SVal $ parcelName parcel
               | detail == cParcelDetailsDesc =  return $ Just $ SVal $ parcelDescription parcel
               | detail == cParcelDetailsOwner = return $ Just $ KVal $ parcelOwner parcel
               | detail == cParcelDetailsGroup = return $ Just $ KVal nullKey
               | detail == cParcelDetailsArea = return $ Just $ FVal $ fromIntegral $ 
                   let (bot,top,left,right) = parcelBoundaries parcel in (top - bot) * (right - left) 
               | otherwise = slogE info ("llGetParcelDetails: invalid detail flag: " ++ show detail) >> return Nothing
           getDetail _ v = slogE info ("llGetParcelDetails: invalid detail: " ++ lslValString v) >> return Nothing

whenParcelPermitted info pk action= do
    (regionIndex,parcelIndex,parcel) <- getPrimParcel pk
    prim <- getPrimE pk
    if parcelOwner parcel /= primOwner prim then slogE info "prim not permitted to change parcel"
        else action regionIndex parcelIndex parcel >>= putParcel regionIndex parcelIndex
        
addToLandACLList aclType aclFromParcel aclIntoParcel info@(ScriptInfo _ _ _ pk _) [KVal ak,FVal duration] = do
    whenParcelPermitted info pk $ \ regionIndex parcelIndex parcel ->
        lift $ runAndLogIfErr ("attempt to change " ++ aclType ++ " unknown avatar " ++ ak) parcel $ do
            avs <- getWorldAvatarsE
            _ <- mlookup ak avs
            when (duration < 0.0) $ slogE info (aclType ++ " change attempted with invalid duration: " ++ show duration)
            t <- getTickE
            let acl = (ak, if duration == 0 then Nothing else Just $ t + durationToTicks duration)
            let acllist = acl : ([ b | b@(k,Just expire) <- aclFromParcel parcel,expire < t, k /= ak] ++
                                 [ b | b@(k,Nothing) <- aclFromParcel parcel, k /= ak])
            let parcel' = aclIntoParcel parcel acllist
            slogE info ("added " ++ ak ++ " to " ++ aclType ++ " list for parcel in " ++ show regionIndex)
            return parcel'
    continueV
            
removeFromLandACLList aclFromParcel aclIntoParcel info@(ScriptInfo _ _ _ pk _) ak =
    whenParcelPermitted info pk $ \ regionIndex parcelIndex parcel -> do
        getWorldAvatar ak
        let acl = aclFromParcel parcel
        return $ aclIntoParcel parcel [ ac | ac@(k,_) <- acl, k /= ak ]

llResetLandBanList info@(ScriptInfo _ _ _ pk _) [] =
    do  whenParcelPermitted info pk (\ regionIndex parcelIndex parcel -> return $ parcel { parcelBanList = [] })
        continueV
llResetLandPassList info@(ScriptInfo _ _ _ pk _) [] =
    do  whenParcelPermitted info pk  (\ regionIndex parcelIndex parcel -> return $ parcel { parcelPassList = [] })
        continueV

llRemoveFromLandPassList info [KVal ak] =
    let aclFromParcel = parcelPassList
        aclIntoParcel = (\ parcel list -> parcel { parcelPassList = list})
    in do removeFromLandACLList aclFromParcel aclIntoParcel info ak
          slogE info ("llRemovFromLandPassList: removed " ++ ak)
          continueV

llRemoveFromLandBanList info [KVal ak] =
    let aclFromParcel = parcelBanList
        aclIntoParcel = (\ parcel list -> parcel { parcelBanList = list})
    in do removeFromLandACLList aclFromParcel aclIntoParcel info ak
          slogE info ("llRemovFromLandBanList: removed " ++ ak)
          continueV
              
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
        getParcelByPosition regionIndex (x,y,z) >>= continueK . parcelOwner . snd
    
llOverMyLand info@(ScriptInfo _ _ _ pk _) [KVal ak] =
    do  regionIndex <- getPrimRegion pk
        owner <- getPrimOwner pk
        (do av <- getWorldAvatar ak
            if avatarRegion av /= regionIndex 
                then slogE info "llOverMyLand: avatar not in sim" >> continueI 0
                else do
                    parcel <- getParcelByPosition regionIndex (avatarPosition av)
                    continueI $ lslBool (owner == (parcelOwner . snd) parcel)
         ) <||> (slogE info "llOverMyLand: no such avatar" >> continueI 0)
                    
-------------------------------------------------------------------------------
-- EMAIL
llGetNextEmail info@(ScriptInfo _ _ sn pk _) [SVal addr, SVal subj] =
   do  emails <- getPrimPendingEmails pk
       case find match emails of
           Nothing -> return ()
           Just email -> do
                pushDeferredScriptEventE (Event "email" params M.empty) pk sn 0
                lift $ setPrimPendingEmails pk (filter (not . match) emails)
                where params = [SVal $ show $ emailTime email, 
                                SVal $ emailAddress email,
                                SVal $ emailSubject email, 
                                SVal $ emailMessage email,
                                IVal (length emails -1)]
       continueV
   where match email = (addr == "" || addr == emailAddress email) &&
                       (subj == "" || subj == emailSubject email)

llEmail info@(ScriptInfo _ _ _ pk _) [SVal address, SVal subject, SVal message] =
    do  let suffix = "@lsl.secondlife.com" 
        let logit = slogE info ("llEmail: sending email to " ++ address)
        if suffix `isSuffixOf` address
           then let potentialKey = take (length address - length suffix) address in
               do pending <- getPrimPendingEmails potentialKey <||> throwError "LSL destination addresses unknown prim"
                  time <- lift getUnixTime
                  lift $ setPrimPendingEmails pk (pending ++ [Email subject address message time])
                  logit
           else logit
        continueV -- should do a yield til... (20 second delay!)
-------------------------------------------------------------------------------
soundExists pk sound = do
    sounds <- getPrimSounds pk
    case findByInvName sound sounds of
        Nothing -> do
            result <- lift $ findAsset sound
            case result of
                Nothing -> return False
                Just v -> return $ isSoundAsset v
        Just _ -> return True
       
llTriggerSound info@(ScriptInfo _ _ _ pk _) [SVal sound, FVal volume] =
    do  found <- soundExists pk sound
        let message = 
                (if found then "llTriggerSound: triggering sound " 
                         else "llTriggerSound: sound not found - ") ++ sound
        slogE info message
        continueV
        
onSound info@(ScriptInfo _ _ _ pk _) fn msg sound = do
    found <- soundExists pk sound
    slogE info (fn ++ ":" ++ (if found then msg else "sound not found -") ++ 
        " " ++ sound)
    continueV
    
llTriggerSoundLimited info [SVal sound, FVal volume, VVal t n e, VVal b s w] =
    do  when (t <= b || n <= s || e <= w) $ throwError 
            "llTriggerSoundLimited: bounding box has zero or negative volume"
        onSound info "llTriggerSoundLimited" "triggering sound" sound
    
llSound info [SVal sound, FVal vol, IVal q, IVal loop] =
    onSound info "llSound (deprecated):" "playing sound " sound

llPlaySound info [SVal sound, FVal vol] =
    onSound info "llPlaySound" "playing sound"  sound
    
llCollisionSound info [SVal sound, FVal vol] =
    onSound info "llCollisionSound" "setting collision sound to" sound
    
llPreloadSound info [SVal sound] =
    onSound info "llPreloadSound" "loading sound" sound
    
llSoundPreload info [SVal sound] =
    onSound info "llSoundPreload" "loading sound" sound
    
llPlaySoundSlave info [SVal sound, FVal vol] =
    onSound info "llPlaySoundSlave" "playing sound" sound

llLoopSoundSlave info [SVal sound, FVal vol] =
    onSound info "llLoopSoundSlave" "playing sound" sound

llLoopSound info [SVal sound, FVal vol] =
    onSound info "llLoopSound" "playing sound" sound

llLoopSoundMaster info [SVal sound, FVal vol] =
    onSound info "llLoopSoundMaster" "playing sound" sound

llStopSound info [] = slogE info "llStopSound: stopping sound" >> continueV
llSetSoundRadius info [FVal radius] = 
    slogE info "llSetSoundRadius: called. This LL function has no effect." >>
        continueV
llSetSoundQueueing info [IVal bool] = 
    slogE info ("llSetSoundQueuiing: set to " ++ 
        (if bool /= 0 then "TRUE" else "FALSE")) >> continueV

------------------------------------------------------------------------------
llCollisionSprite info@(ScriptInfo _ _ _ pk _) [SVal impactSprite] =
    do  tk <- findTexture pk impactSprite
        slogE info ("llCollisionSprite: set sprite to " ++ tk)
        continueV
--

llGetKey (ScriptInfo _ _ _ pk _) [] = continueK pk

llGetOwnerKey info@(ScriptInfo _ _ _ pk _) [KVal k] = 
   (do  regionIndex <- getPrimRegion pk
        mRegionIndex <- Just <$> (getPrimRegion k) <||> return Nothing
        key <- case mRegionIndex of 
            Nothing -> 
                slogE info "llGetOwnerKey: object key not found" >> return k
            Just regionIndex' | regionIndex /= regionIndex' -> 
                slogE info "llGetOwnerKey: object in different simulator" >> return k
                              | otherwise -> getPrimOwner k
        continueK key) <||>
   (do getWorldAvatar k <||> throwError "no such key"
       continueK k)
    
llGetLinkNumber (ScriptInfo oid pid _ pk _) [] =
     if pid /= 0 then continueI (pid + 1) else
         do  links <- primKeys <$> getObjectE oid
             continueI (if length links == 1 then 0 else 1)
        
llGetUnixTime (ScriptInfo _ _ _ _ _) [] = lift getUnixTime >>= continueI

llGetTimestamp (ScriptInfo _ _ _ _ _) [] = lift $ do
    cal <- toUTCTime <$> getTimeOfDay
    continueS $ printf "%04d-%02d-%02dT%02d:%02d:%02.6f"
         (ctYear cal) (1 + fromEnum (ctMonth cal)) (ctDay cal) (ctHour cal) (ctMin cal) 
         (fromIntegral (ctSec cal) / (10.0^12 * fromIntegral (ctPicosec cal)) :: Float)
         
llGetDate (ScriptInfo _ _ _ _ _) [] = lift (getUTCDate >>= continueS)
llGetGMTclock (ScriptInfo _ _ _ _ _) [] = lift $ do
    cal <- toUTCTime <$> getTimeOfDay
    continueI $ (24 * ctHour cal) + (60 * ctMin cal) + ctSec cal
    
llGetWallclock (ScriptInfo _ _ _ _ _) [] = lift $ do
    cal <- toUTCTime <$> (getLocalTimeOfDay (-8))
    continueI $ (24 * ctHour cal) + (60 * ctMin cal) + ctSec cal
        
llGetTimeOfDay (ScriptInfo _ _ _ _ _) [] =
    (continueF . ticksToDuration . flip mod (durationToTicks 4.0)) =<< getTickE
    
getUnixTime :: (Monad (StateT (World a) a), Monad a) => StateT (World a) a Int
getUnixTime = fromIntegral <$> (liftM2 (+) getWorldZeroTime (liftM (floor . ticksToDuration) getTick))
getTimeOfDay :: (Monad (StateT (World a) a), Monad a) => StateT (World a) a ClockTime
getTimeOfDay = (flip TOD 0 . fromIntegral) <$> getUnixTime

getLocalTimeOfDay offset =  (addToClockTime (TimeDiff 0 0 0 offset 0 0 0)) <$> getTimeOfDay
    
getUTCDate :: (Monad (StateT (World a) a), Monad a, PrintfType (Int -> Int -> Int -> b)) => StateT (World a) a b
getUTCDate = do
    cal <- toUTCTime <$> getTimeOfDay
    return $ printf "%04d-%02d-%02d" (ctYear cal) (1 + fromEnum (ctMonth cal)) (ctDay cal)    

llGetRegionFPS _ _ = continueF 45.0
llGetRegionTimeDilation _ _ = continueF 1.0

llGetTime (ScriptInfo _ _ sid pk _) [] = 
     do  t <- getTickE
         reset <- scriptLastResetTick <$> getScriptE pk sid
         continueF $ ticksToDuration (t - reset)

getAndResetTick pk sid = do
    script <- getScriptE pk sid
    let t = scriptLastResetTick script
    t' <- getTickE
    setScriptE pk sid script { scriptLastResetTick = t' }
    return $ t' - t
    
llGetAndResetTime (ScriptInfo _ _ sid pk _) [] =
    getAndResetTick pk sid >>= continueF . ticksToDuration
    
llResetTime (ScriptInfo _ _ sid pk _) [] =
    getAndResetTick pk sid >> continueV
    
llSetText info [SVal text, VVal r g b, FVal alpha] = setTextE "llSetText" 254 info text
llSetSitText info [SVal text] = setTextE "llSetSitText" 9 info text
llSetTouchText info [SVal text] = setTextE "llSetTouchText" 9 info text

setTextE func lim info text =
    do slogE info (func ++ ": setting text to " ++ text)
       when (length text > lim) $ slogE info (func ++ ": text exceeds " ++ show lim ++ " character limit")
       continueV

llGetScriptName (ScriptInfo _ _ sid _ _) [] = continueS sid

llGetNumberOfNotecardLines (ScriptInfo _ _ sn pk _) [SVal name] =
    do  notecards <- getPrimNotecards pk
        case find ((name==) . inventoryItemName) notecards of
            Nothing -> putChatE sayRange pk 0 ("Couldn't find notecard " ++ name) >> continueK nullKey
            Just notecard -> do
                key <- newKeyE
                pushDataserverEventE pk sn key (show $ length $ invNotecardLines $ inventoryItemData notecard)
                continueK key
    
llGetNotecardLine (ScriptInfo _ _ sn pk _) [SVal name, IVal lineNumber] =
    do  notecards <- getPrimNotecards pk
        case find ((name==) . inventoryItemName) notecards of
            Nothing -> sayErr pk ("Couldn't find notecard " ++ name) >> continueK nullKey
            Just notecard -> do
                key <- newKeyE
                pushDataserverEventE pk sn key $ maybe cEOF (take 255) $
                    lookupByIndex lineNumber $ invNotecardLines $ inventoryItemData notecard
                continueK key
 
llRequestInventoryData info@(ScriptInfo _ _ sn pk _) [SVal name] =
    do landmarks <- getPrimLandmarks pk
       landmark <- maybe (throwError ("no landmark named " ++ name)) return (findByInvName name landmarks)
       let (_,v) = invLandmarkLocation $ inventoryItemData landmark
       t <- getTickE
       key <- newKeyE
       pushDataserverEventE pk sn key (lslValString (vec2VVal v))
       yieldWith (t + durationToTicks 1) (KVal key)
    
llGetLinkName (ScriptInfo oid _ _ _ _) [IVal linkNumber] =
    do  links <- primKeys <$> getObjectE oid
        name <- if null links
           then if linkNumber == 0
               then getPrimName oid
               else return nullKey
           else case lookupByIndex (linkNumber - 1) links of
                Nothing -> return nullKey
                Just k -> getPrimName k
        continueS name
    
llGetStatus (ScriptInfo _ _ _ pk _) [IVal check] =
    do  status <- getPrimStatus pk
        continueI $ case mssb check of
            Nothing -> 0
            Just b -> lslBool (bit b .&. status /= 0)
    where mssb i = foldl' (\ x y -> x `mplus` (if testBit i y then Just y else Nothing)) Nothing [31,30..0]

llSetStatus info@(ScriptInfo _ _ _ pk _) [IVal mask, IVal val] =
    do  status <- getPrimStatus pk
        let status' = if val == 0 
                          then status .&. complement mask
                          else status .|. mask
        lift2 setPrimStatus pk status'
        continueV
    
llPassTouches info@(ScriptInfo _ _ _ pk _) [IVal val] =
   lift2 setPrimPassTouches pk (val /= 0) >> continueV
   
llPassCollisions info@(ScriptInfo _ _ _ pk _) [IVal val] =
   lift2 setPrimPassCollisions pk (val /= 0) >> continueV
   
llGetRegionCorner (ScriptInfo _ _ _ pk _) [] = 
    do (x,y) <- getPrimRegion pk
       continueVec (256 * fromIntegral x, 256 * fromIntegral y, 0)

llGetRegionFlags info@(ScriptInfo _ _ _ pk _) [] =
    regionFlags <$> (getPrimRegion pk >>= getRegion) >>= continueI
    
llGetSimulatorHostname (ScriptInfo _ _ _ pk _) [] =
    do (i,j) <- getPrimRegion pk
       continueS (show i ++ "x" ++ show j ++ ".example.com")

llGetRegionName (ScriptInfo _ _ _ pk _) [] = 
    continueS =<< (liftM regionName $
        mlookup <<= getPrimRegion pk <<= lift getWorldRegions)

llGetAgentSize info@(ScriptInfo _ _ _ pk _) [KVal ak] =
    withAvatar ak notFound =<< found <$> getPrimRegion pk
    where notFound = slogE info ("llGetAgentSize: no such agent - " ++ ak) >> continueVec (0,0,0)
          found region av =  
              if avatarRegion av == region 
                   then continueVec (0.45,0.6,avatarHeight av)
                   else slogE info "llGetAgentSize: agent not in sim" >> continueVec (0,0,0)
                          
llGetAgentInfo info@(ScriptInfo _ _ _ pk _) [KVal ak] =
    withAvatar ak notFound =<< found <$> getPrimRegion pk
    where found region av = 
              if avatarRegion av == region 
                  then continueI $ avatarState av
                  else slogE info "llGetAgentInfo: agent not in sim" >> continueI 0
          notFound = slogE info ("llGetAgentInfo: no such agent - " ++ ak) >> continueI 0

llGetAgentLanguage info@(ScriptInfo _ _ _ pk _) [KVal ak] = do
    withAvatar ak notFound =<< found <$> getPrimRegion pk
    where found region av = continueS $
              if avatarRegion av == region then "hu" else ""
          notFound = slogE info ("llGetAgentLanguage: no such agent - " ++ ak)
              >> continueI 0
              
-- llDetected* functions ------------------------------------------------------
-- TODO: only setup for 1 detected thing at a time...
-- This is a problem especially for sensor testing

klook i = mlookup ("key_" ++ show i) . eventInfo
klookm i = maybe (throwError "nothing detected") (klook i)
ilook i = mlookup ("integer_" ++ show i) . eventInfo
ilookm i = maybe (throwError "nothing detected") (ilook i)

look s i = mlookup (s ++ "_" ++ show i) . eventInfo
lookm s i = maybe (throwError "nothing detected") (look s i)

llDetectedKey (ScriptInfo _ _ _ pk mevent) [IVal i] =
    continueWith =<< (klookm i mevent <||> return (KVal nullKey))

llDetectedLinkNumber (ScriptInfo _ _ _ pk mevent) [IVal i] =
    continueWith =<< (ilookm i mevent <||> return (IVal 0))
    
llDetectedGrab (ScriptInfo _ _ _ _ mevent) [IVal i] =
    continueWith $ fromMaybe (VVal 0 0 0) (mevent >>= M.lookup ("vector_" ++ show i) . eventInfo)

llDetectedPos (ScriptInfo _ _ _ _ mevent) [IVal i] =
    do  KVal key <- klookm i mevent
        withAvatar key (getRootPrim key >>= getObjectPosition >>= continueVec)
                (continueVec . avatarPosition) 
llDetectedRot (ScriptInfo _ _ _ _ mevent) [IVal i] =
    do  KVal key <- klookm i mevent
        withAvatar key (getRootPrim key >>= getObjectRotation >>= continueRot)
            (continueRot . avatarRotation) 
llDetectedName (ScriptInfo _ _ _ _ mevent) [IVal i] =
    do  KVal key <- klookm i mevent
        continueS =<< withAvatar key (getPrimName key) (return . avatarName)
   
llDetectedOwner (ScriptInfo _ _ _ _ mevent) [IVal i] =
    do  KVal key <- klookm i mevent
        continueK =<< withAvatar key (getPrimOwner key) (return . const key)

llDetectedGroup info@(ScriptInfo _ _ _ _ mevent) [IVal i] =
    do  KVal key <- klookm i mevent
        avatars <- getWorldAvatarsE
        prims <- getPrimsE
        continueK =<< ((fromMaybe nullKey . avatarActiveGroup) <$> (mlookup key avatars) <||>
             (fromMaybe nullKey . primGroup) <$> (mlookup key prims) <||> 
             throwError "error: key not an avatar or an object!")
    
llDetectedVel _ [IVal i] = continueVec (0,0,0) -- TODO: physics!!!

llDetectedType (ScriptInfo _ _ _ _ mevent) [IVal i] =
    do  KVal key <- klookm i mevent
        continueI =<< withAvatar key (determineActivePassiveScripted key)
            (return . const cAgent) 
    
llDetectedTouchFace (ScriptInfo _ _ _ _ mevent) [IVal i] =
   continueWith =<< lookm "face" i mevent
       
llDetectedTouchST (ScriptInfo _ _ _ _ mevent) [IVal i] =
    continueWith =<< lookm "st" i mevent

-- hack! TODO (maybe)! compute a real UV
-- I think the following is true iff the texture rotation is
-- zero, the texture repeats are (1.0,1.0) and the offsets
-- are (0.0,0.0).  I'm sure the calculation is something 
-- like (cos(rot) * s * rx * ox , sin(rot) * t * ry * oy)
-- but would need to test...
llDetectedTouchUV = llDetectedTouchST

llDetectedTouchPos (ScriptInfo oid _ _ _ mevent) [IVal i] =
    do IVal link <- ilookm i mevent 
       IVal face <- lookm "face" i mevent <||> return (IVal (-1))
       -- hack: need to, based on the face and the position, orientation
       -- and various prim parameters, calculate the touch position.
       -- but that's hard.  So we'll just return the position of the link.
       continueVec =<< if face < 0 then return (0,0,0) 
                                   else getGlobalPos =<< getLinkKey oid link

-- TODO: make this correct!
llDetectedTouchNormal (ScriptInfo _ _ _ _ mevent) [IVal i] =
    do lookm "face" i mevent -- make sure the index is valid
       continueVec (0,0,1) -- return an arbitrary vector
           
-- TODO: make this correct!
llDetectedTouchBinormal (ScriptInfo _ _ _ _ mevent) [IVal i] =
    do lookm "face" i mevent -- make sure the index is valid
       continueVec (0,1,0) -- return an arbitrary vector
           
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
    withAvatar ak notFound =<< found <$> getPrimRegion pk
    where found rp av | avatarRegion av /= rp = 
                          slogE info "llGetAnimationList: avatar not in sim" >> continueL []
                      | otherwise = do
                          now <- getTickE
                          continueL [ KVal anim | (t,anim) <- avatarActiveAnimations av, maybe True (<now) t]
          notFound = slogE info ("llGetAnimationList: no such avatar: " ++ ak) >> continueL []
    
llGetAnimation info@(ScriptInfo _ _ _ pk _) [KVal ak] =
    withAvatar ak notFound =<< found <$> getPrimRegion pk
    where found rp av | avatarRegion av /= rp = 
            slogE info "llGetAnimationList: avatar no in sim" >> continueS ""
                      | otherwise = continueS "Standing" -- TODO: real animation state
          notFound = slogE info ("llGetAnimation: no such avatar: " ++ ak) >> continueS ""
    
llStartAnimation info@(ScriptInfo _ _ sid pk _) [SVal anim] =
    do   avKey <- getPermittedAvKey info "llStartAnimation" cPermissionTriggerAnimation
         case avKey of
             Nothing -> return ()
             Just ak -> do
                av <- getWorldAvatar ak
                let anims = avatarActiveAnimations av
                now <- getTickE
                result <- findAnimation pk anim
                case result of
                    Just (key,mduration) -> setWorldAvatarE  ak av { avatarActiveAnimations = anims' }
                        where anims' = updateAnims anims now mduration key
                    Nothing -> slogE info ("llStartAnimation: animation not found - " ++ anim)
                where updateAnims anims now mduration key =
                          (expire,key):[(t,k) | (t,k) <- anims, maybe True (<now) t && key /= k]
                              where expire = fmap ((now+) . durationToTicks) mduration
         continueV

findAnimation pk anim = 
    do primAnims <- getPrimAnimations pk
       case find (\ item -> inventoryItemName item == anim) primAnims of
           Just item -> 
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
                av <- getWorldAvatar ak
                let anims = avatarActiveAnimations av
                result <- findAnimation pk anim
                case result of
                    Just (key,_) ->
                       case find (\ (_,k) -> k == key) anims of
                           Nothing -> slogE info "llStopAnimation: animation not active"
                           Just _ -> setWorldAvatarE ak av { avatarActiveAnimations = anims' }
                               where anims' = [(expire,k) | (expire,k) <- anims, k /= key]
                    Nothing -> slogE info ("llStopAnimation: animation not found - " ++ anim)
        continueV

llSetClickAction info [IVal action] =
   (if action `elem` cClickActions then slogE info "llSetClickAction: setting click action"
                                   else slogE info "llSetClickAction: invalid click action") >> continueV
-------------------------------------------------------------------------------    
-- XML RPC related functions
llCloseRemoteDataChannel info@(ScriptInfo _ _ sn pk _) [KVal key] =
   do  -- according to LSL wiki this function has no effect,
       -- so we'll just log a message indicating whether or not the passed in key is valid
       lookupScriptFromChan key <||> throwError "key not an open channel"
       continueV
   
llOpenRemoteDataChannel info@(ScriptInfo _ _ sn pk _) [] =
    do key <- lookupDataChannel (pk,sn) <||> newKeyE
       insertScriptChannelPair (pk,sn) key
       slogE info "pushing remote data channel to script"
       pushDeferredScriptEventE (Event "remote_data" [llcRemoteDataChannel, KVal key, KVal nullKey, SVal "", IVal 0, SVal ""] M.empty) pk sn 0
       continueV
       
llSendRemoteData info [KVal channel, SVal dest, IVal idata, SVal sdata] =
    do  -- according to LSL Wiki, this function doesn't do anything... but we'll check
        -- for a valid channel and log a message.
        lookupScriptFromChan channel <||> throwError "key not an open channel"
        newKeyE >>= continueK
    
llRemoteDataReply info@(ScriptInfo _ _ _ _ mevent) [KVal channel, KVal messageId, SVal sdata, IVal idata] =
    do  Just event <- return mevent
        unless (eventName event == "remote_data") $ throwError "no effect unless called within remote_data handler"
        let m = eventInfo event
        (KVal k) <- mlookup "requestKey" m <||> throwError "problem: invalid or mistyped request key in event"
        putWorldEventE 0 (XMLReplyEvent k channel messageId sdata idata)
        slogE info $ 
            concat ["llRemoteDataReply: (",show channel,",",show messageId,",",show sdata,",",show idata,")"]
        continueV
    
llRemoteDataSetRegion info [] = slogE info "llRemoteDataSetRegion: this function has no effect" >> continueV
-------------------------------------------------------------------------------

llDialog info@(ScriptInfo _ _ _ pk _) [KVal ak, SVal message, LVal buttons, IVal channel] =
    do  when (length message > 512) $ doErr "message too long, must be less than 512 characters"
        when (null message) $ doErr "must supply a message"
        unless (all (==LLString) $ map typeOfLSLValue buttons) $ doErr "button list must contain only strings"
        let buttons' = take 12 $ map ( \ (SVal s) -> s) buttons
        when (any ((24 <) . length) buttons') $ doErr "Button Labels cannot have more that 24 characters"
        when (any null buttons') $ doErr "all buttons must have label strings"
        (getWorldAvatarsE >>= mlookup ak) <||> throwError ("no such agent/avatar - " ++ ak)
        putWorldEventE 0 (DialogEvent ak message buttons' channel pk) -- deprecated!
        putWorldEventE 0 (AvatarInputEvent ak (AvatarDialog message buttons' channel pk))
        continueV
    where doErr msg = sayErr pk ("llDialog: " ++ msg) >> throwError msg
-------------------------------------------------------------------------------    
-- old (deprecated) functions for visual effects

llMakeExplosion info _ = slogE info "llMakeExplosion: deprecated" >> continueV
llMakeSmoke info _ = slogE info "llMakeSmoke: deprecated" >> continueV
llMakeFire info _ = slogE info "llMakeFire: deprecated" >> continueV
llMakeFountain info _ = slogE info "llMakeFountain: deprecated" >> continueV
    
--------------------------------------------------------------------------------
llTakeControls info@(ScriptInfo {scriptInfoPrimKey = pk, scriptInfoScriptName = sn}) [IVal controls, IVal accept, IVal pass] =
    do  script <- getScriptE pk sn
        case scriptLastPerm script of
            Nothing -> slogE info "no persmission to take controls from any agent"
            Just ak -> do
                    av <- getWorldAvatar ak
                    maybe (throwError $ "no permission to take controls from " ++ avatarName av)
                          (\ perms -> if perms .&. cPermissionTakeControls /= 0 
                                          then when (accept /= 0) $ setWorldAvatarE ak (av { avatarControlListener = Just acl })
                                          else throwError $ "no permission to take controls from " ++ avatarName av)
                          (M.lookup ak $ scriptPermissions script)
                 where acl = AvatarControlListener { avatarControlListenerMask = controls, avatarControlListenerScript = (pk,sn) }
        continueV

llReleaseControls (ScriptInfo _ _ sn pk _) [] =
    do script <- getScriptE pk sn
       forM_ (scriptControls script) $ \ ak -> do
           av <- getWorldAvatar ak
           whenJust (avatarControlListener av) $ \ acl ->
               when (avatarControlListenerScript acl == (pk,sn)) $
                   setWorldAvatarE ak (av { avatarControlListener = Nothing })
       setScriptE pk sn script 
           { scriptPermissions = M.map resetPerm (scriptPermissions script) }
       continueV
    where resetPerm i = complement cPermissionTakeControls .&. i
--------------------------------------------------------------------------------

llVolumeDetect info@(ScriptInfo { scriptInfoObjectKey = oid }) [IVal i] =
    do  d <- getObjectDynamics oid
        setObjectDynamics oid d { objectVolumeDetect = i /= 0 }
        continueV
    
llCollisionFilter info@(ScriptInfo { scriptInfoPrimKey = pk, scriptInfoScriptName = sn }) [SVal name, KVal k, IVal i] =
    do  script <- getScriptE pk sn
        setScriptE pk sn script { scriptCollisionFilter = (name,k, i /= 0) }
        continueV

llTarget info@(ScriptInfo { scriptInfoPrimKey = pk, scriptInfoScriptName = sn}) [VVal x y z, FVal range] =
    do  script <- getScriptE pk sn
        let index = scriptTargetIndex script
        setScriptE pk sn script {
            scriptTargetIndex = index + 1,
            scriptPositionTargets = IM.insert index ((x,y,z),range) (scriptPositionTargets script) }
        continueI index

llTargetRemove info@(ScriptInfo { scriptInfoPrimKey = pk, scriptInfoScriptName = sn}) [IVal tnumber] =
    do  script <- getScriptE pk sn
        setScriptE pk sn script { 
            scriptPositionTargets = IM.delete tnumber (scriptPositionTargets script) }
        continueV

llRotTarget info@(ScriptInfo { scriptInfoPrimKey = pk, scriptInfoScriptName = sn}) [RVal x y z s, FVal err] = 
    do  script <- getScriptE pk sn
        let index = scriptTargetIndex script
        setScriptE pk sn script { 
            scriptTargetIndex = index + 1,
            scriptRotationTargets = 
                IM.insert index ((x,y,z,s),err) (scriptRotationTargets script) }
        continueV
    
llRotTargetRemove info@(ScriptInfo { scriptInfoPrimKey = pk, scriptInfoScriptName = sn}) [IVal tnumber] =
    do  script <- getScriptE pk sn
        setScriptE pk sn script { scriptRotationTargets = IM.delete tnumber (scriptRotationTargets script) }
        continueV

--------------------------------------------------------------------------------
llGround info [VVal _ _ _] = continueF 0 -- the world is flat!
llGroundContour info [VVal _ _ _] = continueVec (1,0,0) -- could be any vector
llGroundNormal info [VVal _ _ _] = continueVec (0,0,1) -- straight up!
llGroundSlope info [VVal _ _ _] = continueVec (0,1,0)
--------------------------------------------------------------------------------
llGetSunDirection info [] = do
    az <- ((*(pi/7200)) . ticksToDuration) <$> getTickE
    let el = 80 * pi 
    continueVec (sin az * cos el, sin el, cos az * cos el)
    
--------------------------------------------------------------------------------
llLoadURL info [KVal ak, SVal msg, SVal url] = do
    getWorldAvatar ak
    putWorldEventE 0 (AvatarInputEvent ak (AvatarLoadURL msg url))
    continueV
    
llMapDestination info [SVal simName, (VVal x y z), (VVal _ _ _)] = do
        case mevent of
           Nothing -> do
               Attachment { attachmentKey = ak } <- required (getPrimAttachment oid) <??>
                   "prim neither attached nor touched, so call to llMapDestination is not valid"
               putIt ak
           Just (Event "touch" _ m) ->
               case M.lookup "key_0" m of
                   Just (SVal ak) -> putIt ak
                   _ -> throwError "invalid touch event!"
        continueV
    where oid = scriptInfoObjectKey info
          mevent = scriptInfoCurrentEvent info
          putIt ak = putWorldEventE 0 (AvatarInputEvent ak (AvatarMapDestination simName (x,y,z)))
--------------------------------------------------------------------------------
continueWith val = return (EvalIncomplete,val)
continueVec = continueWith . vec2VVal
continueF = continueWith . FVal
continueI = continueWith . IVal
continueRot = continueWith . rot2RVal
continueS = continueWith . SVal
continueK = continueWith . KVal
continueL = continueWith . LVal
continueV = continueWith VoidVal

yieldWith t v = return (YieldTil t,v)
yieldV t = yieldWith t VoidVal

doneWith v = return (EvalComplete Nothing,v)
doneV = doneWith VoidVal

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
        ("llDetectedTouchBinormal",llDetectedTouchBinormal),
        ("llDetectedTouchFace",llDetectedTouchFace),
        ("llDetectedTouchNormal",llDetectedTouchNormal),
        ("llDetectedTouchPos",llDetectedTouchPos),
        ("llDetectedTouchST",llDetectedTouchST),
        ("llDetectedTouchUV",llDetectedTouchUV),
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
        ("llGetAgentLanguage",llGetAgentLanguage),
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
        ("llReleaseControls", llReleaseControls),
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
    ] ++ map (\ (n,f) -> (n, defaultPredef n (\ i -> lift . f i))) internalLLFuncs
    
allFuncs = map (\ (name,_,_) -> name) funcSigs
implementedFuncs = map fst $ M.toList (defaultPredefs::(M.Map String (PredefFunc Maybe)))
unimplementedFuncs = S.toList (S.difference (S.fromList allFuncs) (S.fromList implementedFuncs))
