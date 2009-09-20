{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fwarn-unused-binds -fwarn-unused-imports #-}
module Language.Lsl.Sim(
    SimStatus(..), 
    SimStateInfo(..),
    SimCommand(..),
    SimInputEventDefinition(..),
    SimParam(..),
    SimParamType(..),
    -- imported types re=exported
    LogMessage(..),
    LogLevel(..),
    Breakpoint,
    BreakpointManager,
    EvalResult,
    ScriptInfo(..),
    AvatarOutputEvent(..),
    AvatarInputEvent(..),
    SimEvent(..),
    SimEventArg(..),
    WorldM,
    World,
    --
    eventDescriptors,
    simStep,
    unimplementedFuncs
    ) where

import Control.Arrow((&&&))
import Control.Monad((>=>),filterM,foldM,forM_,liftM,liftM2,unless,when)
import Control.Monad.State(StateT(..),lift)
import Control.Monad.Identity(Identity(..))
import Control.Monad.Error(MonadError(..),ErrorT(..))
import Data.List(elemIndex,find,foldl',nub)
import Data.Bits((.&.),(.|.),complement,xor,testBit)
import Data.Int()
import Data.Map(Map)
import Data.Maybe(fromMaybe,isJust,isNothing)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM

import Language.Lsl.Internal.AvEvents(AvatarOutputEvent(..),
    AvatarInputEvent(..))
import Language.Lsl.Internal.Breakpoint(Breakpoint,BreakpointManager,
    emptyBreakpointManager,setStepOverBreakpoint,setStepOutBreakpoint,
    replaceBreakpoints,setStepBreakpoint,breakpointFile,breakpointLine,
    checkBreakpoint)
import Language.Lsl.Internal.Constants
import Language.Lsl.Internal.Evaluation(ScriptInfo(..),Event(..),
    EvalResult(..))
import Language.Lsl.Internal.EventSigs(EventAdditionalData(..),
    EventDelivery(..),lslEventDescriptors)
import Language.Lsl.Internal.Exec(ExecutionState(..),
    ExecutionInfo(ExecutionInfo),ScriptImage(..),executeLsl,frameInfo,
    hasActiveHandler,initLSLScript)
import Language.Lsl.Internal.ExpressionHandler(evaluateExpression)
import Language.Lsl.Internal.Key(nullKey)
import Language.Lsl.Internal.Log(LogLevel(..),LogMessage(..))
import Language.Lsl.Internal.Physics(checkIntersections,dampForce,dampTorque,
    dampZForce,gravC,kin,rotDyn,totalTorque)
import Language.Lsl.Internal.Type(LSLValue(..),LSLType(..),isIVal,isLVal,
    isSVal,lslShowVal,lslTypeString,rot2RVal,vec2VVal)
import Language.Lsl.UnitTestEnv(simFunc,hasFunc)
import Language.Lsl.Internal.SimLL
import Language.Lsl.Internal.Util(add3d,angleBetween,diff3d,dist3d2,mag3d,
    mlookup,neg3d,norm3d,quaternionToMatrix,rot3d,scale3d,lift2,lift3,(<||>))
import Language.Lsl.WorldDef(Attachment(..),Avatar(..),
    AvatarControlListener(..),InventoryInfo(..),InventoryItem(..),
    InventoryItemData(..),InventoryItemIdentification(..),LSLObject(..),
    ObjectDynamics(..),Prim(..),PositionTarget(..),RotationTarget(..),
    Script(..),WebHandling(..),defaultDynamics,emptyPrim,inventoryItemName,
    mkScript,isInvObjectItem,isInvScriptItem,primPhysicsBit,
    scriptInventoryItem,worldFromFullWorldDef)
import Language.Lsl.Internal.WorldState

import System.Random(mkStdGen)

-- this is extremely approximate....
objRadius oid = do
        LSLObject { primKeys = keys } <- getObjectE oid
        foldM f 0 keys
    where f r k = do
              (x,y,z) <- getPrimPosition k
              (sx,sy,sz) <- getPrimScale k
              let (rx,ry,rz) = (sx / 2 + abs x, sy / 2 + abs y, sz / 2 + abs z)
              return $ maximum [r,rx,ry,rz]
---------------------------------------------------------------------------------------------------
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
           Nothing -> logAMessage LogWarn "sim" ("no such script: " ++ show (key, sid) ++ ", event: " ++ show e)
           Just script -> setWorldScripts (M.insert (key,sid) (script { scriptEventQueue = scriptEventQueue script ++ [e] } ) scripts)
pushEventE = lift3 pushEvent

pushEventToPrim e key =
    runErrPrim key () (getPrimScripts key >>= mapM_ (pushEventE e key . inventoryItemName))
pushEventToPrimE = lift2 pushEventToPrim
           
pushEventToObject e key =
    do objects <- getObjects
       case M.lookup key objects of
           Nothing -> logAMessage LogWarn "sim" ("no such object: " ++ key)
           Just o ->  mapM_ (pushEventToPrim e) (primKeys o)
pushEventToObjectE = lift2 pushEventToObject

newWorld slice maxt iq lib scripts avatars objs prims activeScripts regions
        keyIndex webHandling eventHandler log = 
    World {
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
        worldTouchCheckTime = 0 }
           
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
              Just (Right code) -> do
                  scriptKey <- newKey
                  updatePrim (\ p -> return $ addScript p (scriptInventoryItem name scriptKey script)) key
                  let sstate = initLSLScript code
                  scripts <- getWorldScripts
                  setWorldScripts (M.insert (key,name) ((mkScript sstate) { scriptStartTick = t, scriptLastResetTick =  t }) scripts)
       where addScript prim invScript = prim { primInventory = invScript:primInventory prim }
processEvent chat@(Chat chan name key msg location range) =
    do listeners <- getListeners 
       locatedListeners <- mapM locateListener (IM.elems listeners)
       let listeners'= [ l | (l,_,_) <- filter (matchListener chat) locatedListeners]
       -- event goes to all UNIQUE addresses in list
       let addresses = nub $ map listenAddress listeners'
       mapM_ (uncurry (pushEvent (Event "listen" [IVal chan, SVal name, KVal key, SVal msg] M.empty))) addresses
       when (chan == 0) $ case range of
           Nothing -> return ()
           Just dist -> pushChatToAvatars location dist
    where locateListener (listener,active) = do
              (VVal x y z) <- getPos (listenerPrimKey listener)
              region <- getPrimRegion (listenerPrimKey listener)
              return (listener,active,(region,(x,y,z)))
          pushChatToAvatars location range =
              liftM M.elems getWorldAvatars >>= mapM_ (pushChatToAvatar location range)
          pushChatToAvatar (region,pos) range avatar =
              when (region == avatarRegion avatar && dist3d2 pos (avatarPosition avatar) <= range^2) $
                  putWorldEvent 0 $ AvatarInputEvent (avatarKey avatar) (AvatarHearsChat name key msg)
processEvent (WorldSimEvent name args) = 
    case M.lookup name eventDescriptors of
        Nothing -> logAMessage LogWarn "sim" ("event " ++ name ++ " not understood")
        Just def -> handleSimInputEvent def args
processEvent (PermissionRequestEvent pk sname ak mask) = 
    runAndLogIfErr ("can't find prim/script: " ++ pk ++ "/" ++ sname) () $ do
        script <- getScriptE pk sname
        let script' = script { scriptPermissions = M.insert ak mask (scriptPermissions script), scriptLastPerm = Just ak }
        setScriptE pk sname script'
        pushEventE (Event "run_time_permissions" [IVal mask] M.empty) pk sname
processEvent evt@(TimerEvent interval (primKey,scriptName)) =
    do pushEvent (Event "timer" [] M.empty) primKey scriptName
       putWorldEvent interval evt
processEvent evt@(SensorEvent (pk,sn) name key stype range arc rpt) =
    runAndLogIfErr "problem processing sensor event" () $ do 
       t <- getTickE
       maybe (return ()) (\ interval -> putWorldEventE interval evt) rpt
       sensedObjects <- senseObjects
       sensedAvatars <- senseAvatars
       let list = take 16 (sensedObjects ++ sensedAvatars)
       if null list
           then pushEventE (Event "no_sensor" [] M.empty) pk sn
           else pushEventE (Event "sensor" [(IVal $ length list)] (M.fromList (zipWith (\ i k -> ("key_" ++ show i,KVal k)) [0..] list))) pk sn       
    where senseObjects = do
              pos <- getGlobalPos pk
              rot <- getGlobalRot pk
              root <- getRootPrim pk
              let fwd = fwdFromRot rot
              prims <- rootPrims root
              poss <- mapM (getGlobalPos . primKey) prims
              let pprims = zip poss prims
              statuses <- mapM (objectStatus . primKey) prims
              return [ primKey p | ((pos,p),stat) <- zip pprims statuses, withinSensorArea pos fwd range arc pos, stat .&. stype /= 0]
          senseAvatars = 
              if stype .&. cAgent /= 0 then do
                      pos <- getGlobalPos pk
                      rot <- getGlobalRot pk
                      attachKey <- liftM (fmap attachmentKey) (getPrimAttachment pk)
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
              in (m == 0 ) || (m <= range && angleBetweenV v' direction <= arc)
          rootPrims exclude = do
              objs <- getObjectsE
              mapM getPrimE [ k | LSLObject { primKeys = (k:_) } <- M.elems objs, exclude /= k]
          objectStatus root = do
              LSLObject { primKeys = pks } <- getObjectE root
              stat <- foldM (\ x -> liftM (x .|.) . determineActivePassiveScripted) 0 pks
              if stat .&. cActive /= 0 && stat .&. cPassive /= 0
                 then return (stat .&. complement cPassive)
                 else return stat
          allAvs exclude = do
              avs <- getWorldAvatarsE
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
                        setWorldEventHandlerE (Just (moduleName, results))
                        throwError msg
                                             | otherwise ->
                        let IVal status = fromMaybe (IVal 200) (lookup "outHttpStatus" results >>= ( \ val -> if isIVal val then Just val else Nothing))
                            LVal metadata = fromMaybe (LVal []) (lookup "outHttpMetadata" results >>= ( \ val -> if isLVal val then Just val else Nothing))
                            body = fromMaybe (SVal "") (lookup "outHttpBody" results >>= ( \ val -> if isSVal val then Just val else Nothing))
                            events = fromMaybe (LVal []) (lookup "outEvents" results)
                        in do putHTTPResponseEventE pk sn key status metadata body 2
                              processEventsListE events
                              setWorldEventHandlerE (Just (moduleName, results))
                    Right _ -> throwError "invalid web handling function, must return a string"
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
                (liftM (event:) getWorldOutputQueue >>= setWorldOutputQueue)
                (liftM (key:) getWorldPendingHTTPRequests >>= setWorldPendingHTTPRequests)
                -- put a timeout event on the queue.  'real' time and sim time are much different, so the 
                -- 'real' world timeout is much different
                putHTTPTimeoutEvent pk sn key timeout
processEvent (XMLRequestEvent source channel idata sdata) = 
    runAndLogIfErr "invalid xml request" () $ do
        logAMessageE LogInfo "sim" "processing XMLRequestEvent"
        (pk,sn) <- lookupScriptFromChan channel <||> throwError ("no such channel" ++ channel)
        key <- newKeyE
        liftM (M.insert key source) (lift getWorldXMLRequestRegistry) >>= lift . setWorldXMLRequestRegistry
        pushEventE (Event "remote_data" [llcRemoteDataRequest, KVal channel, KVal nullKey, SVal "", IVal idata, SVal sdata]
                                              (M.fromList [("requestKey",KVal key)])) pk sn
processEvent (XMLReplyEvent key channel messageId sdata idata) =
    runAndLogIfErr "invalid xml reply" () $ do
        source <- (lift getWorldXMLRequestRegistry >>= mlookup key) <||> throwError "missing source for XML request"
        lift (liftM (M.delete key) getWorldXMLRequestRegistry >>= setWorldXMLRequestRegistry)
        case source of
            XMLRequestInternal tag -> do
                (moduleName, state) <- lift getWorldEventHandler >>= maybe (throwError "no handler defined") return
                lib <- lift getWLibrary
                case simFunc lib (moduleName, "xmlReply") state [SVal tag, SVal channel, SVal sdata, IVal idata] of
                    Left s -> throwError s
                    Right (SVal msg, results) | not (null msg) -> do
                        setWorldEventHandlerE (Just (moduleName, results))
                        throwError msg
                                              | otherwise ->
                        let events = fromMaybe (LVal []) (lookup "outEvents" results) in do
                            processEventsListE events
                            setWorldEventHandlerE (Just (moduleName, results))
            XMLRequestExternal tag -> do
                let event = SimEvent "xml_reply" [
                        SimEventArg "tag" tag,
                        SimEventArg "channel" channel,
                        SimEventArg "sdata" sdata,
                        SimEventArg "idata" (show idata)] 0
                lift (liftM (event:) getWorldOutputQueue >>= setWorldOutputQueue)
processEvent (DialogEvent agent message buttons channel source) =
    runAndLogIfErr "invalid dialog event" () $
        lift getWorldEventHandler >>= (\ mhandler -> case mhandler of
            Nothing -> return ()
            Just (moduleName,state) -> do
                lib <- lift getWLibrary
                avName <- liftM avatarName (getWorldAvatarsE >>= mlookup agent) <||> throwError "problem finding avatar"
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
                                setWorldEventHandlerE (Just (moduleName, results))
                                throwError msg
                                                      | otherwise ->
                                let (IVal selection) = fromMaybe (IVal (-1)) (lookup "outDialogButtonSelected" results)
                                    events = fromMaybe (LVal []) (lookup "outEvents" results)
                                in do
                                    warn
                                    when (selection >= 0 && selection < length buttons) $ 
                                        putChatE sayRange agent channel (buttons !! selection)
                                    processEventsListE events
                                    setWorldEventHandlerE (Just (moduleName, results)))
                            where warn = logAMessageE LogWarn "sim" 
                                    "the 'dialog' entry point for the world event handler is deprecated, use the avatar event handler instead"

processEvent (RezObjectEvent links pos vel rot start pk copy atRoot) =
    runAndLogIfErr "invalid rez object event" () $ do
        when (null links) $ throwError "empty link set!"
        -- reset positions relative to root
        let geomCenter = scale3d ( 1 / fromIntegral (length links)) 
                                 (foldl' add3d (0,0,0) (map (flip diff3d (0,0,0) . primPosition) links))
        let pos' = if atRoot then pos else pos `add3d` neg3d geomCenter
        let update = if copy then updateKeys else return
        -- TODO: rotation and velocity
        links' <- mapM update links
        mapM_ activateScripts links'
        let rootKey = primKey (head links')
        lift (liftM (M.insert rootKey (LSLObject (map primKey links') defaultDynamics { objectPosition = pos' })) getObjects >>= setObjects)
        lift (getPrims >>= (\ m -> return (foldl' (\ m l -> M.insert (primKey l) l m) m links')) >>= setPrims)
        pushEventToObjectE (Event "on_rez" [IVal start] M.empty) rootKey
        pushEventToPrimE (Event "object_rez" [KVal rootKey] M.empty) pk
    where activateScripts prim = do
              let scripts = [ item | item <- primInventory prim, isInvScriptItem item ]
              let activateScriptItem item = 
                      let InvScript id state = inventoryItemData item
                          name = inventoryItemName item in
                              activateScript (primKey prim) name id state start
              mapM activateScriptItem scripts
          updateKeys prim = do
              key <- newKeyE
              newInventory <- mapM updateItemKeys (primInventory prim)
              return $ prim { primKey = key, primInventory = newInventory }
          updateItemKeys item = do
              key <- newKeyE
              let name = inventoryItemName item
              newData <- if isInvObjectItem item
                  then liftM InvObject (mapM updateKeys (invObjectPrims $ inventoryItemData item))
                  else return (inventoryItemData item)
              return $ item { inventoryItemIdentification = InventoryItemIdentification (name,key), inventoryItemData = newData }
processEvent (ResetScriptEvent pk sn) =
     runAndLogIfErr "invalid reset script event" () (resetScript pk sn)
processEvent (DetachCompleteEvent oid ak) =
     runAndLogIfErr "invalid detach complete event" () $ do
         -- need to remove the object and it's prims and its active scripts from the live lists, but
         -- save the script states
         avatars <- getWorldAvatarsE
         avatar <- mlookup ak avatars
         LSLObject { primKeys = links } <- getObjectE oid
         prims <- mapM passivatePrim links
         when (null prims) $ throwError "object has no prims!"
         let root = head prims
         let object = InventoryItem (InventoryItemIdentification (primName root,primKey root)) 
                                    (InventoryInfo (primCreator root) (0x0008e000,0x0008e000,0x0008e000,0x0008e000,0x0008e000))
                                    (InvObject prims)
         let avInv = avatarInventory avatar
         setWorldAvatarsE (M.insert ak (avatar { avatarInventory = object:avInv }) avatars)
         objects <- getObjectsE
         setObjectsE (M.delete oid objects)
     where passivatePrim pk = do
               scriptItems <- getPrimScripts pk
               mapM_ (moveScriptStateToPrim pk . inventoryItemName) scriptItems
               prims <- getPrimsE
               prim <- mlookup pk prims
               setPrimsE $ M.delete pk prims
               return prim
           moveScriptStateToPrim pk sn = do
               script <- getScriptE pk sn
               let img = scriptImage script
               primInv <- getPrimInventory pk
               let (xs,ys) = break (\ item -> sn == inventoryItemName item) primInv
               when (null ys) $ throwError "script inexplicably not in inventory"
               (y:ys') <- return ys
               let scriptData = inventoryItemData y
               setPrimInventoryE pk (((y { inventoryItemData = scriptData { invScriptState = Just img } }) : xs) ++ ys)
               delScriptE pk sn
processEvent (GiveAvatarInventoryEvent ak item) = logAMessage LogInfo "sim" "giving avatar inventory: not implemented"
processEvent (AvatarOutputEvent ak avEv) = processAvatarOutputEvent ak avEv
processEvent (AvatarInputEvent ak avEv) = processAvatarInputEvent ak avEv
processEvent _ = error "not implemented"

processEventsListE (LVal []) = return ()
processEventsListE (LVal ((IVal i):l)) = do
    processEventListE (take i l)
    processEventsListE (LVal (drop i l))
processEventsListE _ = logAMessageE LogWarn "sim" "user supplied event handler has invalid type for 'outEvents' variable"
    
processEventListE [SVal "xml_request", SVal sourceId, KVal channel, IVal idata, SVal sdata, FVal delay] = 
    putWorldEventE delay (XMLRequestEvent (XMLRequestInternal sourceId) channel idata sdata)
processEventListE l = logAMessageE LogWarn "sim" ("Invalid event from event handler: " ++ lslShowVal (LVal l))

processAvatarOutputEvent k (AvatarTouch {avatarTouchPrimKey = pk, avatarTouchDuration = secs}) = do
        touches <- getWorldTouches
        now <- getTick
        setWorldTouches (M.alter (alt now) pk touches)
    where alt start Nothing = Just [newTouch start]
          alt start (Just ts) = Just (newTouch start:ts)
          newTouch start = Touch {touchAvatarKey = k, touchPrimKey = pk,
              touchFace = 0, touchST = (0.5, 0.5), touchStartTick = start, 
              touchEndTick = start + durationToTicks secs}
processAvatarOutputEvent k (AvatarWhisper {avatarChatChannel = chan, avatarChatMessage = msg}) = avChat 10 msg k chan
processAvatarOutputEvent k (AvatarSay {avatarChatChannel = chan, avatarChatMessage = msg}) = avChat 20 msg k chan
processAvatarOutputEvent k (AvatarShout {avatarChatChannel = chan, avatarChatMessage = msg}) = avChat 100 msg k chan
processAvatarOutputEvent k (AvatarPay {avatarPayPrimKey = pk, avatarPayAmount = val}) = runAndLogIfErr "problem handling avatar payment" () $
    primHasActiveHandler k "money" >>= flip when (pushEventToPrimE (Event "money" [KVal pk, IVal val] M.empty) pk)
processAvatarOutputEvent k (AvatarControl { avatarNewControlBits = newBits }) = runAndLogIfErr "problem processing control change" () $ do
    av <- getWorldAvatar k
    let prev = avatarControls av
    setWorldAvatarE k (av { avatarControls = newBits })
    flip (maybe (return ())) (avatarControlListener av) $ 
        \ (AvatarControlListener {avatarControlListenerMask = mask, avatarControlListenerScript = (pk,sn)}) ->
            pushEventE (Event "control" [KVal k, IVal newBits, IVal (newBits `xor` prev)] M.empty) pk sn
processAvatarOutputEvent k (AvatarFaceTouch pk secs face st) = do
    touches <- getWorldTouches
    now <- getTick
    setWorldTouches (M.alter (alt now) pk touches)
    where alt start Nothing = Just [newTouch start]
          alt start (Just ts) = Just (newTouch start:ts)
          newTouch start = Touch {touchAvatarKey = k, touchPrimKey = pk,
              touchFace = face, touchST = st, touchStartTick = start, 
              touchEndTick = start + durationToTicks secs}

avChat range msg key chan = runAndLogIfErr "problem processing chat" () $ do
    logAMessageE LogInfo ("av:" ++ key) ("chat! chan: " ++ show chan ++ ", range: " ++ show range ++ ", message: " ++ show msg)
    av <- getWorldAvatar key
    putWorldEventE 0 $ Chat chan (avatarName av) key msg (avatarRegion av,avatarPosition av) (Just range)

processAvatarInputEvent k inputEvent = runAndLogIfErr "problem processing event for avatar" () $ do
        av <- getWorldAvatar k
        let avInfo = [ IVal lslPlusAvatarKey, KVal (avatarKey av), 
                       IVal lslPlusAvatarPos, ((vec2VVal . avatarPosition) av),
                       IVal lslPlusAvatarRot, ((rot2RVal . avatarRotation) av),
                       IVal lslPlusAvatarName, SVal (avatarName av) ]
        flip (maybe (return ())) (avatarEventHandler av) (\ (moduleName,state) -> do
            lib <- lift getWLibrary
            case callAvatarEventProcessor k moduleName inputEvent lib state avInfo of
                Nothing -> return ()
                Just (Left s) -> throwError s
                Just (Right (LVal events,result)) -> do
                    mapM_ (putAvatarOutputEvent k) events
                    setWorldAvatarE k (av { avatarEventHandler = Just (moduleName,result) }))

avEventProcCallInfo (AvatarOwnerSay key msg) avInfo = ("onOwnerSay",[SVal key, SVal msg, LVal avInfo])
avEventProcCallInfo (AvatarHearsChat name key msg) avInfo = ("onChat",[SVal name, SVal key, SVal msg, LVal avInfo])
avEventProcCallInfo (AvatarDialog msg buttons chan source) avInfo = ("onDialog",[SVal msg, LVal $ map SVal buttons, IVal chan, SVal source, LVal avInfo])
avEventProcCallInfo (AvatarLoadURL msg url) avInfo = ("onLoadURL",[SVal msg, SVal url, LVal avInfo])
avEventProcCallInfo (AvatarMapDestination simName position) avInfo = ("onMapDestination",[SVal simName, vec2VVal position, LVal avInfo])
callAvatarEventProcessor k moduleName avEvent lib state avInfo = 
    case hasFunc lib (moduleName,funcName) of
        Left s -> Just (Left s)
        Right True -> Just $ simFunc lib (moduleName, funcName) state args
        Right False -> Nothing
    where (funcName,args) = avEventProcCallInfo avEvent avInfo
    
putAvatarOutputEvent k (SVal s) =
    case reads s of
       ((ev,_):_) -> putWorldEventE 0 (AvatarOutputEvent k ev)
       [] -> logAMessageE LogWarn "sim" ("avatar event handler for " ++ k ++ " returned invalid event: " ++ show s)
putAvatarOutputEvent k v = logAMessageE LogWarn "sim" ("avatar event handler for " ++ k ++ " returned invalid event: " ++ lslShowVal v)
 
activateScript pk sn scriptId Nothing startParam =
    do wscripts <- lift getWScripts
       case lookup scriptId wscripts of
           Nothing -> logAMessageE LogWarn "sim" ("script " ++ scriptId ++ " not found")
           Just (Left s) -> logAMessageE LogWarn "sim" ("script " ++ scriptId ++ " not valid")
           Just (Right code) ->
               setScriptE pk sn (mkScript (initLSLScript code))
activateScript pk sn _ (Just image) startParam =
    setScriptE pk sn (mkScript image) { scriptStartParameter = startParam }
        
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
        t <- liftM (+1) getTick
        let ns = foldl' (calcNxt t) Nothing $ M.elems scripts
        wq <- getWQueue
        let ne = case wq of
                     [] -> Nothing
                     ((i,x):_) -> Just $ max t i
        return $ mmin ne ns
    where calcNxt t n (Script { scriptActive = False }) = n
          calcNxt t n (Script { scriptImage = img , scriptEventQueue = q }) =
              case executionState img of
                  Executing -> Just t
                  Suspended _ -> Just t
                  Waiting -> if null q then n else Just t
                  WaitingTil i -> if isNothing n then Just (max t i) else fmap (max t . min i) n 
                  SleepingTil i -> if isNothing n then Just (max t i) else fmap (max t . min i) n
                  _ -> n
          mmin Nothing Nothing = Nothing
          mmin Nothing (Just i) = Just i
          mmin (Just i) Nothing = Just i
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
                 Nothing -> logAMessage LogWarn "sim" ("script " ++ show k ++ ": prim not found!")
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
                   Nothing -> logAMessage LogInfo "sim" "script seems to have disappeared while executing (result of llDie?)"
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
        touchLists <- liftM M.toList getWorldTouches
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
           ins m (i,(Touch { touchAvatarKey = ak, touchFace = face, touchST = (s,t) },link)) = 
               M.insert ("vector_" ++ show i) (VVal 0 0 0) .
               M.insert ("key_" ++ show i) (KVal ak) .
               M.insert ("integer_" ++ show i) (IVal link) .
               M.insert ("face_" ++ show i) (IVal face) .
               M.insert ("st_" ++ show i) (VVal s t 0) $ m
               
collectTouches touchType acc [] = return acc
collectTouches touchType acc (t@(Touch { touchPrimKey = pk }):ts) = do
    lneeds <- primHasActiveHandler pk touchType
    linkNum <- getPrimLinkNum pk
    rk <- getRootPrim pk
    if pk == rk && lneeds then collectTouches touchType (M.alter (alt (t,linkNum)) pk acc) ts
              else do
                  pass <- getPrimPassTouches pk
                  rneeds <- primHasActiveHandler rk touchType
                  let r = [(rk,(t { touchPrimKey = rk }, linkNum)) | rneeds && pass]
                  let p = [(pk,(t, linkNum)) | lneeds]
                  collectTouches touchType (foldl' ( \ m (k,d) -> M.alter (alt d) k m) acc (r ++ p)) ts
    where alt i Nothing = Just [i]
          alt i (Just l)  = Just (i:l)
runPhysics :: Monad m => WorldM m ()
runPhysics = do
        t0 <- getWorldPhysicsTime
        t1 <- getTick
        os <- getObjects
        let tlist = [t0,(t0+10)..t1] ++ [t1 | (t1 - t0) `mod` 10 > 0]
        let intervals = zip tlist $ tail tlist
        forM_ intervals $ \ (t0,t1) ->
            forM_ (M.toList os) $ \ (pk,obj) -> runErrPrim pk () $ do
                prim <- getPrimE pk
                dyn <- getObjectDynamics pk
                when (flip testBit primPhysicsBit $ primStatus prim) $ do
                    mass <- objectMass pk
                    let pos0@(_,_,z0) = objectPosition dyn
                    let vel0@(_,_,vz0) = objectVelocity dyn
                    let force = (fst (objectForce dyn) `rot3d` rotation) `add3d` (0,0,mass * objectBuoyancy dyn * (-gravC))
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
                    setObjectDynamics pk $! dyn { objectVelocity = vel, 
                                                objectPosition = pos,
                                                objectRotation = rot,
                                                objectOmega = omega }
                    return ()
        handleCollisions
        handleMovers
        handleTargets
        setWorldPhysicsTime $! t1

handleMovers :: Monad m => WorldM m ()
handleMovers = runAndLogIfErr "problem determining which objects moved" () $
    do lastPositions <- lift getWorldLastPositions
       let lastKeys = S.fromList $ M.keys lastPositions
       curPositions <- liftM M.keys getObjectsE >>= mapM (\ k -> liftM ((,)k) (getObjectPosition k))
       let curKeys = S.fromList $ map fst curPositions
       let newKeys = curKeys `S.difference` lastKeys
       let keysFromBefore = curKeys `S.difference` newKeys
       m <- return . M.fromList =<< mapM ( \ (k,pos) -> if k `S.member` keysFromBefore
                                                        then do (moving,lpos) <- mlookup k lastPositions
                                                                let moving' = lpos /= pos
                                                                when (moving' && not moving) $ 
                                                                    pushEventToPrimE (Event "moving_start" [] M.empty) k
                                                                when (moving && not moving') $ 
                                                                    pushEventToPrimE (Event "moving_end" [] M.empty) k
                                                                return (k,(moving', pos))
                                                        else return (k,(False,pos))) curPositions
       lift $ setWorldLastPositions m

handleObjectCollisions prims = do
        -- look at all non-phantom prims
        primBoxes <- mapM (\ (k,p) -> liftM ((,) k . (,) p) (getPrimBox k)) prims
        --let voldtct = map fst $ filter (primVolumeDetect . snd) prims
        voldtct <- liftM (map fst) $ flip filterM prims (\ (_,prim) -> 
            liftM objectVolumeDetect (getObjectDynamics $ fromMaybe (primKey prim) (primParent prim)))
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
            objCollisions' <- liftM (map fst . filter (\ ((k,_),mv) -> (k `notElem` voldtct) || isJust mv)) 
                    (mapM (\ v@(k,_) -> (liftM ((,) v) (attachment k))) objCollisions)
            mapM_ (send "collision") objCollisions')
        toObjCollisions formerCollisions "collision_end" >>= mapM_ (send "collision_end")
        setWorldCollisionsE curCollisions
    where send :: Monad m => String -> (String,[(Int,String)]) -> ErrorT String (WorldM m) ()
          send hn (pk,oinfo) =  collisionScripts >>= mapM_ (sendScript hn pk oinfo)
              where collisionScripts =
                        liftM (map inventoryItemName) (getPrimScripts pk) >>= 
                            filterM (\ sn -> scriptHasActiveHandler pk sn hn)
          sendScript :: Monad m => String -> String -> [(Int,String)] -> String -> ErrorT String (WorldM m) ()
          sendScript hn pk oinfo sn = do
              script <- getScriptE pk sn
              case script of
                  (Script { scriptCollisionFilter = (name,key,accept) })
                          | null name && (null key || nullKey == key) && accept -> pushit oinfo
                          | null name && (null key || nullKey == key) && not accept -> return ()
                          | accept -> filterM (matchesIn name key) oinfo >>= pushit
                          | otherwise -> filterM (matchesOut name key) oinfo >>= pushit
              where matchesIn name key (_,k) = if k == key 
                                                   then return True 
                                                   else (if null key || key == nullKey 
                                                             then liftM (name==) (getPrimName k)
                                                             else return False)
                    matchesOut name key (_,k) = if k == key 
                                                    then return False 
                                                    else (if null key || key == nullKey
                                                              then liftM (not . (name==)) (getPrimName k)
                                                              else return True)
                    pushit oinfo = unless (null oinfo) $ pushEventE ev pk sn
                        where ev = Event hn [IVal $ length oinfo] 
                                      (M.fromList $
                                          zipWith (\ i (_,k) -> ("key_" ++ show i, KVal k)) [0..] oinfo ++
                                          zipWith (\ i (n,_) -> ("integer_" ++ show i, IVal n)) [0..] oinfo)
          toObjCollisions collisionsS hn = do
                  passed <- passes primColliders
                  colliders' <- mapM (\ (k,cs) -> (getPrimLinkNum k >>= \ i -> return (k,i,cs))) primColliders
                  colliders'' <- mapM pk2ok (colliders' ++ passed)
                  colliders''' <-  mapM (\ (k,i,cs) -> liftM ((,,) k i) (filterM (objectHasVolDetect >=> (return . not)) cs)) colliders''
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
                    pk2ok (pk,i,cs) = liftM ((,,) pk i) (primsToObjects cs)
                    primsToObjects ks = liftM (nub . zipWith fromMaybe ks) (mapM getPrimParent ks)
                    pass hn k = do
                        getPrimParent k >>= maybe (return False) (pass')
                        where pass' pk = liftM2 (||) (getPrimPassTouches k)
                                 (liftM not (k `primHasActiveHandler` hn))
                    fromListOfPairs [] = S.empty
                    fromListOfPairs ((x,y):ps) = (S.insert x . S.insert y) (fromListOfPairs ps)
                    collectColliders intsns pk = [ j | (Just j) <- map (other pk) intsns]
                    objectHasVolDetect ok = do
                         getPrimAttachment ok >>=
                             maybe (liftM objectVolumeDetect (getObjectDynamics ok)) 
                                 (const $ return False)
                    other k (x,y) | k == x = Just y
                                  | k == y = Just x
                                  | otherwise = Nothing

handleLandCollisions prims = do
        primBoxes <- mapM (\ (k,p) -> liftM ((,) k ) (getPrimBox k)) prims
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
                  unless (null liveScripts) $ send' pos pk
                  when (null liveScripts || pass) $ getRootPrim pk >>= send' pos
              where send' pos k = do
                        scripts <- liftM (map inventoryItemName) (getPrimScripts k) >>=
                                   filterM (\ sn -> scriptHasActiveHandler k sn hn)
                        forM_ scripts (lift . pushEvent (Event hn [vec2VVal pos] M.empty) k)
                     
handleCollisions :: (Monad m) => WorldM m ()
handleCollisions = runAndLogIfErr "can't handle collisions" () $ do
    prims <- getNonPhantomPrims
    handleObjectCollisions prims
    handleLandCollisions prims
    
getNonPhantomPrims :: (Monad m) => ErrorT String (WorldM m) [(String,Prim)]
getNonPhantomPrims = liftM (filter ((==0) . (.&. cStatusPhantom) . primStatus . snd) . M.assocs) getPrimsE
   
handleTargets :: Monad m => WorldM m ()
handleTargets = do
    t <- getTick
    t0 <- getWorldTargetCheckTime
    when (t - t0 > durationToTicks 0.1) $
        liftM M.keys getObjects >>= mapM_ (\ k ->
            runErrPrim k () $ do
                pos <- getGlobalPos k
                rot <- getGlobalRot k
                scripts <- getActualPrimScripts k
                forM_ scripts (\ ((_,sn),script) -> do
                        forM_ (IM.toList $ scriptPositionTargets script) (\ (i,(target,range)) -> 
                            let image = scriptImage script
                                inRange = (dist3d2 target pos <= range^2) in
                            if hasActiveHandler image "at_target" && inRange
                                then pushEventE (Event "at_target" [IVal i, vec2VVal target, vec2VVal pos] M.empty) k sn
                                else when (hasActiveHandler image "not_at_target" && not inRange) $
                                         pushEventE (Event "not_at_target" [] M.empty) k sn
                            )
                        forM_ (IM.toList $ scriptRotationTargets script) (\ (i,(target,range)) ->
                            let image = scriptImage script
                                inRange = (angleBetween target rot <= range) in
                            if hasActiveHandler image "at_rot_target" && inRange
                                then pushEventE (Event "at_rot_target" [IVal i, rot2RVal target, rot2RVal rot] M.empty) k sn
                                else when (hasActiveHandler image "not_at_rot_target" && not inRange) $
                                         pushEventE (Event "not_at_rot_target" [] M.empty) k sn
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
       unless (t' >= pauseTime || isSuspended suspendInfo) simulate
    where isSuspended Nothing = False
          isSuspended _ = True

-- --------------------------------------------------------------------------------------------

data SimCommand = SimContinue { simCmdBreakpoints :: [Breakpoint], simCmdEvents :: [SimEvent] }
                | SimStep { simCmdBreakpoints :: [Breakpoint], simCmdEvents :: [SimEvent] }
                | SimStepOver { simCmdBreakpoints :: [Breakpoint], simCmdEvents :: [SimEvent] }
                | SimStepOut { simCmdBreakpoints :: [Breakpoint], simCmdEvents :: [SimEvent] } deriving (Show)

data SimStatus = SimEnded { simStatusMessage :: String, simStatusLog :: [LogMessage], simStatusState :: SimStateInfo } | 
                 SimInfo { simStatusEvents :: [SimEvent], simStatusLog :: [LogMessage], simStatusState :: SimStateInfo } |
                 SimSuspended { simStatusEvents :: [SimEvent], 
                                simStatusSuspendInfo :: ExecutionInfo Float,
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
        simStateInfoPrims = map (primKey &&& primName) (M.elems $ wprims world),
        simStateInfoAvatars = map (\ (_,a) -> (avatarKey a, avatarName a)) (M.toList $ worldAvatars world),
        simStateInfoScripts = M.keys (worldScripts world)
    }
    
data SimInputEventDefinition m = SimInputEventDefinition { 
    simInputEventName :: String,
    simInputEventDescription :: String,
    simInputEventParameters :: [SimParam],
    simInputEventHandler :: String -> [LSLValue Float] -> WorldM m () }

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
        if tick world''' < maxTick world'''
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
        when (length params /= length args) $ throwError "wrong number of parameters"
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
        Nothing -> throwError ("invalid " ++ lslTypeString t ++ " expression" )
        Just v -> return v
checkEventArg (SimParam name _ _) Nothing = throwError ("event argument " ++ name ++ " not found")

handleSimInputEvent def args = 
  case checkEventArgs def args of
      Left s -> logAMessage LogWarn "sim" s
      Right argValues -> simInputEventHandler def (simInputEventName def) argValues

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
                        prim <- getPrimE pk
                        linkNum <- getPrimLinkNum pk                        
                        needstouches <- liftM or $ mapM (primHasActiveHandler pk) ["touch","touch_start","touch_end"]
                        let touchList = [pk | needstouches] ++
                                        (if primPassTouches prim || not needstouches then maybe [] return (primParent prim) else [])
                        lift $ mapM_ (doTouch linkNum) touchList
                    where doTouch linkNum k = do
                            putWorldEvent 0 (mkTouchStartEvent k "1" ak linkNum)
                            let tdur = floor (1000 * duration)
                            let tTimes = map ((/1000.0) . fromIntegral) [1,(1 + 500) .. tdur]
                            mapM_ (flip putWorldEvent (mkTouchEvent pk "1" ak linkNum)) tTimes 
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
                        Just av ->
                            putWorldEvent 0 $ Chat chan (avatarName av) ak message (avatarRegion av,avatarPosition av) sayRange
                f name _ = logAMessage LogWarn "sim" ("invalid event activation: " ++ name)
            in f
    }
        
eventDescriptors :: Monad m => Map String (SimInputEventDefinition m)
eventDescriptors = M.fromList $ mkEventDefList ([userTouchEventDef,userChatEventDef] ++ rawLslEventDescriptors)

mkEventDefList = map (\ e -> (simInputEventName e, e))

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
            flip map additionalData (\ d -> case d of
                EventAdditionalInts name description -> SimParam name description (SimParamLSLValue LLInteger)
                EventAdditionalKeys name description -> SimParam name description SimParamKey
                EventAdditionalAvatarKeys name description -> SimParam name description SimParamAvatar
                EventAdditionalVectors name description -> SimParam name description (SimParamLSLValue LLVector)) ++
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
