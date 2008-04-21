module Lsl.World1 where

import Control.Monad
import Control.Monad.State hiding (State,get)
import Control.Monad.Identity
import Control.Monad.Error
import Data.List
import Data.Bits
import Data.Map(Map)
import Data.Maybe
import qualified Data.Map as M
import Debug.Trace

import Lsl.Avatar
import Lsl.Breakpoint
import Lsl.Builder
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
                    wlisteners :: [(Int,Listener)],
                    nextListenerId :: Int,
                    wobjects :: Map String LSLObject,
                    wprims :: Map String Prim,
                    worldScripts :: Map (String,String) (Validity ScriptImage,[Event]), 
                    inventory :: [(String,LSLObject)],
                    tick :: Int,
                    msglog :: [LogMessage],
                    predefs :: [PredefFunc m],
                    randGen :: StdGen,
                    wlibrary :: [(String,Validity LModule)],
                    wscripts :: [(String,Validity CompiledLSLScript)],
                    worldDB :: ValueDB,
                    worldAvatars :: [(String,Avatar)],
                    worldBreakpointManager :: BreakpointManager,
                    worldSuspended :: Maybe (String,String) -- prim-key, script-name, image
                } deriving (Show)

-- a state monad for the World
type WorldM m = StateT (World m) m

worldM f = StateT (\ s -> return (f s))

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
               logAMessage LogDebug (pkey ++ ":" ++ sid) ("called " ++ name ++ (show args))
               return (EvalIncomplete,case rettype of
                      LLVoid -> VoidVal
                      LLInteger -> IVal 0
                      LLFloat -> FVal 0.0
                      LLString -> SVal ""
                      LLKey -> KVal nullKey
                      LLList -> LVal []
                      LLVector -> VVal 0.0 0.0 0.0
                      LLRot -> RVal 0.0 0.0 0.0 1.0)
                   
queryWorld q = worldM (\w -> (q w, w))
updateWorld u = worldM (\w -> ((), u w))

getWorldDB :: Monad m => WorldM m ValueDB
getWorldDB = queryWorld worldDB
setWorldDB db = updateWorld (\ w -> w { worldDB = db })

getSliceSize :: Monad m => WorldM m Int
getSliceSize = queryWorld sliceSize
getListeners :: Monad m => WorldM m [(Int,Listener)]
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
getWorldScripts :: Monad m => WorldM m (Map (String,String) (Validity ScriptImage, [Event]))
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
getWorldAvatars :: Monad m => WorldM m [(String,Avatar)]
getWorldAvatars = queryWorld worldAvatars
getWorldBreakpointManager :: Monad m => WorldM m BreakpointManager
getWorldBreakpointManager = queryWorld worldBreakpointManager
getWorldSuspended :: Monad m => WorldM m (Maybe (String,String))
getWorldSuspended = queryWorld worldSuspended

getPrim k = (lift getPrims >>= M.lookup k)
getPrimVal k f = (lift getPrims >>= M.lookup k >>= return . f)
getPrimName k = getPrimVal k primName
getPrimPosition k = getPrimVal k primPosition
getPrimParent k = getPrimVal k primParent
getPrimDescription k = getPrimVal k primDescription
getPrimOwner k = getPrimVal k primOwner
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

-- TODO: temp until introduce region into Prim definition
getPrimRegion :: Monad m => a -> m (Int,Int)
getPrimRegion _ = return (0,0)
primRegion :: a -> (Int,Int)
primRegion _ = (0,0)

runErrFace k i defaultVal = runAndLogIfErr 
    ("face " ++ (show i) ++ " or prim " ++ k ++ " not found") defaultVal

getPrimFace k i = getPrimFaces k >>= (lookupByIndex i)
getPrimFaceAlpha k i = getPrimFace k i >>= return . faceAlpha
getPrimFaceColor k i = getPrimFace k i >>= return . faceColor

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
           
instance Monad m => Show (ScriptInfo -> [LSLValue] -> WorldM m (EvalResult,LSLValue)) where
    showsPrec _ _ = showString "(function :: ScriptInfo -> [LSLValue] -> WorldM m (EvalResult,LSLValue))"

------------------------------------------------------------------------------------------------------    
--- Predefined function implementation stuff ---------------------------------------------------------
--
--    
data PredefFunc m = PredefFunc { predefFuncName :: String, 
                               predefAllowedKey :: Maybe String,
                               predefAllowedSID :: Maybe String,
                               predefExpectedArgs :: [Maybe LSLValue],
                               predef :: ScriptInfo -> [LSLValue] -> WorldM m (EvalResult,LSLValue) }
    deriving (Show)

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

llMessageLinked :: Monad m => ScriptInfo -> [LSLValue] -> WorldM m (EvalResult,LSLValue)       
llMessageLinked (ScriptInfo oid pid sid pkey _) [IVal link,IVal val,SVal msg,KVal key] =
    do mNumPrims <- queryObject oid (\ o -> return $ length (primKeys o))
       case mNumPrims of
           Nothing -> do logAMessage LogWarn "sim" ("object " ++ oid ++ " not found!")
                         return (EvalIncomplete,VoidVal)
           Just n ->
              let targetList =
                      if link == linkSet then [0..(n-1)]
                      else if link == linkAllOthers then delete pid [0..(n-1)]
                      else if link == linkAllChildren then [1..(n-1)]
                      else if link == linkThis then [pid]
                      else [link - 1] in
                   do mapM (flip (pushEvents oid) (Event "link_message" [IVal pid,IVal val,SVal msg, KVal key] [])) targetList
                      return (EvalIncomplete,VoidVal)

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
       (VVal x y z) <- getPos pkey
       region <- getRegion pkey
       tick <- getTick
       Just name <- queryPrim pkey primName
       putWorldEvent tick $ Chat chan name pkey message (region,(x,y,z)) (Just 20.0)
       return (EvalIncomplete,VoidVal)
llListen (ScriptInfo oid pid sid pkey event) [IVal chan, SVal sender, KVal key, SVal msg] =
    do lid <- registerListener $ Listener pkey sid chan sender key msg
       return (EvalIncomplete,IVal lid)
llListen info params = do
    logAMessage LogError "sim" ("invalid call to llListen.  info = " ++ (show info) ++ ", params = " ++ (show params))
    return (EvalIncomplete, IVal 0)
    
llFrand _ [FVal maxval] =
    do r <- wrand
       continueWith $ FVal (maxval * r)

llGetPermissions (ScriptInfo oid pid sid pk event) [] =  fromErrorT (IVal 0) $ do
    db <- lift getWorldDB
    Right val <- lookupDB ["script","permission",pk,sid,"lastmask"] db
    return val

llRequestPermissions (ScriptInfo _ _ sid pk _) [KVal k, IVal mask] =
    do avatars <- getWorldAvatars
       case lookup k avatars of
           Nothing -> logAMessage LogInfo (pk ++ ":" ++ sid) ("Invalid permissions request: no such avatar: " ++ k)
           _ -> getTick >>= flip putWorldEvent (PermissionRequestEvent pk sid k mask)

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
           
-- TODO: should check for av/prim in same region
-- TODO: no concept of online/offline for av           
llKey2Name info [KVal k] =
    do avs <- getWorldAvatars
       case lookup k avs of
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

llSetAlpha (ScriptInfo _ _ _ pkey _) [FVal alpha, IVal face] =
    if face == -1
        then runErrFace pkey face (EvalIncomplete,VoidVal) $ do 
                faces <- getPrimFaces pkey
                let faces' = map (\ face -> face { faceAlpha = alpha }) faces
                lift $ setPrimFaces pkey faces'
                continueWith VoidVal
        else setPrimFaceAlpha pkey face alpha >> continueWith VoidVal

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

llSetColor (ScriptInfo _ _ _ pkey _) [color, IVal face] =
    if face == -1
        then runErrFace pkey face (EvalIncomplete,VoidVal) $ do
                faces <- getPrimFaces pkey
                let colorVal = vVal2Vec color
                let faces' = map (\ face -> face { faceColor = colorVal }) faces
                lift $ setPrimFaces pkey faces'
                continueWith VoidVal
        else setPrimFaceColor pkey face (vVal2Vec color) >> continueWith VoidVal
        
getPos pkey = runErrPrim pkey
                  (VVal 0.0 0.0 0.0)
                  (getPrimPosition pkey >>= return . vec2VVal)

getRegion pkey = return (0,0)

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
    do result <- runAndLogIfErr ("object " ++ oid ++ " not found") (IVal 0) $ do 
           masks <- getPrimPermissions oid
           let base = if null masks then 0x0008e000 else masks !! 0
           let n = length masks
           let mask = if maskId < n then masks !! maskId else 
                          if maskId == 3 then 0 else base
           return (IVal mask)
       continueWith result
       
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
        case lookup k avatars of
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

--------------
-- get/set prim parameters


continueWith val = return (EvalIncomplete,val)

-- all the predefined functions for which implementations have been created
defaultPredefs :: Monad m => [PredefFunc m]
defaultPredefs = map (\(x,y) -> defaultPredef x y) 
    ([
        ("llFrand",llFrand),
        ("llGetAlpha",llGetAlpha),
        ("llGetBoundingBox",llGetBoundingBox),
        ("llGetColor",llGetColor),
        ("llGetLocalPos",llGetLocalPos),
        ("llGetNumberOfPrims",llGetNumberOfPrims),
        ("llGetNumberOfSides",llGetNumberOfSides),
        ("llGetObjectDesc", llGetObjectDesc),
        ("llGetObjectName", llGetObjectName),
        ("llGetObjectPermMask", llGetObjectPermMask),
        ("llGetObjectPrimCount", llGetObjectPrimCount),
        ("llGetOwner", llGetOwner),
        ("llGetPos", llGetPos),
        ("llGetScale", llGetScale),
        ("llKey2Name", llKey2Name),
        ("llListRandomize",llListRandomize),
        ("llListen", llListen),
        ("llMessageLinked", llMessageLinked),
        ("llOwnerSay", llOwnerSay),
        ("llSay",llSay),
        ("llSetAlpha", llSetAlpha),
        ("llSetColor", llSetColor),
        ("llSetObjectName",llSetObjectName),
        ("llSetObjectDesc",llSetObjectDesc),
        ("llSetPos",llSetPos),
        ("llSetScale",llSetScale),
        ("llSleep", llSleep)
    ] ++ internalLLFuncs)
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
         
data Listener = Listener {
                       listenerPrimKey :: String,
                       listenerScriptName :: String,
                       listenerChannel :: Int,
                       listenerName :: String,
                       listenerKey :: String,
                       listenerMsg :: String }
    deriving (Show)
    

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

pushEvents oid pid e = do
    fromErrorT () $ do
            objects <- lift getObjects
            o <- M.lookup oid objects
            key <- lookupByIndex pid (primKeys o)
            prims <- lift $ getPrims
            p <- M.lookup key prims
            lift $ mapM_ (pushEvent e key) (primScripts p)
    
pushEvent e key sid =
    do scripts <- getWorldScripts
       case M.lookup (key,sid) scripts of
           Nothing -> logAMessage LogWarn "sim" ("no such script: " ++ (show (key, sid)))
           Just (s,q) -> setWorldScripts (M.insert (key,sid) (s,q ++ [e]) scripts)

pushEventToPrim e key =
    do prims <- getPrims
       case M.lookup key prims of
           Nothing -> logAMessage LogWarn "sim" ("no such prim: " ++ key)
           Just p -> mapM_ (pushEvent e key) (primScripts p)
           
pushEventToObject e key =
    do objects <- getObjects
       case M.lookup key objects of
           Nothing -> logAMessage LogWarn "sim" ("no such object: " ++ key)
           Just o ->  mapM_ (pushEventToPrim e) (primKeys o)

getObjectNames :: (Monad m) => WorldM m [String]
getObjectNames = getObjects >>= (return . M.keys)
getListenerIds :: (Monad m) => WorldM m [Int]
getListenerIds = liftM (map fst) getListeners
getObject name = liftM (M.lookup name) getObjects

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
               worldAvatars = [],
               worldBreakpointManager = emptyBreakpointManager,
               worldSuspended = Nothing
           }
           
newWorld' slice maxt iq lib scripts avatars = 
    (newWorld slice maxt iq) { 
        wscripts = scripts, 
        wlibrary = lib,
        worldAvatars = avatars }
newWorld'' slice maxt iq lib scripts avatars objs prims activeScripts valueDB =
    (newWorld' slice maxt iq lib scripts avatars) {
        wobjects = objs,
        wprims = prims,
        worldScripts = activeScripts,
        worldDB = valueDB }
        
addObjectToInventory name obj world = world { inventory = (name,obj):(inventory world) }

ticksPerSecond :: Int
ticksPerSecond = 1000

data WorldEventType = CreatePrim { wePrimName :: String, wePrimKey :: String }
                    | AddScript (String,String) String Bool -- script, prim key, activate
                    | ResetScript String String -- prim key, script name
                    | ResetScripts String -- object name
                    | WorldSimEvent { worldSimEventName :: String, worldSimEventArgs :: [SimEventArg] }
                    | Chat { chatChannel :: Int, chatterName :: String, chatterKey :: String, chatMessage :: String,
                             chatLocation :: ((Int,Int),(Float,Float,Float)),
                             chatRange :: Maybe Float }
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

putWorldEvent tick we = 
    do weq <- getWQueue
       setWQueue $ putWQ tick we weq

checkBp bp sm =
    do  bpm <- getWorldBreakpointManager
        let (result,bpm',sm') = checkBreakpoint bp bpm sm
        setWorldBreakpointManager bpm'
        return (result,sm')
       
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
      setListeners ((id,listener):listeners)
      return id

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
          case lookup script scripts of
              Nothing -> logAMessage LogWarn "sim" ("no such script: " ++ script)
              Just (Invalid s) -> do
                  updatePrim (\ p -> return $ addScript p name) key
                  scripts <- getWorldScripts
                  setWorldScripts (M.insert (key,name) (Invalid s,[]) scripts)
              Just (Valid code) -> do
                  updatePrim (\ p -> return $ addScript p name) key
                  let sstate = initLSLScript code
                  scripts <- getWorldScripts
                  setWorldScripts (M.insert (key,name) (Valid sstate,[Event "state_entry" [] []]) scripts)
processEvent chat@(Chat chan name key msg location range) =
    do listeners <- getListeners 
       locatedListeners <- mapM locateListener (map snd listeners)
       let listeners'= [ l | (l,_) <- filter (matchListener chat) locatedListeners]
       -- event goes to all UNIQUE addresses in list
       let addresses = nub $ map listenAddress listeners'
       mapM (\ (key,sid) -> pushEvent (Event "listen" [IVal chan, SVal name, KVal key, SVal msg] []) key sid) addresses
       return ()
    where locateListener listener = do
              (VVal x y z) <- getPos (listenerPrimKey listener)
              region <- getRegion (listenerPrimKey listener)
              return (listener,(region,(x,y,z)))
processEvent (WorldSimEvent name args) = 
    case M.lookup name eventDescriptors of
        Nothing -> logAMessage LogWarn "sim" ("event " ++ name ++ " not understood")
        Just def -> handleSimInputEvent def args
processEvent (PermissionRequestEvent pk sname ak mask) = 
    do  db <- getWorldDB
        let db' = insertDB ["script","permission",pk,sname,"avatar"] (KVal ak) $
                  insertDB ["script","permission",pk,sname,"mask"] (IVal mask) $
                  insertDB ["script","permission",pk,sname,"lastavatar"] (KVal ak) $
                  insertDB ["script","permission",pk,sname,"lastmask"] (IVal mask) db
        setWorldDB db'
        getTick >>= flip putWorldEvent (mkRunTimePermissionsEvent pk sname mask)
        
    
processEvent _ = error "not implemented"

matchListener (Chat chan' sender' key' msg' (region,position) range) ((Listener pkey sid chan sender key msg),(region',position')) =
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
checkAndRunScript _ (Invalid _,_) = return ()
checkAndRunScript k@(pkey,sname) (Valid img, q) =
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
               setWorldScripts (M.insert (pkey,sname) (Valid img'',q') scripts)
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

data WorldDef = WorldDef { worldDefScript :: String }

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
        simStateInfoAvatars = map (\ (_,a) -> (avatarKey a, avatarName a)) (worldAvatars world)
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
     
data SimEvent = SimEvent { simEventName :: String, simEventArgs :: [SimEventArg], simEventDelay :: Int }
    deriving (Show)
data SimEventArg = SimEventArg { simEventArgName :: String, simEventArgValue :: String }
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
                 let (Just (Valid img,eq)) = M.lookup (k,s) (worldScripts world) -- TODO: non-exhaustive but MUST succeed
                     stepMgr = stepManager img
                     img' = case command of
                                SimStep _ _ -> img { stepManager = setStepBreakpoint stepMgr }
                                SimStepOver _ _ -> img { stepManager = setStepOverBreakpoint stepMgr }
                                SimStepOut _ _ -> img { stepManager = setStepOutBreakpoint stepMgr }
                                _ -> img
                 in M.insert (k,s) (Valid img',eq) (worldScripts world)
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
                     let (Just (Valid img,eq)) = M.lookup (k,s) (worldScripts world''') -- TODO: non-exhaustive but MUST succeed
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
                    case lookup ak avatars of
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

logInitialProblems world =
    let log = msglog world
        newLog = [LogMessage 0 LogWarn "init" ("script \"" ++ name ++ "\" in prim " ++ prim ++ " failed to activate because of error: " ++ s) |
                          ((prim,name),(Invalid (_,s),_)) <- M.toList (worldScripts world) ] ++ log
    in world { msglog = newLog }