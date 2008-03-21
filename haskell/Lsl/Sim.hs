module Lsl.Sim where

import Control.Exception
import Control.Monad.Error
import Data.List
import Lsl.Load
import Lsl.Parse
import Lsl.Structure
import Lsl.World1
import System
import System.Directory
import System.IO
import Lsl.Util

io :: MonadIO m => IO a -> m a
io = liftIO

tryElse defaultVal action = io $ tryJust (const $ Just defaultVal) action >>= return . (either id id)

try1 :: IO a -> String -> ErrorT String IO a
try1 action errMsg =
    do result <- io $ try action
       case result of
           Left _ -> throwError errMsg
           Right v -> return v

run1 :: [(String,String)] -> IO (Either String ())
run1 args = runErrorT $
    do  augLib <- loadLibrary
        let library = libFromAugLib augLib
        scripts <- loadScripts library
        eventList <- findEventList args
        lslExec eventList library scripts 1000 1000 2000 showLog
        return ()

findEventList :: [(String,String)] -> ErrorT String IO WorldEventQueue
findEventList args =
    do  fileName <- case lookup "--events" args of
            Nothing -> tryElse "events.lsle" (getEnv "LSL_EVENTS_FILE")
            Just n -> return n
        s <- try1 (readFile fileName) ("event file " ++ fileName ++ " not found")
        case (reads s)::[(WorldEventQueue,String)] of
             [] -> throwError ("could not parse " ++ fileName)
             (q,_):_ -> return q
            
run2 =
    do  result <- run1 []
        case result of
            Left msg -> putErr ("error: " ++ msg ++ "\n")
            Right _ -> return ()
            
putErr s = hPutStr stderr s

showLog w = io $ mapM_ (putStr . (++"\n")) (msglog w)