module Lsl.Load(
    loadScripts,
    loadLibrary,
    loadScripts',
    loadModules) where

import Control.Exception
import Control.Monad
import Control.Monad.Error
import Data.List
import Lsl.Structure
import Lsl.Parse
import System
import System.Directory
import System.FilePath
import System.IO
import Lsl.Util

import Debug.Trace

loadLibrary :: ErrorT String IO AugmentedLibrary
loadLibrary =
    let checkLib (name,Left s) = putErr ("warning: " ++ name ++ " failed parse: " ++ s ++ "\n") >> return Nothing
        checkLib (name,Right lmodule) = return $ Just (name, lmodule)
    in do libDir <- tryElse "lslLib" (getEnv "LSL_LIB_DIR")
          parsedModules <- parseFiles parseModule libDir ".lslm"
          modules <- lift $ filtMapM checkLib parsedModules
          return $ validLibrary modules

loadScripts :: Library -> ErrorT String IO [(String,Validity ([Global],[Func],[State]))]
loadScripts library =
    let checkScript (name, Left s) = putErr ("warning: " ++ name ++ " failed parse: " ++ s ++ "\n") >> (return $ (name,Invalid (UnknownSourceContext,s)))
        checkScript (name, Right script) = return $ (name, validLSLScript library script)
    in do scriptDir <- tryElse "lslScripts" (getEnv "LSL_SCRIPT_DIR")
          parsedScripts <- parseFiles parseScript scriptDir ".lsl"
          lift $ mapM checkScript parsedScripts

parseFiles p dir suffix =
    do  names <- liftM (filter (isSuffixOf suffix)) $
            try1 (getDirectoryContents dir) ("error: directory '" ++ dir ++ "' not found")
        let paths = map (combine dir) names
        results <- liftIO $ mapM p paths
        return $ zip names results

parseFiles' p files =
    let parseFile (name,path) =
            do result <- tryJust (return.show) $ p path
               case result of
                   Left msg -> return (name,Left (UnknownSourceContext,msg))
                   Right (Left err) -> return (name,Left err)
                   Right (Right m) -> return (name,Right m)
    in liftIO $ mapM parseFile files

loadModules files =
    do parseResults <- parseFiles' parseModule' files
       let (bad,ok) = splitResults parseResults
       let augLib = validLibrary ok
       return (augLib ++ (map (\ (n,err) -> (n,Invalid err)) bad))
       --return (validated ++ (map (\ (n,err) -> (n,Invalid err)) bad))

loadScripts' library files =
    do parseResults <- parseFiles' parseScript' files
       let (bad,ok) = splitResults parseResults
       return $ (map (\ (n,script) -> (n,validLSLScript library script)) ok) ++ 
           (map (\ (n,err) -> (n,Invalid err)) bad)
           
splitResults [] = ([],[])
splitResults ((name,Left err):xs) =
    let (lefts,rights) = splitResults xs in
        ((name,err):lefts,rights)
splitResults ((name, Right m):xs) =
    let (lefts,rights) = splitResults xs in
        (lefts,(name,m):rights)

        
        
putErr = hPutStr stderr

try1 :: IO a -> String -> ErrorT String IO a
try1 action errMsg =
    do result <- liftIO $ try action
       case result of
           Left _ -> throwError errMsg
           Right v -> return v

tryElse defaultVal action = liftIO $ tryJust (const $ Just defaultVal) action >>= return . (either id id)
