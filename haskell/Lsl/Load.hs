module Lsl.Load(
    loadScripts,
    loadModules) where

import Control.Exception(SomeException(..),tryJust)
import Control.Monad.Error(liftIO)
import Lsl.BuiltInModules(avEventGen)
import Language.Lsl.Syntax(validLSLScript,validLibrary,SourceContext(..))
import Lsl.Parse(parseModule, parseScript)

parseFiles p files =
    let parseFile (name,path) =
            do result <- tryJust (\ e@(SomeException x) -> Just (show e)) $ p path
               case result of
                   Left msg -> return (name,Left (UnknownSourceContext,msg))
                   Right (Left err) -> return (name,Left err)
                   Right (Right m) -> return (name,Right m)
    in liftIO $ mapM parseFile files

loadModules files =
    do parseResults <- parseFiles parseModule files
       let (bad,ok) = splitResults parseResults
       let augLib = validLibrary (avEventGen:ok)
       return (augLib ++ (map (\ (n,err) -> (n,Left err)) bad))
       --return (validated ++ (map (\ (n,err) -> (n,Left err)) bad))

loadScripts library files =
    do parseResults <- parseFiles parseScript files
       let (bad,ok) = splitResults parseResults
       return $ (map (\ (n,script) -> (n,validLSLScript library script)) ok) ++ 
           (map (\ (n,err) -> (n,Left err)) bad)
           
splitResults [] = ([],[])
splitResults ((name,Left err):xs) =
    let (lefts,rights) = splitResults xs in
        ((name,err):lefts,rights)
splitResults ((name, Right m):xs) =
    let (lefts,rights) = splitResults xs in
        (lefts,(name,m):rights)
