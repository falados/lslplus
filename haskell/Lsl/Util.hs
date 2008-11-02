{-# OPTIONS_GHC -fwarn-unused-binds -XNoMonomorphismRestriction #-}
module Lsl.Util (
    mlookup,
    ilookup,
    throwStrError,
    ctx,
    readM,
    filtMap, 
    filtMapM,
    lookupByIndex,
    lookupM,
    removeLookup,
    findM,
    elemAtM,
    indexOf,
    fromInt,
    tuplify,
    cut,
    unescape,
    processLines,
    processLinesS,
    generatePermutation,
    fac,
    module Language.Lsl.Internal.Math
    ) where

import Control.Monad(liftM,when)
import Control.Monad.Error(MonadError(..),Error(..))
import Data.List(find,elemIndex,isPrefixOf,tails)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap

-- import Debug.Trace

import Language.Lsl.Internal.Math

import IO(hFlush,stdout)
import Network.URI(escapeURIString,isUnescapedInURI,unEscapeString)

-- lifting lookups for Map (if key is instance of Show) and IntMap

mlookup k m = 
    maybe (throwError $ "key " ++ show k ++ " not found") return (Map.lookup k m)
ilookup i m = 
    maybe (throwError $ "key " ++ show i ++ " not found") return (IntMap.lookup i m)

throwStrError :: (Error e, MonadError e m) => String -> m a
throwStrError = throwError . strMsg
    
tuplify [] = []
tuplify (_:[]) = []
tuplify (a:b:rest) = (a,b) : tuplify rest

readM s = case reads s of
             [] -> fail ("unable to parse " ++ s)
             ((v,_):_) -> return v

ctx s (Left s') = fail (s ++ ": " ++ s' )
ctx _ (Right v) = return v

-- monadified lookup
lookupM :: (Monad m, Eq a, Show a) => a -> [(a,b)] -> m b
lookupM x l =
   case lookup x l of
       Nothing -> fail ((show x) ++ " not found")
       Just y -> return y

-- monadified find
findM :: Monad m => (a -> Bool) -> [a] -> m a
findM p l =
    case find p l of
        Nothing -> fail "not found!"
        Just v -> return v
       
-- filter a list while mapping
filtMap :: (a -> Maybe b) -> [a] -> [b]
filtMap _ [] = []
filtMap f (x:xs) = case f x of
   Nothing -> filtMap f xs
   Just y  -> y : filtMap f xs

filtMapM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
filtMapM _ [] = return []
filtMapM f (x:xs) =
    do  r <- f x
        case r of
            Nothing -> filtMapM f xs
            Just y -> liftM (y:) (filtMapM f xs)
            
lookupByIndex :: Monad m => Int -> [a] -> m a
lookupByIndex i l = lookupM i $ zip [0..] l

removeLookup :: Eq a => a -> [(a,b)] -> [(a,b)]
removeLookup k l = let (xs,ys) = break ((k==).fst) l in
    case ys of
       (_:zs) -> xs ++ zs
       _ -> l
       
indexOf :: Eq a => [a] -> [a] -> Maybe Int
indexOf sub list = elemIndex True $ map (isPrefixOf sub) (tails list) 

fromInt :: Num a => Int -> a
fromInt = fromInteger . toInteger

cut :: Int -> Int -> [a] -> ([a],[a])
cut start end src = (take start src, drop end src)

elemAtM :: (Monad m) => Int -> [a] -> m a
elemAtM index list =
    if index >= 0 && index < length list then return (list !! index)
    else fail ("index " ++ (show index) ++ " out of range")
  

unescape = unEscapeString

-- interactively process lines, stopping when a terminator value is read
-- each line is assumed to be URI encoded.  It is decoded and passed to
-- some string handling function (String -> String).  The result is
-- reencoded and output.
processLines term f =
    do s <- getLine
       when (term /= s) $ do
           putStr (escape $ f (unescape s))
           putStr "\n"
           processLines term f
    where escape = escapeURIString isUnescapedInURI
          
processLinesS state term f =
    do s <- getLine
       --hPutStrLn stderr s
       when (term /= s) $ do
           let (newState,s') = f state (unescape s)
           putStrLn (escape s')
           hFlush stdout
           processLinesS newState term f
    where escape = escapeURIString isUnescapedInURI

-- TODO: fix this definition!
fac :: Integer -> Integer
fac 0 = 1
fac 1 = 1
fac n = n * fac (n - 1)

-- generate the nth permutation of a list...
-- permutations are numbered such that if you gave each element in the original ordering
-- a number (e.g., for a 3 element list [0,1,2]), and then generated all the permutations,
-- and then treated each permutation as a base N number, (e.g. 012, or 12 base 3), and
-- then sorted the permutation, the number of the permutation equals its index in the 
-- list of sorted permutations... simple!
generatePermutation [] _ = []
generatePermutation l  i = 
    let n :: Integer
        n = toInteger (length l) in
        if i < fac n then
            let modulus = fac (n - 1)
                ix :: Int
                ix = fromInteger $ (i `div` modulus) in
            case splitAt ix l of
                (xs,y:ys) -> y : (generatePermutation (xs ++ ys) (i `mod` modulus))
                _ -> error ""
        else error "no such permutation!!!"
