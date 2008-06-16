module Lsl.Util (
    ctx,
    swap,
    extractOne, 
    readM,
    filtMap, 
    filtMapM,
    listProd, 
    hasCycle, 
    filter2, 
    modify, 
    modifyLookup, 
    modifyM, 
    modifyLookupM, 
    modifyByPredM,
    replace,
    replaceLookup,
    lookupByIndex,
    lookupM,
    removeLookup,
    findM,
    elemAtM,
    unwrap,
    weave,
    separateWith,
    isPrefix,
    indexOf,
    fromInt,
    headsAndTails,
    trimFront,
    trimEnd,
    trim,
    tuplify,
    cut,
    dist3d,
    dist3d2,
    mag3d2,
    mag3d,
    norm3d,
    diff3d,
    add3d,
    neg3d,
    scale3d,
    rot3d,
    unescape,
    processLines,
    processLinesS,
    quaternionToMatrix,
    matrixToQuaternion,
    quaternionMultiply,
    Permutation3(..),
    quaternionToRotations,
    rotationsToQuaternion,
    cross,
    generatePermutation,
    fac) where

import Control.Monad
import Control.Monad.State hiding (modify)
import Data.List
import Debug.Trace

import Lsl.Math

import IO
import Network.URI

-- utilities

tuplify [] = []
tuplify (a:[]) = []
tuplify (a:b:rest) = (a,b) : tuplify rest

readM s = case reads s of
             [] -> fail ("unable to parse " ++ s)
             ((v,_):_) -> return v

whenM :: Monad m => m Bool -> m () -> m ()
whenM p action = do
    val <- p
    when val action

ctx s (Left s') = fail (s ++ ": " ++ s' )
ctx s (Right v) = return v

swap (x,y) = (y,x)

-- extract an element from a list based on a predicate to apply to each element, and return
-- a tuple containing the element (or Nothing) and the other elements of the list.
extractOne :: (a -> Bool) -> [a] -> (Maybe a, [a])
extractOne pred list =
   let f [] others = (Nothing, others)
       f (x:xs) others = if pred x then (Just x, others ++ xs) else f xs (x:others)
   in
       f list []

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
filtMap f [] = []
filtMap f (x:xs) = case f x of
   Nothing -> filtMap f xs
   Just y  -> y : filtMap f xs

filtMapM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
filtMapM f [] = return []
filtMapM f (x:xs) =
    do  r <- f x
        case r of
            Nothing -> filtMapM f xs
            Just y -> liftM (y:) (filtMapM f xs)
            
unwrap f = f >>= id

-- form a cartesian product of two lists, applying the function to
-- each element of the resulting product
listProd :: (a -> b -> c) -> [a] -> [b] -> [c]
listProd f x y = 
  concat (map (\p -> map (f p) y) x)

-- test to see if a list of tuples (taken to be edges in a graph)
-- has a cycle of a specified minimum length
hasCycle :: (Eq a) => Int -> a -> [(a,a)] -> Bool
hasCycle minLength x list =
  let hasPath i x y list =
        i >= minLength && elem (x,y) list ||
        case extractOne (\(x',_) -> x == x') list of
            (Nothing,_) -> False
            (Just (_,x'), rest) ->
                hasPath (i+1) x' y rest || hasPath (i+1) x y rest
  in hasPath 1 x x list
     
-- filter a list, returning a tuple containing a list of elements which match the predicate,
-- and a list of elements which do not.
filter2 :: (a -> Bool) -> [a] -> ([a],[a])
filter2 f l =
  let filt f [] r = r
      filt f (x:xs) (ys, ns) = if (f x) then filt f xs (x:ys, ns) else filt f xs (ys, x:ns) in
          filt f l ([],[])

-- modify a single element of a list
modify :: Int -> (a -> a) -> [a] -> [a]
modify index f l =
    map (\ (i,v) -> if i == index then f v else v) $ zip [0..] l

-- replace a single element of a list
replace :: Int -> a -> [a] -> [a]
replace index v = modify index (const v) 
    
modifyM :: (Monad m) => Int -> (a -> m a) -> [a] -> m [a]
modifyM index f l =
    mapM (\ (i,v) -> if i == index then f v else return v) $ zip [0..] l

modifyByPredM :: (Monad m) => (a -> Bool) -> (a -> m a) -> [a] -> m [a]
modifyByPredM p f l =
    mapM (\ v -> if p v then f v else return v) l
        
modifyLookup :: Eq a => a -> (a -> b -> b) -> [(a,b)] -> [(a,b)]
modifyLookup key f l =
    map (\ (k,v) -> if key == k then (k, f k v) else (k,v)) l

replaceLookup key v l =
    map (\ (k,v') -> if key == k then (k,v) else (k,v')) l
    
modifyLookupM :: (Functor m, Monad m, Eq a) => a -> (a -> b -> m b) -> [(a,b)] -> m [(a,b)]
modifyLookupM key f l =
    mapM (\ (k,v) -> if key == k then fmap ((,)k) (f k v) else return (k,v)) l

lookupByIndex :: Monad m => Int -> [a] -> m a
lookupByIndex i l = lookupM i $ zip [0..] l

removeLookup :: Eq a => a -> [(a,b)] -> [(a,b)]
removeLookup k l = let (xs,ys) = break ((k==).fst) l in
    case ys of
       (z:zs) -> xs ++ zs
       _ -> l
       
-- interleave the elements of two lists
weave :: [a] -> [a] -> [a]
weave as [] = as
weave [] bs = bs
weave (a:as) (b:bs) = a:b:(weave as bs)

separateWith :: a -> [a] -> [a]
separateWith sep list = weave list (replicate (length list - 1) sep)

isPrefix :: Eq a => [a] -> [a] -> Bool
isPrefix p list = p == take (length p) list

indexOf :: Eq a => [a] -> [a] -> Maybe Int
indexOf sub list = elemIndex True $ map (isPrefix sub) (tails list) 

fromInt :: Num a => Int -> a
fromInt = fromInteger . toInteger

cut :: Int -> Int -> [a] -> ([a],[a])
cut start end src = (take start src, drop end src)

elemAtM :: (Monad m) => Int -> [a] -> m a
elemAtM index list =
    if index >= 0 && index < length list then return (list !! index)
    else fail ("index " ++ (show index) ++ " out of range")
  
headsAndTails [] = []
headsAndTails (a:as) = (a,as):(headsAndTails as)

trimFront s = let (_,s') = span (==' ') s in s'
trimEnd s = let (_,s') = span (==' ') $ reverse s in reverse s'

trim s = trimEnd $ trimFront s

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

processLinesST :: a -> String -> (String -> StateT a IO String) -> IO ()
processLinesST state term f =
    do s <- getLine
       when (term /= s) $ do
           (s',state') <- runStateT (f $ unescape s) state
           putStr (escape s')
           putStr "\n"
           processLinesST state' term f
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
generatePermutation [] i = []
generatePermutation l  i = 
    let (n::Integer) = toInteger (length l) in
        if i < fac n then
            let modulus = fac (n - 1)
                (ix::Int) = fromInteger $ (i `div` modulus) in
            case splitAt ix l of
                (xs,y:ys) -> y : (generatePermutation (xs ++ ys) (i `mod` modulus))
                _ -> error ""
        else error "no such permutation!!!"
