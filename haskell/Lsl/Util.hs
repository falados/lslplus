module Lsl.Util (
    ctx,
    swap,
    extractOne, 
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
    cut,
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
import IO
import Network.URI

-- utilities

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

lookupByIndex :: Int -> [a] -> Maybe a
lookupByIndex i l = lookup i $ zip [0..] l

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

-- some random math functions

quaternionToMatrix (x,y,z,s) =
    ((s*s + x*x - y*y - z*z, 2*x*y - 2*s*z,         2*x*z + 2*s*y),
     (2*x*y + 2*s*z,         s*s - x*x + y*y - z*z, 2*y*z - 2*s*x),
     (2*x*z - 2*s*y,         2*y*z + 2*s*x,         s*s - x*x - y*y + z*z))
     
-- reversing the conversion:
matrixToQuaternion ((r00,r01,r02),(r10,r11,r12),(r20,r21,r22)) =
    let sign x = if (x < 0) then -1 else 1 in
        (sign (r21 - r12) * (sqrt (max 0 (1 + r00 - r11 - r22 ))) / 2,
         sign (r02 - r20) * (sqrt (max 0 (1 - r00 + r11 - r22))) / 2,
         sign (r10 - r01) * (sqrt (max 0 (1 - r00 - r11 + r22 ))) / 2,
         (sqrt (max 0  (1 + r00 + r11 + r22))) / 2)

quaternionMultiply (x1,y1,z1,s1) (x2,y2,z2,s2) =
    ((s1 * x2 + x1 * s2 + y1 * z2 - z1 * y2),
     (s1 * y2 - x1 * z2 + y1 * s2 + z1 * x2),
     (s1 * z2 + x1 * y2 - y1 * x2 + z1 * s2),
     (s1 * s2 - x1*x2 - y1 * y2 - z1 * z2))

rotationsToQuaternion :: Permutation3 -> (Float,Float,Float) -> (Float,Float,Float,Float)
rotationsToQuaternion order (x,y,z) = 
    let rx = (sin (x/2), 0.0, 0.0, cos (x/2))
        ry = (0.0, sin (y/2), 0.0, cos (y/2))
        rz = (0.0, 0.0, sin (z/2), cos (z/2))
        (r3,r2,r1) = permute order (rx,ry,rz)
    in ((r3 `quaternionMultiply` r2) `quaternionMultiply` r1)
    
data Permutation3 = P123 | P132 | P213 | P231 | P312 | P321

permute P123 (x,y,z) = (x,y,z)
permute P132 (x,y,z) = (x,z,y)
permute P213 (x,y,z) = (y,x,z)
permute P231 (x,y,z) = (y,z,x)
permute P312 (x,y,z) = (z,x,y)
permute P321 (x,y,z) = (z,y,x)

quatPermute (x,y,z,w) p = let (p1,p2,p3) = permute p (x,y,z) in (p1,p2,p3,w)

sign f = if f < 0 then -1 else 1

quaternionToRotations :: Permutation3 -> Bool -> (Float,Float,Float,Float) -> (Float,Float,Float)
quaternionToRotations rotOrder lh quat=
    let (p1,p2,p3,p0) = quatPermute quat rotOrder
        mult = if lh then -1 else 1
        sinTheta2 = 2 * (p0 * p2 + mult * p1 * p3)
        (sinTheta2',singularity) = 
            if (abs sinTheta2 >= 0.9999999) then (sign sinTheta2,True) else (sinTheta2,False)
        theta2 = asin sinTheta2'
        theta3 = if singularity then atan2 (p3*p0 + p1*p2) (0.5 - p1*p1 - p3*p3)
                 else atan2 (2 * (p0 * p3 - mult * p1 * p2)) (1 - 2 * (p2 * p2 + p3 * p3))
        theta1 = if singularity then 0
                 else atan2 (2 * ( p0 * p1 - mult * p2 * p3)) (1 - 2 * (p1 * p1 + p2 * p2))
    in (theta1,theta2,theta3)
    
cross (x1,y1,z1) (x2,y2,z2) = ((y1 * z2 - z1 * y2),(z1 * x2 - x1 * z2),(x1 * y2 - y1 * x2))

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
