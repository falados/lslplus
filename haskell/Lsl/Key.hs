module Lsl.Key (nextKey, nullKey, mkKey) where
import Data.List(elemIndex)
import Text.Printf(printf)
nullKey = "00000000-0000-0000-0000-000000000000"
keyCharSet = ['0'..'9'] ++ ['a'..'z']

nextKey [] = error "can't generate a successor key!"
nextKey ('-':cs) = '-':(nextKey cs)
nextKey (c:cs) =
    case elemIndex c keyCharSet of
         Nothing -> c:(nextKey cs)
         Just i -> if i == length keyCharSet then '0' : (nextKey cs) else (keyCharSet !! (i+1)):cs
         
mkKey :: Integer -> String
mkKey i =
    let s0 = printf "%032x" i
        (p1,s1) = splitAt 8 s0
        (p2,s2) = splitAt 4 s1
        (p3,s3) = splitAt 4 s2
        (p4,p5) = splitAt 4 s3
    in p1 ++ ('-':p2) ++ ('-':p3) ++ ('-':p4) ++ ('-':p5)