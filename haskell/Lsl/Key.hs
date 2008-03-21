module Lsl.Key (nextKey, nullKey) where
import Data.List
nullKey = "00000000-0000-0000-0000-000000000000"
keyCharSet = ['0'..'9'] ++ ['a'..'z']

nextKey [] = error "can't generate a successor key!"
nextKey ('-':cs) = '-':(nextKey cs)
nextKey (c:cs) =
    case elemIndex c keyCharSet of
         Nothing -> c:(nextKey cs)
         Just i -> if i == length keyCharSet then '0' : (nextKey cs) else (keyCharSet !! (i+1)):cs
