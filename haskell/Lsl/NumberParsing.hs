module Lsl.NumberParsing(readHexFloat,readInt) where

import Data.Char
import Text.ParserCombinators.Parsec
import Control.Monad.Error

hexFloat =
    do char '0'
       oneOf "xX"
       wholeDigits <- many1 hexDigit
       let w = foldl (\ x y -> x * 16.0 + y) 0.0 $ map (fromIntegral . digitToInt) wholeDigits
       frac <- option 0.0 fractionalPart
       p <- expPart
       return $ (w + frac) * p
       
fractionalPart =
    do char '.'
       digits <- many1 hexDigit
       return $ foldr (\ x y -> (x + y) / 16.0) 0.0 $ map (fromIntegral . digitToInt) digits
       
expPart =
    do oneOf "pP"
       f <- option (2^) ((char '+' >> return (2^)) <|> (char '-' >> return ((1/).(2^)))) 
       digits <- many1 digit
       let exp = foldl (\ x y -> 10 * x + y) 0 $ map digitToInt digits
       return $ f exp

hexFloatAndTail :: Fractional a => GenParser Char b [(a,String)]
hexFloatAndTail =
    do v <- hexFloat
       rest <- many (satisfy (const True))
       return [(realToFrac v,rest)]

readHexFloat s = 
    case parse hexFloatAndTail "" s of
        Left _ -> []
        Right v -> v
        
hexInt =
    do oneOf "xX"
       digits <- many1 hexDigit
       return $ foldl (\ x y -> x * 16 + y) 0 $ map digitToInt digits
       
decimalInt =
    do digits <- many1 digit
       return $ foldl (\ x y -> x * 10 + y) 0 $ map digitToInt digits

int = do char '0'
         (hexInt <|> decimalInt <|> return 0)
   <|> decimalInt
   
intAndTail =
    do i <- int
       rest <- many (satisfy (const True))
       return (i,rest)
       
readInt s =
    case parse intAndTail "" s of
        Left _ -> []
        Right v -> [v]