{-# LANGUAGE Rank2Types #-}
module Data.Generics.Extras.Schemes (
    downup,
    everythingTwice
 ) where

------------------------------------------------------------------------------

import Data.Data
import Data.Generics.Aliases
import Control.Monad

everythingTwice :: (r -> r -> r) -> GenericQ r -> GenericQ r -> GenericQ r
everythingTwice k f g x
  = foldl k (f x) (gmapQ (everythingTwice k f g) x) `k` (g x)


downup :: Monad m => GenericM m -> GenericM m -> GenericM m

downup down up x = do x' <- down x
                      x'' <- gmapM (downup down up) x'
                      up x''
