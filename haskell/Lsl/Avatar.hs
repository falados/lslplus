module Lsl.Avatar(Avatar(..), defaultAvatar) where

import Lsl.Type
data Avatar = Avatar { avatarKey :: String,
                       avatarName :: String,
                       avatarRegion :: (Int,Int),
                       avatarPosition :: (Float,Float,Float),
                       avatarRotation :: (Float,Float,Float,Float) }
     deriving (Show)
                       
defaultAvatar key = 
    Avatar { avatarKey = key,
             avatarName = "Default Avatar",
             avatarRegion = (0,0),
             avatarPosition = (128.0,128.0,128.0),
             avatarRotation = (0.0, 0.0, 0.0, 1.0) }
                       