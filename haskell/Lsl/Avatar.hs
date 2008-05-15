module Lsl.Avatar(Avatar(..), defaultAvatar) where

import qualified Data.IntMap as IM

import Lsl.Inventory
import Lsl.Type
data Avatar = Avatar { avatarKey :: String,
                       avatarName :: String,
                       avatarRegion :: (Int,Int),
                       avatarPosition :: (Float,Float,Float),
                       avatarRotation :: (Float,Float,Float,Float),
                       avatarHeight :: Float
                       avatarState :: Int,
                       avatarInventory :: [InventoryItem],
                       avatarAttachments :: IM.IntMap String }
     deriving (Show)

defaultAvatar key = 
    Avatar { avatarKey = key,
             avatarName = "Default Avatar",
             avatarRegion = (0,0),
             avatarPosition = (128.0,128.0,0.0),
             avatarRotation = (0.0, 0.0, 0.0, 1.0),
             avatarHeight = 2,
             avatarState = 0,
             avatarInventory = [],
             avatarAttachments = IM.empty }
                       