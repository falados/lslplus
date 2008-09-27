module Lsl.AvEvents(AvatarOutputEvent(..),
                    AvatarInputEvent(..)) where

data AvatarOutputEvent =
      AvatarTouch { avatarTouchPrimKey :: String, avatarTouchDuration :: Float }
    | AvatarWhisper { avatarChatChannel :: Int, avatarChatMessage :: String }
    | AvatarSay { avatarChatChannel :: Int, avatarChatMessage :: String }
    | AvatarShout { avatarChatChannel :: Int, avatarChatMessage :: String }
    | AvatarPay { avatarPayPrimKey :: String, avatarPayAmount :: Int }
    | AvatarControl { avatarNewControlBits :: Int } deriving (Read,Show)

data AvatarInputEvent =
      AvatarOwnerSay { avatarOwnerSayPrimKey :: String, avatarOwnerSayMsg :: String }
    | AvatarHearsChat { avatarHearsChatFromName :: String, avatarHearsChatFromKey :: String, avatarHearsChatMsg :: String }
    deriving (Read,Show)