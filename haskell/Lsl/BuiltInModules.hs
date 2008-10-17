{-# OPTIONS_GHC -XQuasiQuotes #-}
module Lsl.BuiltInModules(avEventGen) where

import Lsl.Structure
import Lsl.Parse
import Lsl.QQ
import Data.List

-- (Just avEventGenAST) = parseModuleFromString $ intercalate "\n"
--     ["$module ",
--     "string mkTouch(string primKey, float duration) {",
--     "    return \"AvatarTouch {avatarTouchPrimKey = \\\"\" + primKey + \"\\\", avatarTouchDuration = \" + (string) duration +\"}\";",
--     "}",
--     "string mkWhisper(integer chan, string message) { ",
--     "    return \"AvatarWhisper { avatarChatChannel = \" + (string)chan + \", avatarChatMessage = \\\"\" + message + \"\\\"}\";",
--     "}",
--     "string mkSay(integer chan, string message) { ",
--     "    return \"AvatarSay { avatarChatChannel = \" + (string)chan + \", avatarChatMessage = \\\"\" + message + \"\\\"}\";",
--     "}",
--     "string mkShout(integer chan, string message) { ",
--     "    return \"AvatarShout { avatarChatChannel = \" + (string)chan + \", avatarChatMessage = \\\"\" + message + \"\\\"}\";",
--     "}",
--     "string mkPay(string primKey, integer amount) { ",
--     "    return \"AvatarPay { avatarPayPrimKey = \\\"\" + primKey + \"\\\", avatarPayAmount = \" + (string) amount +\"}\";",
--     "}",
--     "string mkControl(integer newControlBits) { ",
--     "    return \"AvatarControl { avatarNewControlBits = \" + (string) newControlBits + \"}\";",
--     "}"]
--     
-- avEventGen = ("$avEventGen", avEventGenAST)
x = "hi"
avEventGenAST = [$lslm|$module
        string mkTouch(string primKey, float duration) {
            return "AvatarTouch {avatarTouchPrimKey = \"" + primKey + "\", avatarTouchDuration = " + (string) duration + "}";
        }
        string mkWhisper(integer chan, string message) {
            return "AvatarWhisper { avatarChatChannel = " + (string)chan + ", avatarChatMessage = \"" + message + "\"}";
        }
        string mkSay(integer chan, string message) {
            return "AvatarSay { avatarChatChannel = " + (string)chan + ", avatarChatMessage = \"" + message + "\"}";
        }
        string mkShout(integer chan, string message) {
            return "AvatarShout { avatarChatChannel = " + (string)chan + ", avatarChatMessage = \"" + message + "\"}";
        }
        string mkPay(string primKey, integer amount) {
            return "AvatarPay { avatarPayPrimKey = \"" + primKey + "\", avatarPayAmount = " + (string) amount + "}";
        }
        string mkControl(integer newControlBits) {
            return "AvatarControl { avatarNewControlBits = " + (string) newControlBits + "}";
        }
        string mkFoo() { return $string:x; }
    |]
    
avEventGen = ("$avEventGen", avEventGenAST)