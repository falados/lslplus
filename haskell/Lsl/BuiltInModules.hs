module Lsl.BuiltInModules(avEventGen) where

import Lsl.Parse
import Data.List

(Just avEventGenAST) = parseModuleFromString $ intercalate "\n"
    ["$module ",
    "string mkTouch(string primKey, float duration) {",
    "    return \"AvatarTouch {avatarTouchPrimKey = \\\"\" + primKey + \"\\\", avatarTouchDuration = \" + (string) duration +\"}\";",
    "}",
    "string mkWhisper(integer chan, string message) { ",
    "    return \"AvatarWhisper { avatarChatChannel = \" + chan + \", avatarChatMessage = \\\"\" + message + \"\\\"}\";",
    "}",
    "string mkSay(integer chan, string message) { ",
    "    return \"AvatarSay { avatarChatChannel = \" + chan + \", avatarChatMessage = \\\"\" + message + \"\\\"}\";",
    "}",
    "string mkShout(integer chan, string message) { ",
    "    return \"AvatarShout { avatarChatChannel = \" + chan + \", avatarChatMessage = \\\"\" + message + \"\\\"}\";",
    "}",
    "string mkPay(string primKey, integer amount) { ",
    "    return \"AvatarPay { avatarPayPrimKey = \\\"\" + primKey + \"\\\", avatarPayAmount = \" + (string) amount +\"}\";",
    "}",
    "string mkControl(integer newControlBits) { ",
    "    return \"AvatarControl { avatarNewControlBits = \" + (string) newControlBits + \"}\";",
    "}"]
    
avEventGen = ("$avEventGen", avEventGenAST)