module Lsl.EventsParser where

import Debug.Trace
import Lsl.World1(WorldEventType(..))
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( javaStyle )
import Text.ParserCombinators.Parsec.Expr
import Control.Monad
import Control.Monad.Error

-- define basic rules for lexical analysis
lslStyle = javaStyle
             { P.reservedOpNames= ["-", ":"],
               P.reservedNames = ["create_prim",
                                  "add_script",
                                  "reset_script",
                                  "reset_scripts",
                                  "touch",
                                  "chat", "true","false"],
               P.caseSensitive = True }
lexer :: P.TokenParser ()
lexer  = P.makeTokenParser lslStyle

identLetter = P.identLetter lslStyle
               
identifier       = P.identifier lexer
reserved         = P.reserved lexer
operator         = P.operator lexer
reservedOp       = P.reservedOp lexer
                        
charLiteral      = P.charLiteral lexer
stringLiteral    = P.stringLiteral lexer
natural          = P.natural lexer
integer          = P.integer lexer
float            = P.float lexer
naturalOrFloat   = P.naturalOrFloat lexer
decimal          = P.decimal lexer
hexadecimal      = P.hexadecimal lexer
octal            = P.octal lexer

symbol           = P.symbol lexer
lexeme           = P.lexeme lexer
whiteSpace       = P.whiteSpace lexer
             
parens           = P.parens lexer 
braces           = P.braces lexer
angles           = P.angles lexer
brackets         = P.brackets lexer
semi             = P.semi lexer
comma            = P.comma lexer
colon            = P.colon lexer
dot              = P.dot lexer
semiSep          = P.semiSep lexer
semiSep1         = P.semiSep1 lexer
commaSep         = P.commaSep lexer
commaSep1        = P.commaSep1 lexer

boolLiteral = choice [reserved "true" >> return True,
                      reserved "false" >> return False]
                      
createPrim = do reserved "create_prim"
                name <- stringLiteral
                key <- stringLiteral
                return (CreatePrim name key)
addScript = do reserved "add_script"
               inventoryName <- stringLiteral
               scriptName <- stringLiteral
               primKey <- stringLiteral
               activate <- boolLiteral
               return (AddScript (inventoryName,scriptName) primKey activate)
resetScript = do reserved "reset_script"
                 primKey <- stringLiteral
                 invName <- stringLiteral
                 return (ResetScript primKey invName)
resetScripts = do reserved "reset_scripts"
                  key <- stringLiteral
                  return (ResetScripts key)
touch = do reserved "touch"
           agent <- stringLiteral
           prim <- stringLiteral
           duration <- natural
           return (Touch agent prim (fromInteger duration))
chat = do reserved "chat"
          neg <- option (-1) $ (reservedOp "-" >> return 1)
          chatChannel <- natural
          chatterName <- stringLiteral
          chatterKey <- stringLiteral
          message <- stringLiteral
          return (Chat (fromInteger $ neg * chatChannel) chatterName chatterKey message)

entry = do delay <- natural
           reservedOp ":"
           event <- choice [createPrim,addScript,resetScript,resetScripts,touch,chat]
           return (delay,event)
elist =  do whiteSpace
            list <- sepEndBy entry semi
            eof
            return list

parseFile p file =
    let parser = parse p "" in
        do s <- readFile file
           case parser s of
               Left err -> do putStr "parse error at: "
                              print err
                              fail "parse failed"
               Right x -> return x

parseFile' p file =
    do s <- liftIO $ readFile file
       case parser s of
           Left err -> return $ Left (show err)
           Right x -> return  $ Right x
    where parser = parse p ""
   