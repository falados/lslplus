{-# LANGUAGE TemplateHaskell #-}
module Language.Lsl.QQ(lslm,lsl) where

import Data.Generics.Aliases(extQ)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote(QuasiQuoter(..),dataToPatQ,dataToExpQ)
import Language.Lsl.Syntax(Expr(..))
import Lsl.Parse(parseModuleFromString1,parseScriptFromString1)
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Pos

aqe (AQString v) = Just $ TH.appE (TH.conE 'StringLit) $ TH.varE $ TH.mkName v
aqe (AQInteger v) = Just $ TH.appE (TH.conE 'IntLit) $ TH.varE $ TH.mkName v
aqe (AQKey v) = Just $ TH.appE (TH.conE 'KeyLit) $ TH.varE $ TH.mkName v
aqe (AQFloat v) = Just $ TH.appE (TH.conE 'FloatLit) $ TH.varE $ TH.mkName v
aqe _ = Nothing
aqp (AQString v) = Just $ TH.conP 'StringLit [TH.varP $ TH.mkName v]
aqp (AQInteger v) = Just $ TH.conP 'IntLit [TH.varP $ TH.mkName v]
aqp (AQKey v) = Just $ TH.conP 'KeyLit [TH.varP $ TH.mkName v]
aqp (AQFloat v) = Just $ TH.conP 'FloatLit [TH.varP $ TH.mkName v]
aqp _ = Nothing

lslModulePat :: String -> TH.Q TH.Pat
lslModulePat s = 
    case parseModuleFromString1 s of 
        Left err -> let (pos,msgs) = (errorPos err, errorMessages err) in do
            l <- TH.location
            let (line,col) = TH.loc_start l
            let (line1, col1) = (sourceLine pos, sourceColumn pos)
            fail $ "at " ++ show (line + line1 - 1) ++ ":" ++ show (col1) ++ ": "  ++
                   showErrorMessages "or" "unknown" "expecting" "unexpected" "end of input" msgs
        Right x -> dataToPatQ (const Nothing `extQ` aqp) x
lslModuleExp :: String -> TH.Q TH.Exp
lslModuleExp s = 
    case parseModuleFromString1 s of 
        Left err -> let (pos,msgs) = (errorPos err, errorMessages err) in do
            l <- TH.location
            let (line,col) = TH.loc_start l
            let (line1, col1) = (sourceLine pos, sourceColumn pos)
            fail $ "at " ++ show (line + line1 - 1) ++ ":" ++ show (col1) ++ ": "  ++
                   showErrorMessages "or" "unknown" "expecting" "unexpected" "end of input" msgs
        Right x -> dataToExpQ (const Nothing `extQ` aqe) x

lslm = QuasiQuoter lslModuleExp lslModulePat

lslScriptPat :: String -> TH.Q TH.Pat
lslScriptPat s = 
    case parseScriptFromString1 s of 
        Left err -> let (pos,msgs) = (errorPos err, errorMessages err) in do
            l <- TH.location
            let (line,col) = TH.loc_start l
            let (line1, col1) = (sourceLine pos, sourceColumn pos)
            fail $ "at " ++ show (line + line1 - 1) ++ ":" ++ show (col1) ++ ": "  ++
                   showErrorMessages "or" "unknown" "expecting" "unexpected" "end of input" msgs
        Right x -> dataToPatQ (const Nothing `extQ` aqp) x
lslScriptExp :: String -> TH.Q TH.Exp
lslScriptExp s = 
    case parseScriptFromString1 s of 
        Left err -> let (pos,msgs) = (errorPos err, errorMessages err) in do
            l <- TH.location
            let (line,col) = TH.loc_start l
            let (line1, col1) = (sourceLine pos, sourceColumn pos)
            fail $ "at " ++ show (line + line1 - 1) ++ ":" ++ show (col1) ++ ": "  ++
                   showErrorMessages "or" "unknown" "expecting" "unexpected" "end of input" msgs
        Right x -> dataToExpQ (const Nothing `extQ` aqe) x

lsl = QuasiQuoter lslScriptExp lslScriptPat
