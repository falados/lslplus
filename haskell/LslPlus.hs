module Main where

import qualified Language.Lsl.Internal.MetaData as MetaData
import qualified Language.Lsl.Internal.Compiler as Compiler
import qualified Language.Lsl.Internal.ExpressionHandler as ExpressionHandler
import qualified Language.Lsl.Internal.SimMetaData as SimMetaData
import qualified Language.Lsl.Internal.SystemTester as SystemTester
import qualified Language.Lsl.Internal.UnitTester as UnitTester

import Control.Monad
import IO
import System
import System.Exit

usage progName = "Usage: " ++ progName ++ " [MetaData|Compiler|ExpressionHandler|SimMetaData|SystemTester|UnitTester]"
main = do
    progName <- getProgName
    args <- getArgs
    when (length args /= 1) $ do
        hPutStrLn stderr "Invalid number of command line arguments"
        hPutStrLn stderr (usage progName)
        exitFailure
    case head args of
        "MetaData" -> MetaData.printMeta
        "Compiler" -> Compiler.main0
        "ExpressionHandler" -> ExpressionHandler.validateExpression stdin stdout
        "SimMetaData" -> SimMetaData.printSimMeta
        "SystemTester" -> SystemTester.testSystem
        "UnitTester" -> UnitTester.main2
        val -> do
            hPutStrLn stderr ("Invalid argument: " ++ val)
            hPutStrLn stderr (usage progName)
            exitFailure
