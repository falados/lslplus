module Lsl.TestResult(TestResult(..),resultToXML, emitTestResult) where

import Lsl.XmlCreate hiding (emit)
import qualified Lsl.XmlCreate as X

emit tag = X.emit tag []

data TestResult = ErrorResult String String [(Int,String)] |
                  FailureResult String String [(Int,String)] |
                  Timeout String [(Int,String)] |
                  SuccessResult String [(Int,String)]
    deriving Show
    
okResult = 0
failureResult = 1
errorResult = 2

resultToXML = flip emitTestResult ""

emitTestResult (ErrorResult name msg log) = emitResult' name errorResult msg log
emitTestResult (FailureResult name msg log) = emitResult' name failureResult msg log
emitTestResult (Timeout name log) = emitResult' name failureResult "timeout" log
emitTestResult (SuccessResult name log) = emitResult' name okResult "ok" log

emitResult' name code msg log =
    emit "test-result" [emit "name" [showString name], emitResultInfo code msg, emitLog log]

emitResultInfo code msg =
    emit "resultInfo" [emit "resultCode" [showString (show code)], emit "resultMessage" [showString msg]]

emitLog log =
    emit "messages" $ map emitMessage (reverse log)

emitMessage (time,text) =
    emit "message" [emit "time" [showString $ show time], emit "text" [showString (escapeString text)]]


escapeString =
    let escape [] = []
        escape ('\n':cs) = '\\':'n':(escape cs)
        escape (c:cs) = c:(escape cs)
    in escape . xmlEscape