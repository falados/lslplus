module Lsl.UnitTester where

import Control.Monad
import IO
import Lsl.Compiler
import Lsl.DOMProcessing
import Lsl.DOMSourceDescriptor
import Lsl.DOMUnitTestDescriptor
import Lsl.Structure
import Lsl.UnitTest
import Lsl.UnitTestWorld
import Text.XML.HaXml hiding (when)
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Pretty

import Debug.Trace

trace1 x = trace (show x) x

testExecutionElement :: Monad m => ElemAcceptor m (([(String,String)],[(String,String)]),[LSLUnitTest])
testExecutionElement =
    let f (Elem _ _ contents) =
            do (sources,contents1) <- findElement sourceFilesElement [ e | e@(CElem _ _) <- contents]
               (tests,[]) <- findElement testsElement contents1
               return (sources,tests)
    in ElemAcceptor "test-descriptor" f
               
readTestExecutionDescription :: Handle -> IO (([(String,String)],[(String,String)]),[LSLUnitTest])
readTestExecutionDescription handle = do
    input <- hGetContents handle
    let doc = xmlParse "" input
    parseTestExecutionDescription doc

parseTestExecutionDescription (Document _ _ root _) = match testExecutionElement root

main :: IO ()
main = do
    (src,unitTests) <- readTestExecutionDescription stdin
    (augLib,scripts) <- compile src
    case runUnitTests (libFromAugLib augLib) scripts unitTests of
       Left s -> fail s -- error s
       Right results -> putStr $ renderTestResults results ""

main1 :: IO ()
main1 = do
    (src,unitTests) <- readTestExecutionDescription stdin
    (augLib,scripts) <- compile src
    runUnitTests' (libFromAugLib augLib) scripts unitTests
