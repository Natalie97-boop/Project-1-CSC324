{-|
Module: P1StarterTests
Description: Tests for Project 1
Copyright: (c) University of Toronto Mississagua, 2020
               CSC324 Principles of Programming Languages, Fall 2020
-}

module P1StarterTest where

import Test.QuickCheck (Property, (==>), label, quickCheck)
import Data.Map (lookup, empty, Map, insert)
import P1 (computeSpreadsheet, evalDeer)
import P1Types (Spreadsheet(..), Definition(..), Column(..), Expr(..), 
                Value(..), Env, exampleSpreadsheet)


--Some simple tests to get you started--

prop_testBuiltins :: Bool
prop_testBuiltins= 
  let expr = (Builtin "++" [Literal $ VStr "x", Literal $ VStr "y"])
      result = evalDeer expr empty
  in result ==  VStr "xy"

prop_testValCols :: Property
prop_testValCols = label "a spreadsheet with only value columns" $
  let spreadsheet = Spreadsheet [] [ValCol "id" [VNum 1, VNum 2, VNum 3]]
      result = computeSpreadsheet spreadsheet
  in result == [ValCol "id" [VNum 1, VNum 2, VNum 3]]


prop_testBasicIdentifier :: Property
prop_testBasicIdentifier = label "a spreadsheet with a basic identifier" $
  let spreadsheet = Spreadsheet [Def "basic-var" (Literal $ VStr "test")]
                                [ValCol "id" [VNum 1.0],
                                 ComputedCol "output-column" (Id "basic-var")]
      result = computeSpreadsheet spreadsheet
  in result == [ValCol "id" [VNum 1.0],
                ValCol "output-column" [VStr "test"]]


prop_testBuiltinIdentifier :: Property
prop_testBuiltinIdentifier = label "computedCol using a builtin identifier" $
  let spreadsheet = Spreadsheet [] 
                                [ValCol "id" [VNum 1, VNum 2, VNum 3],
                                 ComputedCol "times-10" 
                                   (Builtin "*" 
                                    [Literal $ VNum 10, Id "id"])]
      result = computeSpreadsheet spreadsheet
  in result == [ValCol "id" [VNum 1, VNum 2, VNum 3],
                ValCol "times-10" [VNum 10, VNum 20, VNum 30]]


prop_testBasicError :: Property
prop_testBasicError = label "a spreadsheet with a divide by 0 error" $
  let spreadsheet = Spreadsheet [Def "divide" (Lambda ["a", "b"]
                                                (Builtin "/" 
                                                  [Id "a", Id "b"]))]
                                [ValCol "x" [VNum 1, VNum 2, VNum 3],
                                 ValCol "y" [VNum 1, VNum 0, VNum 0],
                                 ComputedCol "result" (Apply 
                                                        (Id "divide") 
                                                        [Id "x", Id "y"])]
      result = computeSpreadsheet spreadsheet
  in result == [ValCol "x" [VNum 1.0, VNum 2.0, VNum 3.0],
                ValCol "y" [VNum 1.0, VNum 0.0, VNum 0.0],
                ValCol "result" [VNum 1.0, Error, Error]]


prop_fullExampleSpreadsheet :: Property
prop_fullExampleSpreadsheet = label "example spreadsheet in the project handout" $
  let result = computeSpreadsheet exampleSpreadsheet
  in result == [ValCol "id" [VNum 1.0, VNum 2.0, VNum 3.0, VNum 4.0, VNum 5.0],
               ValCol "name" [VStr "adam", VStr "betty", VStr "clare", 
                              VStr "eric", VStr "sam"],
               ValCol "age" [VNum 12.0, VNum 15.0, VNum 18.0, 
                             VNum 49.0, VNum 17.0],
               ValCol "voter" [VBool False, VBool False, VBool True, 
                               VBool True, VBool False],
               ValCol "name2" [VStr "adamadam", VStr "bettybetty", 
                               VStr "clareclare", VStr "ericeric", VStr "samsam"],
               ValCol "year-until-100" [VNum 88.0, VNum 85.0, VNum 82.0, 
                                        VNum 51.0, VNum 83.0]]

-------------------------------------------------------------------------------
-- * Main function (for testing purposes only)
-------------------------------------------------------------------------------

-- This main function is executed when you compile and run this Haskell file.
-- It runs the QuickCheck tests; we'll talk about "do" notation much later in
-- the course, but for now if you want to add your own tests, just define them
-- above, and add a new `quickCheck` line below.
main :: IO ()
main = do
    quickCheck prop_testBuiltins 
    quickCheck prop_testValCols 
    quickCheck prop_testBasicIdentifier
    quickCheck prop_testBuiltinIdentifier 
    quickCheck prop_testBasicError 
    quickCheck prop_fullExampleSpreadsheet 

