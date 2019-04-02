module Main where

import Test.Tasty (defaultMain, TestTree, testGroup)

import HuttonsRazorTest (testHuttonsRazor)

main :: IO ()
main =
  defaultMain tests

tests :: TestTree
tests = testGroup "huttons-razor" [
   testHuttonsRazor
 ]