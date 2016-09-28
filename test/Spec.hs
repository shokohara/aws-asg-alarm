module Spec where

import Test.Hspec
import Main
import Slack
import Lambda
import Data.ByteString.Lazy.Char8
import Data.Aeson
import System.IO

main :: IO ()
main = hspec $ do
  describe "test" $ do
    it "test1" $ test1
    it "test2" $ test2

test2' x = decode x :: Maybe Event

test2 :: IO ()
test2 = print . test2' =<< Data.ByteString.Lazy.Char8.readFile "event.json"

test1' x = decode x :: Maybe Root
test1 :: IO ()

test1 = print . test1' =<< Data.ByteString.Lazy.Char8.readFile ".event.json"

