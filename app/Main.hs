{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Lens hiding (Context)
import Data.Char
import Data.ByteString.Lazy.Char8 hiding (take, drop, map, reverse)
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.TH
import GHC.Generics

data Message = Message {
  alarmName, alarmDescription, aWSAccountId, newStateValue, newStateReason, stateChangeTime, region, oldStateValue :: String
  } deriving (Show, Generic)
data Sns = Sns {
  signatureVersion, timestamp, signature, signingCertUrl, messageId, message, type', unsubscribeUrl, topicArn, subject :: String
  } deriving (Show, Generic)
data Record = Record { eventVersion, eventSubscriptionArn, eventSource :: String, sns :: Sns } deriving (Show, Generic)
data Event = Event { records :: [Record] } deriving (Show, Generic)
data Context = Context {
  callbackWaitsForEmptyEventLoop :: Bool
  ,logGroupName ,logStreamName ,functionName ,memoryLimitInMB ,functionVersion ,invokeid ,awsRequestId ,invokedFunctionArn :: String
  } deriving (Show, Generic)
data Root = Root { event :: Event, context :: Context } deriving (Show, Generic)
f1 :: [Char] -> [Char]
f1 x = (map toUpper $ take 1 x) ++ drop 1 x
f2 :: [Char] -> [Char]
f2 x = if (take 1 . reverse . f1 $ x) == "'" then reverse . drop 1 . reverse $ f1 x else f1 x
instance FromJSON Message where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = f1 }
instance FromJSON Sns where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = f2 }
instance FromJSON Record where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = f1 }
instance FromJSON Event where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = f1 }
instance FromJSON Context
instance FromJSON Root

test2 x = decode x :: Maybe Event
test = print . test2 =<< Data.ByteString.Lazy.Char8.readFile "event.json"
test4 x = decode x :: Maybe Root
test3 = print . test4 =<< Data.ByteString.Lazy.Char8.readFile ".event.json"

main :: IO ()
main = test3
