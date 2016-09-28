{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Lambda where

import Data.Char
import Data.Aeson
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
  callbackWaitsForEmptyEventLoop :: Bool ,logGroupName ,logStreamName ,functionName ,memoryLimitInMB ,functionVersion ,invokeid ,awsRequestId ,invokedFunctionArn :: String
  } deriving (Show, Generic)

data Root = Root { event :: Event, context :: Context } deriving (Show, Generic)

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

f1 :: [Char] -> [Char]
f1 x = (map toUpper $ take 1 x) ++ drop 1 x

f2 :: [Char] -> [Char]
f2 x = if (take 1 . reverse . f1 $ x) == "'" then reverse . drop 1 . reverse $ f1 x else f1 x

