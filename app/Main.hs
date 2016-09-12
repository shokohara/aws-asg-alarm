{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Lens hiding (Context)
import Data.ByteString.Lazy.Char8
import Data.Aeson
import Data.Aeson.Lens

data Record = Record { eventVersion :: String } deriving Show
data Event = Event { records :: [Record] } deriving Show
data Context = Context { callbackWaitsForEmptyEventLoop :: Bool } deriving Show
data Root = Root { event :: Event, context :: Context } deriving Show

instance FromJSON Record where
  parseJSON (Object v) = Record <$> v .: "EventVersion"
  parseJSON _ = mempty

instance FromJSON Event where
  parseJSON (Object v) = Event <$> v .: "Records"
  parseJSON _ = mempty

instance FromJSON Context where
  parseJSON (Object v) = Context <$> v .: "callbackWaitsForEmptyEventLoop"
  parseJSON _ = mempty

instance FromJSON Root where
  parseJSON (Object v) = Root <$> v .: "event" <*> v .: "context"
  parseJSON _ = mempty

test2 x = decode x :: Maybe Event
test = print . test2 =<< Data.ByteString.Lazy.Char8.readFile "event.json"
test4 x = decode x :: Maybe Root
test3 = print . test4 =<< Data.ByteString.Lazy.Char8.readFile ".event.json"

main :: IO ()
main = test3
