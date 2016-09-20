{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Lens hiding (Context)
import Data.ByteString.Lazy.Char8 hiding (drop)
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.TH
import GHC.Generics

data Record = Record { eEventVersion :: String } deriving (Show, Generic)
data Event = Event { rRecords :: [Record] } deriving (Show, Generic)
data Context = Context { callbackWaitsForEmptyEventLoop :: Bool } deriving (Show, Generic)
data Root = Root { event :: Event, context :: Context } deriving (Show, Generic)

-- deriveJSON defaultOptions { fieldLabelModifier = drop 1 } ''Record
-- deriveJSON defaultOptions { fieldLabelModifier = drop 1 } ''Event
instance FromJSON Record where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }
instance FromJSON Event where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }
instance FromJSON Context
instance FromJSON Root

test2 x = decode x :: Maybe Event
test = print . test2 =<< Data.ByteString.Lazy.Char8.readFile "event.json"
test4 x = decode x :: Maybe Root
test3 = print . test4 =<< Data.ByteString.Lazy.Char8.readFile ".event.json"

main :: IO ()
main = test3
