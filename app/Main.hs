{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lambda
import Control.Lens
import Network.Slack.Webhook
import Data.ByteString.Lazy.Char8 hiding (putStrLn)
import Data.Aeson
import System.IO
import Network.AWS
import Network.AWS.Lambda

functionDescription = do
  e <- newEnv NorthVirginia Discover
  runResourceT . runAWS e . send $ getFunctionConfiguration "haskell-test-youcandeletethisfunction"

main :: IO ()
main = do
  ex <- functionDescription
  print $ ex ^. fcDescription
