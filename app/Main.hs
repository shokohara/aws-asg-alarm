module Main where

import Lambda
import Slack
import Data.ByteString.Lazy.Char8 hiding (putStrLn)
import Data.Aeson
import System.IO

url = "https://hooks.slack.com/services/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"
main :: IO ()
main = do
  ln <- getLine
  x <- sendMessage url ln
  putStrLn $ "Got: " ++ ln

