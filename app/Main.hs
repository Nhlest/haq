module Main where

import Lib

main :: IO ()
main = do
  a <- parseHaQF "hello.haq"
  print a
  pure ()