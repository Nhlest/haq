module Lib where

import Text.Parsec.String
import Text.Parsec (runParser)

parseHaQF :: String -> IO ()
parseHaQF file = do
  text <- readFile file
  case runParser parseHaQ () file text of
    Left _ -> pure ()
    Right r -> pure r

parseHaQ :: Parser ()
parseHaQ = do
  pure ()