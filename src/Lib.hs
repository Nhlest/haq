module Lib where

import Text.Parsec.String
import Text.Parsec
import Data.Functor

parseHaQF :: String -> IO [AST]
parseHaQF file = do
  text <- readFile file
  case runParser parseHaQ () file text of
    Left _ -> error ""
    Right r -> pure r

parseHaQ :: Parser [AST]
parseHaQ = do
  sepBy (choice [plus, number, period]) (many1 $ char ' ') 
 where number = Number . read <$> many1 digit
       plus   = char '+' $> Plus
       period = char '.' $> Period

eval :: [Int] -> [AST] -> ([Int], String)
eval s [] = (s, mempty)
eval s (x:xs) = (next_stack, mconcat [out, next_out])
  where (new_stack, out) = case x of 
           Number a -> (a:s, "")
           Plus -> (a+b:rest, "")
            where (a,b,rest) = pop2 s
           Period -> (rest, mconcat [show a, "\n"])
            where (a,rest) = pop s
        (next_stack, next_out) = eval new_stack xs

data AST = Number Int | Plus | Period deriving Show

pop :: [Int] -> (Int, [Int])
pop []     = (0, [])
pop (x:xs) = (x, xs)

pop2 :: [Int] -> (Int, Int, [Int])
pop2 []       = (0, 0, [])
pop2 [x]      = (x, 0, [])
pop2 (x:y:xs) = (x, y, xs)