module Lib where

import Text.Parsec.String
import Text.Parsec
import Data.Functor
import Control.Applicative

parseHaQF :: String -> IO [AST]
parseHaQF file = do
  text <- readFile file
  case runParser parseHaQ () file text of
    Left e -> error $ show e
    Right r -> pure r

parseHaQ :: Parser [AST]
parseHaQ = do
  block <- parseASTs
  eof
  pure block

parseASTs :: Parser [AST]
parseASTs = do 
  sepEndBy parseAST whitespace

parseAST :: Parser AST
parseAST = do
  choice [try block, try block1, stringlit, try glyphedword, try biglyph, try glyph, try number, try word]
 where 
       number = Number . read <$> many1 digit
       word        = do 
        g <- many1 alphabet
        void $ lookAhead $ noneOf "}])"
        pure $ Word g
       glyph       = do 
        g <- glyph_
        void $ lookAhead $ noneOf "}])"
        pure $ Glyph g
       alphabet    = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
       glyph_      = oneOf "':/+.->"
       biglyph     = liftA2 BiGlyph glyph_ glyph_
       glyphedword = do
        g <- glyph_
        word_ <- many1 alphabet
        pure $ GlyphedWord g word_
       stringlit = do
        lit <- between (char '"') (char '"') (many1 alphaNum)
        pure $ StringLit lit
       block1 = do
        bracket <- lookAhead $ try $ oneOf "([{"
        b <- between (sequence [string [bracket], whitespace]) (sequence [string [reverseBracket bracket]]) parseASTs
        pure $ Block_ (bracketType bracket) b
       block = do
        (bracket, ch) <- lookAhead $ do
          c1 <- try $ oneOf "([{"
          c2 <- anyChar
          pure (c1, c2)
        b <- between (sequence [string [bracket, ch], whitespace]) (sequence [string [ch, reverseBracket bracket]]) parseASTs
        pure $ Block (bracketType bracket) (GlyphC ch) b

reverseBracket :: Char -> Char
reverseBracket '(' = ')'
reverseBracket '[' = ']'
reverseBracket '{' = '}'
reverseBracket _   = error "Unknown bracket"
bracketType :: Char -> BracketType
bracketType '(' = Round
bracketType '[' = Square
bracketType '{' = Squigly
bracketType _   = error "Unknown bracket"

whitespace :: Parser String
whitespace = many1 $ oneOf " \n\t"

data BracketType = Round | Square | Squigly deriving Show
newtype GlyphC = GlyphC Char deriving Show

data AST = Number Int 
         | Glyph Char
         | BiGlyph Char Char
         | Word String
         | GlyphedWord Char String
         | StringLit String
         | Block BracketType GlyphC [AST]
         | Block_ BracketType [AST]
         deriving Show