{-# LANGUAGE TemplateHaskell #-}
module Lib where

import Text.Parsec.Text
import Text.Parsec
import Data.Functor
import Control.Applicative
import qualified Data.Vector as V
import qualified Control.Monad.State.Lazy as S
import Control.Lens hiding (noneOf)
import Control.Monad
import qualified Data.Text.IO as TXT

parseHaQF :: String -> IO [AST]
parseHaQF file = do
  text <- TXT.readFile file
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
        -- TODO: errors out if it's the last character in input before EOF
        g <- glyph_
        void $ lookAhead $ noneOf "}])"
        pure $ Glyph g
       alphabet    = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
       glyph_      = oneOf "':/+.-<>=_@"
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

data ByteCode = Drop
              | Sink Int
              | Dive Int
              | LEq
              | Push Int
              | Minus
              | Plus
              | CallReq
              | Call Int
              | Print
  deriving Show

data VType = VNumber | VBoolean deriving (Show, Eq)

data StackValue = StackValue { 
  alias :: Maybe String,
  vType :: VType
 } deriving Show

data CompilerStateMachine = CompilerStateMachine {
  _blocks :: V.Vector [ByteCode],
  _current_block :: [ByteCode],
  _local_stack :: [StackValue]
} deriving Show

emptyCSM :: CompilerStateMachine
emptyCSM = CompilerStateMachine V.empty [] []

$(makeLenses ''CompilerStateMachine)
type CompilerStateMachineM a = S.State CompilerStateMachine a

(++=) :: S.MonadState s m => ASetter' s [a] -> a -> m ()
(++=) lens_a value = lens_a <>= [value]

(=:) :: S.MonadState s m => ASetter s s [a] [a] -> a -> m ()
(=:) lens_a value = lens_a %= (value:)

errorCheck :: CompilerStateMachineM (Either a String) -> CompilerStateMachineM a
errorCheck f = do
  result <- f
  case result of
    Left r -> pure r
    Right e -> error e

compileBlock :: [AST] -> CompilerStateMachineM [ByteCode]
compileBlock block = do 
  forM_ block $ errorCheck . compileAst
  old_block <- use current_block
  current_block .= []
  pure old_block

compileAst :: AST -> CompilerStateMachineM (Either () String)
compileAst (Number n) = cNumber n
compileAst (Glyph '<') = cSink
compileAst (Glyph '+') = cPlus
compileAst x = error $ show x ++ " Not Yet Implemented"

cNumber :: Int -> CompilerStateMachineM (Either () String)
cNumber n = do
  current_block ++= Push n
  local_stack =: StackValue Nothing VNumber
  pure $ Left ()

pop :: S.MonadState s m => Over (->) ((,) r) s s [r] [r] -> m r
pop lens_a = lens_a %%= (\x -> (head x, tail x))

checkStackLength :: Int -> CompilerStateMachineM (Either a String) -> CompilerStateMachineM (Either a String)
checkStackLength n f = do
  stack_length <- length <$> use local_stack
  if stack_length < n then
    pure $ Right "Stack underflow"
  else f

cSink :: CompilerStateMachineM (Either () String)
cSink = do
  checkStackLength 2 $ do
    current_block ++= Sink 1
    a <- pop local_stack
    b <- pop local_stack
    local_stack =: a
    local_stack =: b
    pure $ Left ()

cPlus :: CompilerStateMachineM (Either () String)
cPlus = do
  checkStackLength 2 $ do
    a <- pop local_stack
    b <- pop local_stack
    if vType a /= vType b then
      pure $ Right "Type Mismatch"
    else do
      if vType a /= VNumber then
        pure $ Right "Type Not Number"
      else do
        current_block ++= Plus
        local_stack =: StackValue Nothing VNumber
        pure $ Left ()