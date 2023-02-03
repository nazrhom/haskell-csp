module Types (
    Constraint(..)
  , Expression(..)
  , Operation(..)
  , Relation(..)
  , Variable
  ) where

import Data.Text (Text, pack)

import Data.String (IsString, fromString)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Constraint = Constraint Expression Relation Expression
    deriving Show

data Expression = Var Variable | Const Int | Expr Expression Operation Expression
    deriving Show

data Operation = Times | Plus | Minus
    deriving Show

data Relation = Equal | Greater | Smaller
    deriving Show

type Variable = Text

type Parser = Parsec Void String

instance IsString Constraint where
    fromString s = case runParser parseConstraint "" s  of
        Left e -> error $ show e
        Right c -> c

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (L.space space1 empty empty)

parseConstraint :: Parser Constraint
parseConstraint = Constraint <$> lexeme parseExpression <*> lexeme parseRelation <*> lexeme parseExpression

parseExpression :: Parser Expression
parseExpression = try parseComposite <|> parseLit

parseLit :: Parser Expression
parseLit = parseConst <|> parseVariable

parseVariable :: Parser Expression
parseVariable = Var . pack <$> many letterChar

parseConst :: Parser Expression
parseConst = Const <$> L.decimal

parseOperation :: Parser Operation
parseOperation = choice 
    [ Times <$ char '*'
    , Plus <$ char '+'
    , Minus <$ char '-'
    ]

parseComposite :: Parser Expression
parseComposite = Expr <$> lexeme (parseLit <|> parseExpression) <*> lexeme parseOperation <*> lexeme parseExpression

parseRelation :: Parser Relation
parseRelation = choice [ Equal <$ char '=' ]

