module Parser where

import Expr
import Ty
import Primitives
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Control.Applicative (some)

data Quotation = Q Expression
               deriving (Show)

type Expression = [(Either Identifier Quotation)]

type Program = [(Either Identifier Quotation)]

identifier :: Parser Identifier
identifier = some letter

quotation :: Parser Quotation
quotation = Q <$> between (char '[') (char ']') composition

composition :: Parser Expression
composition = many (many space *> (try (Right <$> quotation) <|> (Left <$> identifier)) <* many space)

program :: Parser Program
program = composition

