module Parser where

import Expr
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Control.Applicative (some)

data Expression = EIdentifier Identifier
                | EQuote Quotation
                deriving (Show)
type Composition = [Expression]
data Quotation = Q Composition
               deriving (Show)
type Program = Composition

identifier :: Parser Identifier
identifier = some letter

quotation :: Parser Quotation
quotation = Q <$> between (char '[') (char ']') composition

composition :: Parser Composition
composition = many (many space *>
                    ((EQuote <$> quotation) <|> (EIdentifier <$> identifier))
                    <* many space)

program :: Parser Program
program = composition

