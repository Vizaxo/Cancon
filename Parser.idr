module Parser

import Expr
import Types
import Primitives
import Lightyear
import Lightyear.Char
import Lightyear.Strings
import Data.SortedMap

mutual
  public export
  data Quotation = Quote Expression

  public export
  Expression : Type
  Expression = List (Either Identifier Quotation)

public export
Program : Type
Program = List(Either Identifier Quotation)

identifier : Parser Identifier
identifier = map pack (some letter)

mutual
  quotation : Parser Quotation
  quotation = Quote <$> between (char '[') (many space *> char ']') (commitTo composition)

  --TODO: nested quotations
  composition : Parser Expression
  composition = many (many space *> (Left <$> identifier) <|>| (Right <$> quotation) <* many space)

export
program : Parser Program
program = composition

