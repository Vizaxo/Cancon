module Parser

import Expr
import Types
import Primitives
import Lightyear
import Lightyear.Char
import Lightyear.Strings
import Data.SortedMap

public export
Env : Type
Env = SortedMap Identifier Function

public export
Program : Type
Program = List Identifier

identifier : Parser String
identifier = map pack (some letter)

infixr 4 <$
(<$) : Applicative f => a -> f b -> f a
(<$) a b = pure a <* b

emptyProgram : Parser Program
emptyProgram = [] <$ many space

composition : Parser Program
composition = many (many space *> identifier)

export
program : Parser Program
program = composition

