module Interpreter

import Expr
import Eval
import TypeCheck
import Types

interpret : Stack -> Expr -> Either String Stack
interpret s e = if checkType (Compose (stackToExpr s) e)
                     then case eval s e of
                               Just s => Right s
                               Nothing => Left "Runtime error."
                     else Left "Type checking failed."
