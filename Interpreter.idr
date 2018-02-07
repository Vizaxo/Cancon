module Interpreter

import Expr
import Eval
import TypeCheck
import Types

export
checkAndEval : Stack -> Expr -> Either String Stack
checkAndEval s e = if checkType (Compose (stackToExpr s) e)
                     then case eval s e of
                               Just s => Right s
                               Nothing => Left "Runtime error."
                     else Left "Type checking failed."

compose : List Expr -> Expr
compose [] = id
compose (x :: xs) = (Compose x (compose xs))

export
run : List Expr -> Either String Stack
run xs = checkAndEval Empty (compose xs)
