module Interpreter

import Expr
import Eval
import TypeCheck
import Types
import Primitives

export
checkAndEval : Stack -> Expr -> Either String Stack
checkAndEval s e = if checkType (Compose (stackToExpr s) e)
                     then case eval s e of
                               Just s => Right s
                               Nothing => Left "Runtime error."
                     else Left "Type checking failed."

export
composeProgram : List Expr -> Expr
composeProgram [] = Func id
composeProgram (x :: xs) = (Compose x (composeProgram xs))

export
run : List Expr -> Either String Stack
run xs = checkAndEval Empty (composeProgram xs)
