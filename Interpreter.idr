module Interpreter

import Expr
import Eval
import TypeCheck
import Types
import Primitives
import Parser
import Data.SortedMap
import Lightyear.Strings

toExpr : Env -> Program -> Either String Expr
toExpr gamma [] = Right $ Func id
toExpr gamma (x :: xs) = case lookup x gamma of
                           Just f => toExpr gamma xs >>= Right . Compose (Func f)
                           Nothing => Left $ "Undefined identifier " ++ x

export
defaultEnv : Env
defaultEnv = insert "dup" dup $
             insert "drop" drop $
             insert "swap" swap $
             insert "apply" apply $
             insert "quote" primQuote $
             insert "compose" primCompose $
             insert "id" id $
             insert "quotedId" (Custom $ Quote $ Func id) $
             empty

read : String -> Either String Expr
read s = parse program s >>= toExpr defaultEnv

export
stackToExpr : Stack -> Expr
stackToExpr Empty = Func stackBottom
stackToExpr (Push x s) = Compose (stackToExpr s) x

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

interpret : String -> Either String Stack
interpret s = read s >>= checkAndEval Empty
