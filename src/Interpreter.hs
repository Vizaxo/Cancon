module Interpreter where

import Prelude hiding (drop)

import Eval
import Expr
import TypeCheck
import Ty
import Primitives
import Parser
import Text.Parsec
import Data.Map (empty, insert)

toExprSingle :: Expression -> Expr
toExprSingle (EQuote (Q x))  = Quote $ toExpr x
toExprSingle (EIdentifier x) = Id x

toExpr :: Program -> Expr
toExpr []     = id'
toExpr [x]     = toExprSingle x
toExpr (x:xs) = Compose (toExprSingle x) (toExpr xs)

defaultEnv :: Env
defaultEnv = insert "dup" dup $
             insert "drop" drop $
             insert "swap" swap $
             insert "apply" apply $
             insert "quote" primQuote $
             insert "compose" primCompose $
             insert "id" id' $
             empty

parseProgram :: String -> Either String Expr
parseProgram s = case parse program "" s of
                   Right x    -> Right $ toExpr x
                   Left error -> Left $ show error

stackToExpr :: Stack -> Expr
stackToExpr []    = stackBottom
stackToExpr (x:s) = Compose (stackToExpr s) x

checkAndEval :: Stack -> Expr -> Either String Stack
checkAndEval s e = do inferType defaultEnv (Compose (stackToExpr s) e)
                      case eval defaultEnv e s of
                        Just s  -> Right s
                        Nothing -> Left "Runtime error."

composeProgram :: [Expr] -> Expr
composeProgram []     = id'
composeProgram (x:xs) = (Compose x (composeProgram xs))

run :: [Expr] -> Either String Stack
run xs = checkAndEval [] (composeProgram xs)

interpret :: String -> Either String Stack
interpret s = parseProgram s >>= checkAndEval []
