module Interpreter where

import Prelude hiding (drop, lookup)

import Eval
import Expr
import TypeCheck
import Ty
import Primitives
import Parser
import Text.Parsec
import Data.Map hiding (drop)

type Env = Map Identifier Expr

toExpr :: Env -> Program -> Either String Expr
toExpr gamma [(Left x)] = case lookup x gamma of
                            Just f -> Right $ Function f
                            Nothing -> Left $ "Undefined identifier " ++ x
toExpr gamma [(Right (Q x))] = toExpr gamma x >>= Right . Quote
toExpr gamma ((Left x):xs) = case lookup x gamma of
                               Just f -> toExpr gamma xs >>= Right . Compose (Function f)
                               Nothing -> Left $ "Undefined identifier " ++ x
toExpr gamma ((Right (Q x)):xs) = do x' <- toExpr gamma x
                                     toExpr gamma xs >>= Right . Compose (Quote x')

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
                   Right x -> toExpr defaultEnv x
                   Left error -> Left $ show error

stackToExpr :: Stack -> Expr
stackToExpr [] = stackBottom
stackToExpr (x:s) = Compose (stackToExpr s) x

checkAndEval :: Stack -> Expr -> Either String Stack
checkAndEval s e = if checkType (Compose (stackToExpr s) e)
                     then case eval s e of
                               Just s -> Right s
                               Nothing -> Left "Runtime error."
                     else Left "Type checking failed."

composeProgram :: [Expr] -> Expr
composeProgram [] = id'
composeProgram (x:xs) = (Compose x (composeProgram xs))

run :: [Expr] -> Either String Stack
run xs = checkAndEval [] (composeProgram xs)

interpret :: String -> Either String Stack
interpret s = parseProgram s >>= checkAndEval []
