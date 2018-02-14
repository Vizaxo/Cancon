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
stackToExpr (x:s) = Compose (stackToExpr s) (Quote x)

checkAndEval :: Stack -> Expr -> Either String Stack
checkAndEval s e = do inferType defaultEnv (Compose (stackToExpr s) e)
                      case eval defaultEnv e s of
                        Just s  -> Right s
                        Nothing -> Left "Runtime error."

run :: Stack -> String -> Either String Stack
run s str = parseProgram str >>= checkAndEval s

interpret :: String -> Either String Stack
interpret = run []

runRepl :: Stack -> IO ()
runRepl s = do str <- getLine
               case run s str of
                 Right s' -> do putStrLn $ showStack s'
                                runRepl s'
                 Left err -> do putStrLn err
                                runRepl s

repl :: IO ()
repl = runRepl []
