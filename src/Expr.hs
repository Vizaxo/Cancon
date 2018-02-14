module Expr where

import Ty
import Text.Show.Functions
import Data.Map

type Stack = [Expr]

type Identifier = String

type Env = Map Identifier Expr

data Expr = Compose Expr Expr
          | Quote Expr
          | Primitive (Env -> Stack -> Maybe Stack) Ty
          | Id Identifier

showStack :: Stack -> String
showStack xs = "{" ++ output xs ++ "}"
  where output [] = ""
        output [x] = show x
        output (x:xs) = output xs ++ ", " ++ show x

instance Show Expr where
  show (Compose a b) = show a ++ " " ++ show b
  show (Quote a) = "[" ++ show a ++ "]"
  show (Primitive _ _) = "<prim_func>"
  show (Id x) = x
