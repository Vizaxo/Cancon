module Expr where

import Ty
import Text.Show.Functions

type Stack = [Expr]

data Expr = Compose Expr Expr
          | Function Expr
          | Quote Expr
          | Primitive (Stack -> Maybe Stack) Ty
          deriving (Show)

