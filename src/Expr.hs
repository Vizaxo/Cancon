module Expr where

import Ty
import Text.Show.Functions

type Identifier = String

type Stack = [Expr]

data Expr = Compose Expr Expr | Quote Expr | Primitive (Stack -> Maybe Stack) Ty | Function Expr
          deriving (Show)

