module Expr where

import Ty
import Text.Show.Functions
import Data.Map

type Stack = [Expr]

type Identifier = String

type Env = Map Identifier Expr

data Expr = Compose Expr Expr
          | Quote Expr
          deriving (Show)
          | Primitive (Env -> Stack -> Maybe Stack) Ty
          | Id Identifier

