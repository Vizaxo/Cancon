module Expr

import Types

%access public export

Identifier : Type
Identifier = String

mutual
  data Stack : Type where
    Empty : Stack
    Push : (x : Expr) -> (s : Stack) -> Stack

  data Function : Type where
    Primitive : (eval : Stack -> Maybe Stack) -> (type : Ty) -> Function
    Custom : Expr -> Function

  data Expr : Type where
    Compose : (a : Expr) -> (b : Expr) -> Expr
    Quote : (a : Expr) -> Expr
    Func : Function -> Expr
