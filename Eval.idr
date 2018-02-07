module Eval

import Expr
import Types

public export
data Stack : Type where
  Empty : Stack
  Push : (x : Expr) -> Stack -> Stack

export
stackToExpr : Stack -> Expr
stackToExpr Empty = Function StackBottom
stackToExpr (Push x s) = Compose (stackToExpr s) x

mutual
  evalFun : Stack -> Prim t -> Maybe Stack
  evalFun (Push x s) Drop = Just s
  evalFun (Push x s) Dup = Just $ Push x $ Push x s
  evalFun (Push x (Push y z)) Swap = Just $ Push y (Push x z)
  evalFun (Push y s) Apply = eval s y
  evalFun (Push x (Push y z)) PrimCompose = Just $ Push (Compose y x) z
  evalFun (Push x s) PrimQuote = Just $ Push (Quote x) s
  evalFun s (Literal x _) = Just $ Push x s
  evalFun _ StackBottom = Nothing
  evalFun _ _ = Nothing

  export
  eval : Stack -> Expr -> Maybe Stack
  eval s (Compose a b) = do s' <- eval s a
                            eval s' b
  eval s (Quote a) = Just $ Push a s
  eval s (Function t) = evalFun s t
