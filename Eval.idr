module Eval

import Expr
import Types

public export
data Stack : Type where
  Empty : Stack
  Push : (x : Expr) -> Stack -> Stack

export
stackToExpr : Stack -> Expr
stackToExpr Empty = id
stackToExpr (Push x Empty) = x
stackToExpr (Push x s) = Compose x (stackToExpr s)

mutual
  evalFun : Stack -> Prim t -> Maybe Stack
  evalFun Empty Drop = Nothing
  evalFun (Push x s) Drop = Just s
  evalFun Empty Dup = Nothing
  evalFun (Push x s) Dup = Just $ Push x $ Push x s
  evalFun s (Literal x _) = Just $ Push x s
  evalFun (Push y s) Apply = eval s y

  export
  eval : Stack -> Expr -> Maybe Stack
  eval s (Compose a b) = do s' <- eval s a
                            eval s' b
  eval s (Quote a) = Just $ Push a s
  eval s (Function t) = evalFun s t
