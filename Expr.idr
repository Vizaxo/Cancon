module Expr

import Types

%access public export

mutual
  data Prim : Ty -> Type where
    Drop : Prim (Func (Product (Var 0) (Var 1)) (Var 0))
    Dup : Prim (Func (Product (Var 0) (Var 1)) (Product (Product (Var 0) (Var 1)) (Var 1)))
    Apply : Prim (Func (Product (Var 0) (Func (Var 0) (Var 1))) (Var 1))
    Literal : (x : Expr) -> (a : Ty) -> Prim (Func (Var 0) (Product (Var 0) a))

  data Expr : Type where
    Compose : (a : Expr) -> (b : Expr) -> Expr
    Quote : (a : Expr) -> Expr
    Function : Prim t -> Expr

id : Expr
id = (Compose (Function (Literal (Function Drop) (Func (Product (Var 0) (Var 1)) (Var 0)))) (Function Drop))
