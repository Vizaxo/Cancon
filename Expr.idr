module Expr

import Types

%access public export

mutual
  data Prim : Ty -> Type where
    Drop : Prim (Func (Product (Var 0) (Var 1)) (Var 0))
    Dup : Prim (Func (Product (Var 0) (Var 1)) (Product (Product (Var 0) (Var 1)) (Var 1)))
    Swap : Prim (Func (Product (Product (Var 0) (Var 1)) (Var 2)) (Product (Product (Var 0) (Var 2)) (Var 1)))
    Apply : Prim (Func (Product (Var 0) (Func (Var 0) (Var 1))) (Var 1))
    PrimQuote : Prim (Func (Product (Var 0) (Var 1)) (Product (Var 0) (Func (Var 2) (Product (Var 2) (Var 1)))))
    PrimCompose : Prim (Func (Product (Product (Var 0) (Func (Var 1) (Var 2))) (Func (Var 2) (Var 3))) (Product (Var 0) (Func (Var 1) (Var 3))))
    StackBottom : Prim StackBottomTy
    Literal : (x : Expr) -> (a : Ty) -> Prim (Func (Var 0) (Product (Var 0) a))

  data Expr : Type where
    Compose : (a : Expr) -> (b : Expr) -> Expr
    Quote : (a : Expr) -> Expr
    Function : Prim t -> Expr

drop : Expr
drop = Function Drop

dup : Expr
dup = Function Dup

apply : Expr
apply = Function Apply

id : Expr
id = (Compose (Quote drop) drop)

quote : Expr
quote = Function PrimQuote

compose : Expr
compose = Function PrimCompose

swap : Expr
swap = Function Swap
