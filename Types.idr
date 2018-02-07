module Types

public export
data Ty : Type where
  Func : (a : Ty) -> (b : Ty) -> Ty
  Var : (s : Nat) -> Ty
  Product : (a : Ty) -> (b : Ty) -> Ty
  StackBottomTy : Ty
