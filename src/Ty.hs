module Ty where

data Ty = Func Ty Ty
        | Var Int
        | Product Ty Ty
        | StackBottomTy
        deriving (Show)
