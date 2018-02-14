module Ty where

data Ty = Func Ty Ty
        | Var Int
        | Product Ty Ty
        | StackBottomTy

instance Show Ty where
  show (Func a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
  show (Product a b) = "(" ++ show a ++ " * " ++ show b ++ ")"
  show (Var i) = varToStr i
  show StackBottomTy = "()"

varToStr :: Int -> String
varToStr n | n > 25 = varToStr ((n `div` 26) - 1) ++ varToStr (n `mod` 26)
           | otherwise = [toEnum (n + fromEnum 'a')]

infixr 1 -->
(-->) :: Ty -> Ty -> Ty
(-->) = Func

v :: Char -> Ty
v = Var . (flip (-) (fromEnum 'a')) . fromEnum

infixl 5 -*
(-*) :: Ty -> Ty -> Ty
(-*) = Product
