module Primitives where

import Prelude hiding (drop)
import Expr
import Ty
import Eval

dropEval :: Stack -> Maybe Stack
dropEval [] = Nothing
dropEval (x:s) = Just s

drop :: Expr
drop = Primitive dropEval (Func (Product (Var 0) (Var 1)) (Var 0))

dupEval :: Stack -> Maybe Stack
dupEval [] = Nothing
dupEval (x:s) = Just (x:(x:s))

dup :: Expr
dup = Primitive dupEval (Func (Product (Var 0) (Var 1)) (Product (Product (Var 0) (Var 1)) (Var 1)))

swapEval :: Stack -> Maybe Stack
swapEval [] = Nothing
swapEval (x:[]) = Nothing
swapEval (x:(y:s)) = Just (y:(x:s))

swap :: Expr
swap = Primitive swapEval (Func (Product (Product (Var 0) (Var 1)) (Var 2)) (Product (Product (Var 0) (Var 2)) (Var 1)))

applyEval :: Stack -> Maybe Stack
applyEval [] = Nothing
applyEval (x:s) = eval s x

apply :: Expr
apply = Primitive applyEval (Func (Product (Var 0) (Func (Var 0) (Var 1))) (Var 1))

quoteEval :: Stack -> Maybe Stack
quoteEval [] = Nothing
quoteEval (x:s) = Just ((Quote x):s)

primQuote :: Expr
primQuote = Primitive quoteEval (Func (Product (Var 0) (Var 1)) (Product (Var 0) (Func (Var 2) (Product (Var 2) (Var 1)))))

composeEval :: Stack -> Maybe Stack
composeEval [] = Nothing
composeEval (x:[]) = Nothing
composeEval (x:(y:s)) = Just ((Compose y x):s)

primCompose :: Expr
primCompose = Primitive composeEval (Func (Product (Product (Var 0) (Func (Var 1) (Var 2))) (Func (Var 2) (Var 3))) (Product (Var 0) (Func (Var 1) (Var 3))))

stackBottom :: Expr
stackBottom = Primitive (\_ -> Nothing) StackBottomTy

id' :: Expr
id' = Function (Compose (Quote drop) drop)
