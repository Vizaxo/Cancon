module Primitives where

import Prelude hiding (drop)
import Expr
import Ty
import Eval

dropEval :: Env -> Stack -> Maybe Stack
dropEval _ [] = Nothing
dropEval _ (x:s) = Just s

drop :: Expr
drop = Primitive dropEval (Func (Product (Var 0) (Var 1)) (Var 0))

dupEval :: Env -> Stack -> Maybe Stack
dupEval _ [] = Nothing
dupEval _ (x:s) = Just (x:x:s)

dup :: Expr
dup = Primitive dupEval (Func (Product (Var 0) (Var 1)) (Product (Product (Var 0) (Var 1)) (Var 1)))

swapEval :: Env -> Stack -> Maybe Stack
swapEval _ [] = Nothing
swapEval _ (x:[]) = Nothing
swapEval _ (x:(y:s)) = Just (y:x:s)

swap :: Expr
swap = Primitive swapEval (Func (Product (Product (Var 0) (Var 1)) (Var 2)) (Product (Product (Var 0) (Var 2)) (Var 1)))

applyEval :: Env -> Stack -> Maybe Stack
applyEval _ [] = Nothing
applyEval gamma (x:s) = eval gamma x s

apply :: Expr
apply = Primitive applyEval (Func (Product (Var 0) (Func (Var 0) (Var 1))) (Var 1))

quoteEval :: Env -> Stack -> Maybe Stack
quoteEval _ [] = Nothing
quoteEval _ (x:s) = Just ((Quote x):s)

primQuote :: Expr
primQuote = Primitive quoteEval (Func (Product (Var 0) (Var 1)) (Product (Var 0) (Func (Var 2) (Product (Var 2) (Var 1)))))

composeEval :: Env -> Stack -> Maybe Stack
composeEval _ [] = Nothing
composeEval _ (x:[]) = Nothing
composeEval _ (x:(y:s)) = Just ((Compose y x):s)

primCompose :: Expr
primCompose = Primitive composeEval (Func (Product (Product (Var 0) (Func (Var 1) (Var 2))) (Func (Var 2) (Var 3))) (Product (Var 0) (Func (Var 1) (Var 3))))

stackBottom :: Expr
stackBottom = Primitive (\_ _-> Nothing) StackBottomTy

id' :: Expr
id' = Compose (Quote drop) drop
