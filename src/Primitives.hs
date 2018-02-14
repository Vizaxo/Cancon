module Primitives where

import Prelude hiding (drop)
import Expr
import Ty
import Eval

dropEval :: Env -> Stack -> Maybe Stack
dropEval _ [] = Nothing
dropEval _ (x:s) = Just s

drop :: Expr
drop = Primitive dropEval (v 's' -* v 'a' --> v 's')

dupEval :: Env -> Stack -> Maybe Stack
dupEval _ [] = Nothing
dupEval _ (x:s) = Just (x:x:s)

dup :: Expr
dup = Primitive dupEval (v 's' -* v 'a' --> v 's' -* v 'a' -* v 'a')

swapEval :: Env -> Stack -> Maybe Stack
swapEval _ [] = Nothing
swapEval _ (x:[]) = Nothing
swapEval _ (x:(y:s)) = Just (y:x:s)

swap :: Expr
swap = Primitive swapEval (v 's' -* v 'a' -* v 'b' --> v 's' -* v 'b' -* v 'a')

applyEval :: Env -> Stack -> Maybe Stack
applyEval _ [] = Nothing
applyEval gamma (x:s) = eval gamma x s

apply :: Expr
apply = Primitive applyEval (v 's' -* (v 's' --> v 'r') --> v 'r')

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
primCompose = Primitive composeEval (v 's' -* (v 'a' --> v 'b') -* (v 'b' --> v 'c') --> v 's' -* (v 'a' --> v 'c'))

stackBottom :: Expr
stackBottom = Primitive (\_ _-> Nothing) StackBottomTy

id' :: Expr
id' = Compose (Quote drop) drop
