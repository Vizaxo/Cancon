module Primitives

import Expr
import Types
import Eval
import Data.SortedMap

dropEval : Stack -> Maybe Stack
dropEval Empty = Nothing
dropEval (Push x s) = Just s

export
drop : Function
drop = Primitive dropEval (Func (Product (Var 0) (Var 1)) (Var 0))

dupEval : Stack -> Maybe Stack
dupEval Empty = Nothing
dupEval (Push x s) = Just (Push x (Push x s))

export
dup : Function
dup = Primitive dupEval (Func (Product (Var 0) (Var 1)) (Product (Product (Var 0) (Var 1)) (Var 1)))

swapEval : Stack -> Maybe Stack
swapEval Empty = Nothing
swapEval (Push x Empty) = Nothing
swapEval (Push x (Push y s)) = Just (Push y (Push x s))

export
swap : Function
swap = Primitive swapEval (Func (Product (Product (Var 0) (Var 1)) (Var 2)) (Product (Product (Var 0) (Var 2)) (Var 1)))

applyEval : Stack -> Maybe Stack
applyEval Empty = Nothing
applyEval (Push x s) = eval s x

export
apply : Function
apply = Primitive applyEval (Func (Product (Var 0) (Func (Var 0) (Var 1))) (Var 1))

quoteEval : Stack -> Maybe Stack
quoteEval Empty = Nothing
quoteEval (Push x s) = Just (Push (Quote x) s)

export
primQuote : Function
primQuote = Primitive quoteEval (Func (Product (Var 0) (Var 1)) (Product (Var 0) (Func (Var 2) (Product (Var 2) (Var 1)))))

composeEval : Stack -> Maybe Stack
composeEval Empty = Nothing
composeEval (Push x Empty) = Nothing
composeEval (Push x (Push y s)) = Just (Push (Compose y x) s)

export
primCompose : Function
primCompose = Primitive composeEval (Func (Product (Product (Var 0) (Func (Var 1) (Var 2))) (Func (Var 2) (Var 3))) (Product (Var 0) (Func (Var 1) (Var 3))))

export
stackBottom : Function
stackBottom = Primitive (\_ => Nothing) StackBottomTy

export
id : Function
id = Custom (Compose (Quote (Func drop)) (Func drop))

literal : Function
--literal : (x = Primitive) -> (a : Ty) -> Prim (Func (Var 0) (Product (Var 0) a))
 
