module Eval where

import Expr
import Ty

eval :: Expr -> Stack -> Maybe Stack
eval (Compose a b)   s = eval a s >>= eval b
eval (Quote a)       s = Just (a:s)
eval (Primitive f t) s = f s
eval (Function e)    s = eval e s
