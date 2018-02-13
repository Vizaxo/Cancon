module Eval where

import Expr
import Ty

eval :: Stack -> Expr -> Maybe Stack
eval s (Compose a b) = do s' <- eval s a
                          eval s' b
eval s (Quote a) = Just (a:s)
eval s (Primitive f t) = f s
eval s (Function e) = eval s e
