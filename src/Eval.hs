module Eval where

import Expr
import Ty
import Prelude hiding (lookup)
import Data.Map

eval :: Env -> Expr -> Stack -> Maybe Stack
eval gamma (Compose a b)   s = eval gamma a s >>= eval gamma b
eval _     (Quote a)       s = Just (a:s)
eval gamma (Primitive f _) s = f gamma s
eval gamma (Id x)          s = lookup x gamma >>= flip (eval gamma) s
