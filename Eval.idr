module Eval

import Expr
import Types
import Data.SortedMap

export
eval : Stack -> Expr -> Maybe Stack
eval s (Compose a b) = do s' <- eval s a
                          eval s' b
eval s (Quote a) = Just $ Push a s
eval s (Func (Primitive f t)) = f s
eval s (Func (Custom e)) = eval s e
