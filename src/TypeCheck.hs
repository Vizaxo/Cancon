module TypeCheck where

import Expr
import Ty
import Data.Maybe

type Substitutions = [(Int, Ty)]

incVars :: Int -> Ty -> Ty
incVars n (Func a b)    = Func (incVars n a) (incVars n b)
incVars n (Var s)       = Var (s + n)
incVars n (Product a b) = Product (incVars n a) (incVars n b)
incVars n StackBottomTy = StackBottomTy

unify :: Ty -> Ty -> Maybe Substitutions
unify replacement   (Var id)      = Just [(id, replacement)]
unify (Var id)      replacement   = Just [(id, replacement)]
unify (Func a b)    (Func x y)    = do ax <- unify a x
                                       by <- unify (substitute ax b) (substitute ax y)
                                       pure (ax ++ by)
unify (Product a b) (Product x y) = do ax <- unify a x
                                       by <- unify (substitute ax b) (substitute ax y)
                                       pure (ax ++ by)
unify _             _             = Nothing

substitute :: Substitutions -> Ty -> Ty
substitute []     y = y
substitute (x:xs) y = substitute xs (sub x y)
  where sub :: (Int, Ty) -> Ty -> Ty
        sub (id, replacement) (Var s) | id == s   = replacement
                                      | otherwise = (Var s)
        sub s (Func a b)    = Func (sub s a) (sub s b)
        sub s (Product a b) = Product (sub s a) (sub s b)
        sub _ StackBottomTy = StackBottomTy

maxVar :: Ty -> Int
maxVar = maxVar' 0
  where maxVar' :: Int -> Ty -> Int
        maxVar' acc (Var s)       = max s acc
        maxVar' acc (Func a b)    = max (maxVar' acc a) (maxVar' acc b)
        maxVar' acc (Product a b) = max (maxVar' acc a) (maxVar' acc b)
        maxVar' _ StackBottomTy   = 0

uniqueVars :: Ty -> Ty -> Ty
uniqueVars base focus = incVars (maxVar base + 1) focus

inferType :: Expr -> Maybe Ty
inferType (Compose a b) =
  case inferType a of
    Just (Var n)       -> Nothing
    Just (Func s r)    -> do (Func a b) <- inferType b
                             let (Func r' t) = uniqueVars (Func s r) (Func a b)
                             substs <- unify r r'
                             Just $ Func (substitute substs s) (substitute substs t)
    Just (Product a b) -> Nothing
    Just StackBottomTy -> do (Func r t) <- inferType b
                             substs <- unify StackBottomTy r
                             Just $ Func StackBottomTy (substitute substs t)
inferType (Quote a)       = do aTy <- inferType a
                               pure $ Func (Var 0) (Product (Var 0) (incVars 1 aTy))
inferType (Function e)    = inferType e
inferType (Primitive e t) = Just t

checkType :: Expr -> Bool
checkType = isJust . inferType
