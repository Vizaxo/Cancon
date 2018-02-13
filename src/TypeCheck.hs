{-# LANGUAGE LambdaCase #-}
module TypeCheck where

import Prelude hiding (lookup)

import Expr
import Ty
import Data.Either
import Data.Composition
import Data.Map

type Substitutions = [(Int, Ty)]

incVars :: Int -> Ty -> Ty
incVars n (Func a b)    = Func (incVars n a) (incVars n b)
incVars n (Var s)       = Var (s + n)
incVars n (Product a b) = Product (incVars n a) (incVars n b)
incVars n StackBottomTy = StackBottomTy

unify :: Ty -> Ty -> Either String Substitutions
unify replacement   (Var id)      = Right [(id, replacement)]
unify (Var id)      replacement   = Right [(id, replacement)]
unify (Func a b)    (Func x y)    = do ax <- unify a x
                                       by <- unify (substitute ax b) (substitute ax y)
                                       pure (ax ++ by)
unify (Product a b) (Product x y) = do ax <- unify a x
                                       by <- unify (substitute ax b) (substitute ax y)
                                       pure (ax ++ by)
unify _             _             = Left "Type error: could not unify types."

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

inferType :: Env -> Expr -> Either String Ty
inferType gamma (Compose a b) = inferType gamma a >>=
  \case
    (Func s r)    -> do (Func a b) <- inferType gamma b
                        let (Func r' t) = uniqueVars (Func s r) (Func a b)
                        substs <- unify r r'
                        Right $ Func (substitute substs s) (substitute substs t)
    StackBottomTy -> do (Func r t) <- inferType gamma b
                        substs <- unify StackBottomTy r
                        Right $ Func StackBottomTy (substitute substs t)
    (Var n)       -> Left "Type error: composing a non-function type."
    (Product a b) -> Left "Type errer: composing a non-function type."
inferType gamma (Quote a)   = do aTy <- inferType gamma a
                                 pure $ Func (Var 0) (Product (Var 0) (incVars 1 aTy))
inferType _ (Primitive _ t) = Right t
inferType gamma (Id x)      = case lookup x gamma of
                                Just a -> inferType gamma a
                                Nothing -> Left $ "Invalid identifier " ++ x ++ "."

checkType :: Env -> Expr -> Bool
checkType = isRight .: inferType
