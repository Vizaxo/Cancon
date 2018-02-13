module TypeCheck where

import Expr
import Ty

incVars :: Int -> Ty -> Ty
incVars n (Func a b) = Func (incVars n a) (incVars n b)
incVars n (Var s) = Var (s + n)
incVars n (Product a b) = Product (incVars n a) (incVars n b)
incVars n StackBottomTy = StackBottomTy

type Substitutions = [(Int, Ty)]

unify :: Ty -> Ty -> Maybe Substitutions
unify t (Var s) = Just [(s, t)]
unify (Var s) t = Just [(s, t)]
unify (Func a b) (Func x y) = do ax <- unify a x
                                 by <- unify b y
                                 pure (ax ++ by)
unify (Product a b) (Product x y) = do ax <- unify a x
                                       by <- unify b y
                                       pure (ax ++ by)
unify (Product a b) (Func x y) = Nothing
unify (Func a b) (Product x y) = Nothing
unify StackBottomTy _ = Nothing
unify _ StackBottomTy = Nothing

substitute :: Substitutions -> Ty -> Ty
substitute [] y = y
substitute (x : xs) y = substitute xs (sub x y)
  where sub :: (Int, Ty) -> Ty -> Ty
        sub s (Func a b) = Func (sub s a) (sub s b)
        sub (id, replacement) (Var s) = if id == s then replacement else (Var s)
        sub s (Product a b) = Product (sub s a) (sub s b)
        sub _ StackBottomTy = StackBottomTy

maxVar :: Ty -> Int
maxVar x = maxVar' x 0
  where maxVar' :: Ty -> Int -> Int
        maxVar' (Func a b) acc = max (maxVar' a acc) (maxVar' b acc)
        maxVar' (Var s) acc = max s acc
        maxVar' (Product a b) acc = max (maxVar' a acc) (maxVar' b acc)
        maxVar' StackBottomTy _ = 0

uniqueVars :: Ty -> Ty -> Ty
uniqueVars base focus = incVars (maxVar base + 1) focus

inferType :: Expr -> Maybe Ty
inferType (Compose a b) = case inferType a of
                            Just (Func s r) -> do (Func a b) <- inferType b
                                                  let (Func r' t) = uniqueVars (Func s r) (Func a b)
                                                  substs <- unify r r'
                                                  Just $ Func (substitute substs s) (substitute substs t)
                            Just StackBottomTy -> do (Func r t) <- inferType b
                                                     substs <- unify StackBottomTy r
                                                     Just $ Func StackBottomTy (substitute substs t)
                            Just (Var n) -> Nothing
                            Just (Product a b) -> Nothing
inferType (Quote a) = do aTy <- inferType a
                         pure $ Func (Var 0) (Product (Var 0) (incVars 1 aTy))
inferType (Function e) = inferType e
inferType (Primitive e t) = Just t

checkType :: Expr -> Bool
checkType e = case inferType e of
                   Nothing -> False
                   Just x -> True
