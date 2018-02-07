module TypeCheck

import Expr
import Types

%default total

incVars : (n : Nat) -> Ty -> Ty
incVars n (Func a b) = Func (incVars n a) (incVars n b)
incVars n (Var s) = Var (s + n)
incVars n (Product a b) = Product (incVars n a) (incVars n b)
incVars n StackBottomTy = StackBottomTy

Substitutions : Type
Substitutions = List (Nat, Ty)

unify : (s : Ty) -> (t : Ty) -> Maybe Substitutions
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

substitute : Substitutions -> Ty -> Ty
substitute [] y = y
substitute (x :: xs) y = substitute xs (sub x y)
  where sub : (Nat, Ty) -> Ty -> Ty
        sub s (Func a b) = Func (sub s a) (sub s b)
        sub (id, replacement) (Var s) = if id == s then replacement else (Var s)
        sub s (Product a b) = Product (sub s a) (sub s b)
        sub _ StackBottomTy = StackBottomTy

maxVar : Ty -> Nat
maxVar x = maxVar' x 0
  where maxVar' : Ty -> (acc : Nat) -> Nat
        maxVar' (Func a b) acc = max (maxVar' a acc) (maxVar' b acc)
        maxVar' (Var s) acc = max s acc
        maxVar' (Product a b) acc = max (maxVar' a acc) (maxVar' b acc)
        maxVar' StackBottomTy _ = 0

uniqueVars : (base : Ty) -> (focus : Ty) -> Ty
uniqueVars base focus = incVars (maxVar base + 1) focus

partial
export
inferType : Expr -> Maybe Ty
inferType (Compose a b) = case inferType a of
                            Just (Func s r) => do (Func a b) <- inferType b
                                                    | Nothing
                                                  let (Func r' t) = uniqueVars (Func s r) (Func a b)
                                                  substs <- unify r r'
                                                  Just $ Func (substitute substs s) (substitute substs t)
                            Just StackBottomTy => do (Func r t) <- inferType b
                                                       | Nothing
                                                     substs <- unify StackBottomTy r
                                                     Just $ Func StackBottomTy (substitute substs t)
                            Just (Var n) => Nothing
                            Just (Product a b) => Nothing
inferType (Quote a) = do A <- inferType a
                         pure $ Func (Var Z) (Product (Var Z) (incVars 1 A))
inferType (Function x {t}) = Just $ t

partial
export
checkType : Expr -> Bool
checkType e = case inferType e of
                   Nothing => False
                   Just x => True
