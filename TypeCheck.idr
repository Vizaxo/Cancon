module TypeCheck

import Expr
import Types

%default total

incVars : (n : Nat) -> Ty -> Ty
incVars n (Func a b) = Func (incVars n a) (incVars n b)
incVars n (Var s) = Var (s + n)
incVars n (Product a b) = Product (incVars n a) (incVars n b)

Substitutions : Type
Substitutions = List (Nat, Ty)

unify : (s : Ty) -> (t : Ty) -> Maybe Substitutions
unify (Var s) t = Just [(s, t)]
unify t (Var s) = Just [(s, t)]
unify (Func a b) (Func x y) = do ax <- unify a x
                                 by <- unify b y
                                 pure (ax ++ by)
unify (Product a b) (Product x y) = do ax <- unify a x
                                       by <- unify b y
                                       pure (ax ++ by)
unify (Product a b) (Func x y) = Nothing
unify (Func a b) (Product x y) = Nothing

substitute : Substitutions -> Ty -> Ty
substitute [] y = y
substitute (x :: xs) y = substitute xs (sub x y)
  where sub : (Nat, Ty) -> Ty -> Ty
        sub s (Func a b) = Func (sub s a) (sub s b)
        sub (id, replacement) (Var s) = if id == s then replacement else (Var s)
        sub s (Product a b) = Product (sub s a) (sub s b)

maxVar : Ty -> Nat
maxVar x = maxVar' x 0
  where maxVar' : Ty -> (acc : Nat) -> Nat
        maxVar' (Func a b) acc = max (maxVar' a acc) (maxVar' b acc)
        maxVar' (Var s) acc = max s acc
        maxVar' (Product a b) acc = max (maxVar' a acc) (maxVar' b acc)

uniqueVars : (base : Ty) -> (focus : Ty) -> Ty
uniqueVars base focus = incVars (maxVar base + 1) focus

partial
export
inferType : Expr -> Maybe Ty
inferType (Compose a b) = do (Func s r) <- inferType a
                                   | Nothing
                             (Func a b) <- inferType b
                                   | Nothing
                             let (Func r' t) = uniqueVars (Func s r) (Func a b)
                             substs <- unify r r'
                             Just $ Func (substitute substs s) (substitute substs t)
inferType (Quote a) = do A <- inferType a
                         pure $ Func (Var Z) (incVars 1 A)
inferType (Function x {t}) = Just $ t

partial
export
checkType : Expr -> Bool
checkType e = case inferType e of
                   Nothing => False
                   Just x => True
