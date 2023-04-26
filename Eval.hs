module Eval where

import Types
import Prelude hiding (EQ, LT, GT)
import GHC.Num (naturalGt, naturalBit)

eval :: Env Exp -> Exp -> Exp
eval env (Bool e1) = Bool e1
eval env (Num n) = Num n
eval env (Lam l x) = Lam l x
eval env (Arith op e1 e2) = arith op (eval env e1) (eval env e2)
eval env (If cond1 tru fal)
    | eval env cond1 == Bool True = eval env tru
    | eval env cond1 == Bool False = eval env fal
    | otherwise = WRONG
eval env (Rel op e1 e2) = relOp op (eval env e1) (eval env e2)
eval env (Log op e1 e2) = logOp op (eval env e1) (eval env e2)
eval env (App e1 e2) = eval env (app (eval env e1) e2)
eval env (Var v) = env v
eval env e = e

arith :: ArithOp -> Exp -> Exp -> Exp
arith ADD (Num n1) (Num n2) = Num (n1 + n2)
arith SUB (Num n1) (Num n2) = Num (n1 - n2)
arith MUL (Num n1) (Num n2) = Num (n1 * n2)
arith DIV (Num n1) (Num n2)
 |n2 == 0 = WRONG
 |otherwise = Num (div n1 n2)
arith _ _ _ = WRONG

logOp :: LogOp -> Exp -> Exp -> Exp
logOp AND (Bool n1) (Bool n2) = Bool (n1 && n2)
logOp OR (Bool n1) (Bool n2) = Bool (n1 || n2)

relOp :: RelOp -> Exp -> Exp -> Exp
relOp EQ (Num n1) (Num n2) = Bool (n1 == n2)
relOp NE (Num n1) (Num n2) = Bool (n1 /= n2)
relOp LT (Num n1) (Num n2) = Bool (n1 < n2)
relOp LEQ (Num n1) (Num n2) = Bool (n1 <= n2)
relOp GT (Num n1) (Num n2) = Bool (n1 > n2)
relOp GEQ (Num n1) (Num n2) = Bool (n1 >= n2)

app :: Exp -> Exp -> Exp
app (Lam v e) e2 = replace v e2 e
app _ _ = WRONG

replace :: Name -> Exp -> Exp -> Exp 
replace x e2 (Num n) = Num n
replace x e2 (Bool b) = Bool b
replace x e2 (Lam y e)
 | x==y         = Lam y e
 | otherwise    = Lam y (replace x e2 e)
replace x e2 (Var v)
 | x==v = e2
 | otherwise = Var v
replace x e2 (Arith op left right) = Arith op (replace x e2 left) (replace x e2 right)
replace x e2 (Rel op left right) = Rel op (replace x e2 left) (replace x e2 right)
replace x e2 (Log op left right) = Log op (replace x e2 left) (replace x e2 right)
replace x e2 (If con tru fal) = If (replace x e2 con)(replace x e2 tru)(replace x e2 fal)
replace x e2 (App left right) = App (replace x e2 left) (replace x e2 right)
replace _ _ _ = WRONG