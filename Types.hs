module Types where

type Name = String
type Env a = Name -> a

extend :: Env a -> Name -> a -> Env a
extend env name val = \x -> if name == x then val else env x


data Token = TAST | TTOKEN | TAssign | TOr | TAnd
           | TEq | TNe | TLt | TLeq | TGt | TGeq
           | TAdd | TSub | TMul | TDiv
           | TIf | TThen | TElse
           | LParen | RParen | TLambda | TDot 
           | TNum Int | TVar Name | TBool Bool
 deriving (Show, Eq)

data Stmt = Asgn Name Exp
          | PrintAST Exp
          | PrintTokens [Token]
          | Exp Exp
 deriving (Show, Eq)

data Exp = Log LogOp Exp Exp
         | Rel RelOp Exp Exp
         | Arith ArithOp Exp Exp
         | Num Int
         | Bool Bool
         | Var Name
         | Lam Name Exp
         | App Exp Exp
         | If Exp Exp Exp
         | WRONG
 deriving (Show,Eq)

data LogOp = AND | OR
 deriving (Show,Eq)

data RelOp = EQ | NE | LT | LEQ | GT | GEQ
 deriving (Show,Eq)

data ArithOp = ADD | SUB | MUL | DIV
 deriving (Show,Eq)
