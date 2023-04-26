
module Parse (parse) where

import Control.Monad (when)
import Types
import Prelude hiding (EQ, LT, GT)

parse :: [Token] -> Either String Stmt
parse ts = do (p,ts') <- stmt ts
              when (not (null ts')) (Left $ "PARSE ERROR: extra tokens\n" ++ show ts)
              return p

stmt :: [Token] -> Either String (Stmt, [Token])
stmt ts = case ts of
               (TAST:ts)   -> do (e,ts') <- expr ts
                                 return (PrintAST e, ts') 
               (TTOKEN:ts) -> return (PrintTokens ts, [])
               (TVar n:TAssign:ts) -> do (e,ts') <- expr ts
                                         return (Asgn n e, ts')
               _ -> do fmap (\(e,t) -> (Exp e, t)) (expr ts)

expr :: [Token] -> Either String (Exp, [Token])
expr ts = case ts of
               (TIf:_) -> do (cond, ts1) <- matchExp TIf ts
                             (texp, ts2) <- matchExp TThen ts1
                             (fexp, ts3) <- matchExp TElse ts2
                             return (If cond texp fexp, ts3)
               _       -> do (f,ts') <- disjunct ts
                             (fs,ts'') <- disjuncts ts'
                             return (foldl (Log OR) f fs, ts'')
 where matchExp tok ts = if length ts >= 1 && tok == head ts
                         then expr (tail ts)
                         else Left ("PARSE ERROR: expected " ++ show tok ++ "\n" ++ show ts)

disjuncts :: [Token] -> Either String ([Exp], [Token])
disjuncts ts = case ts of
                    (TOr:_) -> do (f,ts') <- conjunct (tail ts)
                                  (fs,ts'') <- conjuncts ts'
                                  return (f : fs, ts'')
                    _ -> return ([],ts)

disjunct :: [Token] -> Either String (Exp, [Token])
disjunct ts = do (f,ts') <- conjunct ts
                 (fs,ts'') <- conjuncts ts'
                 return (foldl (Log AND) f fs, ts'')

conjuncts :: [Token] -> Either String ([Exp], [Token])
conjuncts ts = case ts of
                    (TAnd:_) -> do (f,ts') <- conjunct (tail ts)
                                   (fs,ts'') <- conjuncts ts'
                                   return (f : fs, ts'')
                    _ -> return ([],ts)

conjunct :: [Token] -> Either String (Exp, [Token])
conjunct ts = do (f,ts') <- relop ts
                 (fs,ts'') <- relops ts'
                 return (foldl applyOp f fs, ts'')
 where applyOp f1 (op,f2) = op f1 f2

relops :: [Token] -> Either String ([(Exp -> Exp -> Exp, Exp)], [Token])
relops ts = case ts of
                 (TEq:_) -> do (f,ts') <- relop (tail ts)
                               (fs,ts'') <- relops ts'
                               return ((Rel EQ, f) : fs, ts'')
                 (TNe:_) -> do (f,ts') <- relop (tail ts)
                               (fs,ts'') <- relops ts'
                               return ((Rel NE, f) : fs, ts'')
                 (TLt:_) -> do (f,ts') <- relop (tail ts)
                               (fs,ts'') <- relops ts'
                               return ((Rel LT, f) : fs, ts'')
                 (TLeq:_) -> do (f,ts') <- relop (tail ts)
                                (fs,ts'') <- relops ts'
                                return ((Rel LEQ, f) : fs, ts'')
                 (TGt:_) -> do (f,ts') <- relop (tail ts)
                               (fs,ts'') <- relops ts'
                               return ((Rel GT, f) : fs, ts'')
                 (TGeq:_) -> do (f,ts') <- relop (tail ts)
                                (fs,ts'') <- relops ts'
                                return ((Rel GEQ, f) : fs, ts'')
                 _ -> return ([],ts)

relop :: [Token] -> Either String (Exp, [Token])
relop ts = do (f,ts') <- term ts
              (fs,ts'') <- terms ts'
              return (foldl applyOp f fs, ts'')
 where applyOp f1 (op,f2) = op f1 f2

terms :: [Token] -> Either String ([(Exp -> Exp -> Exp, Exp)], [Token])
terms ts = case ts of
                (TAdd:_) -> do (f,ts') <- term (tail ts)
                               (fs,ts'') <- terms ts'
                               return ((Arith ADD,f) : fs, ts'')
                (TSub:_) -> do (f,ts') <- term (tail ts)
                               (fs,ts'') <- terms ts'
                               return ((Arith SUB,f) : fs, ts'')
                _ -> return ([],ts)

term :: [Token] -> Either String (Exp, [Token])
term ts = do (f,ts') <- factor ts
             (fs,ts'') <- factors ts'
             return (foldl applyOp f fs, ts'')
 where applyOp f1 (op,f2) = op f1 f2

factors :: [Token] -> Either String ([(Exp -> Exp -> Exp, Exp)], [Token])
factors ts = case ts of
                  (TMul:_) -> do (f,ts') <- factor (tail ts)
                                 (fs,ts'') <- factors ts'
                                 return ((Arith MUL,f) : fs, ts'')
                  (TDiv:_) -> do (f,ts') <- factor (tail ts)
                                 (fs,ts'') <- factors ts'
                                 return ((Arith DIV,f) : fs, ts'')
                  _ -> return ([],ts)

factor :: [Token] -> Either String (Exp, [Token])
factor ts = do (a,ts') <- atom ts
               (as,ts'') <- atoms ts'
               return (foldl App a as, ts'')

atoms :: [Token] -> Either String ([Exp], [Token])
atoms ts = case ts of
                (LParen:_)  -> run
                (TLambda:_) -> run
                (TNum n:_)  -> run
                (TBool b:_) -> run
                (TVar v:_)  -> run
                _           -> return ([], ts)
 where run = do (a,ts')   <- atom ts
                (as,ts'') <- atoms ts'
                return (a:as, ts'')

atom :: [Token] -> Either String (Exp, [Token])
atom s = case s of
              (LParen:ts)  -> do (e,ts') <- expr ts
                                 rparen e ts'
              (TLambda:ts) -> do (v,ts') <- lambdaStart ts
                                 (body,ts'') <- expr ts'
                                 return (Lam v body, ts'')
              (TNum n:ts)  -> return (Num n, ts)
              (TBool b:ts) -> return (Bool b, ts)
              (TVar v:ts)  -> return (Var v, ts)
              _            -> Left ("PARSE ERROR: not a valid atom\n" ++ show s)
 where rparen e s    = case s of
                         (RParen:s') -> return (e,s')
                         _ -> Left ("PARSE ERROR: unmatched parentheses\n" ++ show s)
       lambdaStart s = case s of
                            (TVar v : TDot : ts') -> return (v, ts')
                            _ -> Left ("PARSE ERROR: badly formed lambda expression\n" ++ show s)

