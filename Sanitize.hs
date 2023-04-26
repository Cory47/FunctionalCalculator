module Sanitize (sanitize) where

import Types
import Control.Monad.State (State(..), modify, get, evalState)

rename :: Env Int -> String -> Int -> String
rename env v r = if env v == 0 then v else v ++ "#" ++ show r ++ "_" ++ show (env v)

sanitize :: Int -> Stmt -> Either String Stmt
sanitize r (Asgn n e)       = Right $ (Asgn n (evalState (san (\_ -> 0) r e) 0))
sanitize r (PrintAST e)     = Right $ (PrintAST (evalState (san (\_ -> 0) r e) 0))
sanitize r (PrintTokens ts) = Right $ (PrintTokens ts)
sanitize r (Exp e)          = Right $ (Exp (evalState (san (\_ -> 0) r e) 0))

san :: Env Int -> Int -> Exp -> State Int Exp
san env r (Log op e1 e2)   = do e1' <- san env r e1
                                e2' <- san env r e2
                                return $ Log op e1' e2'
san env r (Rel op e1 e2)   = do e1' <- san env r e1
                                e2' <- san env r e2
                                return $ Rel op e1' e2'
san env r (Arith op e1 e2) = do e1' <- san env r e1
                                e2' <- san env r e2
                                return $ Arith op e1' e2'
san env r (Num n)          = return $ Num n
san env r (Bool b)         = return $ Bool b
san env r (Var v)          = return $ Var (rename env v r)
san env r (Lam v e)        = do modify (+1)
                                n <- get
                                let env' = extend env v (n+1)
                                e' <- san env' r e
                                return $ Lam (rename env' v r) e'
san env r (App e1 e2)      = do e1' <- san env r e1
                                e2' <- san env r e2
                                return $ App e1' e2'
san env r (If c t f)       = do c' <- san env r c
                                t' <- san env r t
                                f' <- san env r f
                                return $ If c' t' f'
