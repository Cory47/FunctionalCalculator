import Types
import Lex
import Parse
import Sanitize
import Eval
import System.IO (hFlush, stdout)
import Prelude hiding (lex, EQ, LT, GT)

main :: IO ()
main = run 0 (\x -> WRONG)

run :: Int -> Env Exp -> IO ()
run n env = do putStr "\\+> "
               hFlush stdout
               line <- getLine
               case line == ":q" of
                True -> return ()
                False -> case runStep n env line of
                  Left err            -> putStrLn err >> run n env
                  Right (answer,env') -> putStrLn answer >> run (n+1) env'

runStep :: Int -> Env Exp -> String -> Either String (String, Env Exp)
runStep n env line = lex line >>= parse >>= sanitize n >>= exec env

exec :: Env Exp -> Stmt -> Either String (String, Env Exp)
exec env (PrintTokens ts) = Right (show ts, env)
exec env (PrintAST e)     = Right (show e, env)
exec env (Asgn n e)       = let val = eval env e
                            in Right $ (show val, extend env n val)
exec env (Exp e)          = Right $ (show (eval env e), env)

