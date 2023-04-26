module Lex (lex) where

import Types
import Prelude hiding (lex)
import Data.Char (isDigit, isAlpha)

lex :: String -> Either String [Token]
lex s = case s of
             []          -> return []
             (' ':t)     -> lex t
             ('\t':t)    -> lex t
             ('\r':t)    -> lex t
             ('\n':t)    -> lex t
             (':':'t':t) -> lex t >>= (return . (TTOKEN :))
             (':':'a':t) -> lex t >>= (return . (TAST :))
             ('=':'=':t) -> lex t >>= (return . (TEq :))
             ('!':'=':t) -> lex t >>= (return . (TNe :))
             ('>':'=':t) -> lex t >>= (return . (TGeq :))
             ('<':'=':t) -> lex t >>= (return . (TLeq :))
             ('&':'&':t) -> lex t >>= (return . (TAnd :))
             ('|':'|':t) -> lex t >>= (return . (TOr :))
             ('<':t)     -> lex t >>= (return . (TLt :))
             ('>':t)     -> lex t >>= (return . (TGt :))
             ('=':t)     -> lex t >>= (return . (TAssign :))
             ('+':t)     -> lex t >>= (return . (TAdd :))
             ('-':t)     -> lex t >>= (return . (TSub :))
             ('*':t)     -> lex t >>= (return . (TMul :))
             ('/':t)     -> lex t >>= (return . (TDiv :))
             ('(':t)     -> lex t >>= (return . (LParen :))
             (')':t)     -> lex t >>= (return . (RParen :))
             ('\\':t)    -> lex t >>= (return . (TLambda :))
             ('.':t)     -> lex t >>= (return . (TDot :))
             _ -> if starts isAlpha s then var s
                  else if starts isDigit s then num s
                       else Left ("LEX ERROR: unexpected token " ++ s)
 where starts f s = not (null s) && f (head s)

var :: String -> Either String [Token]
var s
 | v == "if"    = lex s' >>= (return . (TIf :))
 | v == "then"  = lex s' >>= (return . (TThen :))
 | v == "else"  = lex s' >>= (return . (TElse :))
 | v == "true"  = lex s' >>= (return . (TBool True :))
 | v == "false" = lex s' >>= (return . (TBool False :))
 | otherwise    = lex s' >>= (return . (TVar v :))
 where (v,s') = span isAlpha s

num :: String -> Either String [Token]
num s = lex s' >>= (return . (TNum (read d) :))
 where (d,s') = span isDigit s

