module ToyLisp.Evaluator (eval) where

import           ToyLisp.Runtime (LispObject, RuntimeError)
import           ToyLisp.Syntax  (Ast)

eval :: Ast -> Either RuntimeError LispObject
eval = error "Not implemented"
