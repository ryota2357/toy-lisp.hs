module Main where

import           ToyLisp.Evaluator (eval)
import           ToyLisp.Parser    (parse)

main :: IO ()
main = do
    case parse "(princ (+ 1 2))" of
        Left err -> print err
        Right ast -> do
            !_ <- eval ast
            putStrLn ""
