{-# LANGUAGE OverloadedRecordDot #-}

module ToyLisp (RunConfig (..), RunMode(..), runWith, runReplWith) where

import           ToyLisp.Evaluator (EvalIO (..), eval)
import           ToyLisp.Parser    (parse)

data RunConfig = RunConfig
    { runMode     :: RunMode
    , runPreloads :: [String]
    }

data RunMode = ExecuteProgram | ShowAstOnly

runWith :: (EvalIO m) => RunConfig -> String -> m ()
runWith config content = do
    case parse content of
        Left err -> writeErrorLn $ show err
        Right ast -> do
            case config.runMode of
                ExecuteProgram -> do
                    !_ <- eval ast
                    pure ()
                ShowAstOnly -> do
                    writeOutputLn $ show ast

runReplWith :: (EvalIO m) => RunConfig -> m ()
runReplWith config =
    writeOutputLn "ToyLisp REPL"
        >> replLoop
    where
        replLoop = do
            writeOutput "* "
            input <- readSExpr
            case parse input of
                Left err -> writeErrorLn $ show err
                Right ast -> do
                    case config.runMode of
                        ExecuteProgram -> do
                            !_ <- eval ast
                            pure ()
                        ShowAstOnly -> do
                            writeOutputLn $ show ast
            replLoop

data ReadSExprState = ReadSExprState
    { parensDepth    :: Int
    , isInsideString :: Bool
    }

readSExpr :: (EvalIO m) => m String
readSExpr =
    readLoop "" ReadSExprState { parensDepth = 0, isInsideString = False }
  where
    readLoop :: (EvalIO m) => String -> ReadSExprState -> m String
    readLoop currentString state = do
        inputedLine <- (++ "\n") <$> readInputLine
        let nextString = currentString ++ inputedLine
        let nextState = updateState state inputedLine
        if (nextState.parensDepth < 0) || (nextState.parensDepth == 0 && not nextState.isInsideString)
            then return nextString
            else readLoop nextString nextState

    updateState :: ReadSExprState -> String -> ReadSExprState
    updateState state line = foldl updateState' state $ zip (Nothing : map Just line) line

    updateState' :: ReadSExprState -> (Maybe Char, Char) -> ReadSExprState
    updateState' state (prev, cur) = case cur of
        '(' | not state.isInsideString -> state { parensDepth = state.parensDepth + 1 }
        ')' | not state.isInsideString -> state { parensDepth = state.parensDepth - 1 }
        '"' | not state.isInsideString -> state { isInsideString = True }
        '"' | prev /= Just '\\' -> state { isInsideString = False }
        _   -> state
