{-# LANGUAGE OverloadedRecordDot #-}

module ToyLisp where

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
runReplWith _config = error "REPL mode not implemented"
