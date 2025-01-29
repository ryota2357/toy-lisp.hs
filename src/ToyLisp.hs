{-# LANGUAGE OverloadedRecordDot #-}

module ToyLisp where

import           ToyLisp.Evaluator (eval)
import           ToyLisp.Parser    (parse)
import qualified ToyLisp.Runtime   as RT

data RunConfig = RunConfig
    { runMode     :: RunMode
    , runPreloads :: [String]
    }

data RunMode = ExecuteProgram | ShowAstOnly

runWith :: (RT.ExecIO m) => RunConfig -> String -> m ()
runWith config content = do
    case parse content of
        Left err -> RT.writeErrorLn $ show err
        Right ast -> do
            case config.runMode of
                ExecuteProgram -> do
                    !_ <- eval ast
                    pure ()
                ShowAstOnly -> do
                    RT.writeOutputLn $ show ast

runReplWith :: (RT.ExecIO m) => RunConfig -> m ()
runReplWith _config = error "REPL mode not implemented"
