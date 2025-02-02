{-# LANGUAGE OverloadedRecordDot #-}

module ToyLisp (RunConfig (..), RunMode(..), runWith, runReplWith) where

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
                    (result, _) <- eval RT.emptyEnvironment ast
                    case result of
                        Left err -> RT.writeErrorLn $ show err
                        Right _  -> pure ()
                ShowAstOnly -> do
                    RT.writeOutputLn $ show ast

runReplWith :: (RT.ExecIO m) => RunConfig -> m ()
runReplWith config = do
    RT.writeOutputLn "ToyLisp REPL"
    case config.runMode of
        ExecuteProgram -> executeProgramLoop
        ShowAstOnly    -> showAstOnlyLoop
  where
    executeProgramLoop = loop RT.emptyEnvironment
      where
        loop env = do
            RT.writeOutput "> "
            input <- readSExpr
            case parse input of
                Left err -> do
                    RT.writeErrorLn $ show err
                    loop env
                Right ast -> do
                    (result, nextEnv) <- eval env ast
                    case result of
                        Left err  -> RT.writeErrorLn $ show err
                        Right obj -> do
                            let display = RT.displayLispObjectWith (const Nothing)
                            RT.writeOutputLn $ "← " ++ display obj
                    loop nextEnv
    showAstOnlyLoop = do
        RT.writeOutput "> "
        input <- readSExpr
        case parse input of
            Left err  -> RT.writeErrorLn $ show err
            Right ast -> RT.writeOutputLn $ show ast
        showAstOnlyLoop

data ReadSExprState = ReadSExprState
    { parensDepth    :: Int
    , isInsideString :: Bool
    }

readSExpr :: (RT.ExecIO m) => m String
readSExpr = readLoop "" ReadSExprState { parensDepth = 0, isInsideString = False }
  where
    readLoop currentString state = do
        inputedLine <- (++ "\n") <$> RT.readInputLine
        let nextString = currentString ++ inputedLine
        let nextState = updateState state inputedLine
        if (nextState.parensDepth < 0) || (nextState.parensDepth == 0 && not nextState.isInsideString)
            then pure nextString
            else readLoop nextString nextState
    updateState state line = foldl updateByChar state $ zip (Nothing : map Just line) line
      where
        updateByChar s (prev, cur) = case cur of
            '(' | not s.isInsideString -> s { parensDepth = s.parensDepth + 1 }
            ')' | not s.isInsideString -> s { parensDepth = s.parensDepth - 1 }
            '"' | not s.isInsideString -> s { isInsideString = True }
            '"' | prev /= Just '\\'    -> s { isInsideString = False }
            _                          -> s
