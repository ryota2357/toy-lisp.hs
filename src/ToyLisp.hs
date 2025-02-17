{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}

module ToyLisp (RunConfig (..), RunMode(..), runWith, runReplWith, PreloadScript (..)) where

import           Control.Monad        (foldM)
import           Control.Monad.Except (runExceptT, throwError)
import           Control.Monad.Trans  (lift)
import           ToyLisp.Analyzer     (validateAst)
import           ToyLisp.Evaluator    (eval)
import           ToyLisp.Parser       (parse)
import qualified ToyLisp.Runtime      as RT
import           ToyLisp.Syntax       (Ast, SyntaxError)

data RunConfig = RunConfig
    { runMode        :: RunMode
    , preloadScripts :: [PreloadScript]
    }

data PreloadScript = PreloadScript
    { scriptName :: String
    , scriptBody :: String
    }

data RunMode = ExecuteProgram | ShowAstOnly

runWith :: (RT.ExecIO m) => RunConfig -> String -> m ()
runWith config content = do
    case parseValidate content of
        Left err -> RT.writeErrorLn $ show err
        Right ast -> do
            case config.runMode of
                ExecuteProgram -> loadPreloadScripts config.preloadScripts >>= \case
                    Left err -> RT.writeErrorLn err
                    Right env -> do
                        (result, _) <- eval env ast
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
    executeProgramLoop = do
        env <- loadPreloadScripts config.preloadScripts >>= \case
            Left err -> RT.writeErrorLn err >> pure RT.emptyEnvironment
            Right env -> pure env
        loop env
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

parseValidate :: String -> Either SyntaxError Ast
parseValidate input = case parse input of
    Left err -> Left err
    Right ast -> case validateAst ast of
        Left err -> Left  err
        Right _  -> Right ast

loadPreloadScripts :: (RT.ExecIO m) => [PreloadScript] -> m (Either String RT.Environment)
loadPreloadScripts = runExceptT <$> foldM loadScripts RT.emptyEnvironment
  where
    loadScripts env script = do
      ast <- case parseValidate script.scriptBody of
          Left err  -> throwError $ "Syntax error in script " ++ scriptName script ++ ": " ++ show err
          Right ast -> pure ast
      (result, env') <- lift $ eval env ast
      case result of
          Left err -> throwError $ "Runtime error in script " ++ scriptName script ++ ": " ++ show err
          Right _  -> pure env'

data ReadSExprState = ReadSExprState
    { parensDepth     :: Int
    , isInsideString  :: Bool
    , isInsideComment :: Bool
    }

readSExpr :: (RT.ExecIO m) => m String
readSExpr = readLoop "" ReadSExprState
    { parensDepth = 0
    , isInsideString = False
    , isInsideComment = False
    }
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
        updateByChar s (prev, cur) = if s.isInsideComment
            then s { isInsideComment = cur /= '\n' }
            else
                case cur of
                    ';' | not s.isInsideString -> s { isInsideComment = True }
                    '(' | not s.isInsideString -> s { parensDepth = s.parensDepth + 1 }
                    ')' | not s.isInsideString -> s { parensDepth = s.parensDepth - 1 }
                    '"' | not s.isInsideString -> s { isInsideString = True }
                    '"' | prev /= Just '\\'    -> s { isInsideString = False }
                    _                          -> s
