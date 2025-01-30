{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module ToyLisp.Evaluator (eval) where

import           Control.Monad.Except (ExceptT, MonadError, runExceptT,
                                       throwError)
import           Control.Monad.State  (StateT, get, lift, runStateT)
import           Data.Function        (fix)
import qualified Data.Map.Strict      as M
import qualified Data.Text            as T
import qualified ToyLisp.Runtime      as RT
import           ToyLisp.Runtime      (Environment, ExecIO, LispObject (..),
                                       RuntimeError (..))
import           ToyLisp.Syntax       (Ast (..), AstNode (..), Symbol,
                                       TextRange, unSymbol)

eval :: (ExecIO m) => Environment -> Ast -> m (Either RuntimeError LispObject, Environment)
eval env (Ast nodes) = do
    result <- runEvaluator $ mapM evalNode nodes
    return $ case result of
        (Left err, finalEnv) -> (Left err, finalEnv)
        (Right objs, finalEnv) -> case objs of
            []    -> (Right $ LispList [], finalEnv)
            objs' -> (Right $ last objs', finalEnv)
  where
    runEvaluator evaluator = runStateT (runExceptT evaluator) env

type Evaluator m = ExceptT RuntimeError (StateT Environment m)

throwRuntimeError :: MonadError RuntimeError m => TextRange -> T.Text -> m a
throwRuntimeError pos msg = throwError $ RuntimeError pos msg

evalNode :: forall m. (ExecIO m) => AstNode -> Evaluator m LispObject
evalNode = \case
    IntNode _ i -> return $ LispInt i
    FloatNode _ f -> return $ LispFloat f
    StringNode _ s -> return $ LispString s
    SymbolNode pos sym -> do
        env <- get
        case M.lookup sym systemValueBindingsMap of
            Just obj -> return obj
            Nothing  -> case RT.lookupBindingValue sym env of
                Just obj -> return obj
                Nothing  -> throwRuntimeError pos $ "Unbound symbol: " <> unSymbol sym
    ListNode _ [] -> return $ LispList []
    ListNode _ (SymbolNode fnPos fnName : args) -> do
        case M.lookup fnName (systemFunctionBindingsMap @m) of
            Just fn -> do
                result <- fn args
                case result of
                    Right val -> return val
                    Left err  -> throwRuntimeError fnPos err
            Nothing -> throwRuntimeError fnPos $ "Unknown function: " <> unSymbol fnName
    ListNode _ _ -> error "Not implemented"

systemValueBindingsMap :: M.Map Symbol LispObject
systemValueBindingsMap = M.fromList
    [ ("nil", LispList [])
    , ("t", LispTrue)
    ]

systemFunctionBindingsMap :: (ExecIO m) => M.Map Symbol ([AstNode] -> Evaluator m (Either T.Text LispObject))
systemFunctionBindingsMap = M.fromList
    [ ("+", \args -> do
        argValues <- mapM evalNode args
        return $ case argValues of
            [LispInt a, LispInt b]     -> Right $ LispInt (a + b)
            [LispFloat a, LispFloat b] -> Right $ LispFloat (a + b)
            [LispInt a, LispFloat b]   -> Right $ LispFloat (fromIntegral a + b)
            [LispFloat a, LispInt b]   -> Right $ LispFloat (a + fromIntegral b)
            [_, _]                     -> Left "Arguments are not numbers"
            _                          -> Left $ mkInvalidArgCountErrorText "+" args
      )
    , ("-", \args -> do
        argValues <- mapM evalNode args
        return $ case argValues of
            [LispInt a, LispInt b]     -> Right $ LispInt (a - b)
            [LispFloat a, LispFloat b] -> Right $ LispFloat (a - b)
            [LispInt a, LispFloat b]   -> Right $ LispFloat (fromIntegral a - b)
            [LispFloat a, LispInt b]   -> Right $ LispFloat (a - fromIntegral b)
            [_, _]                     -> Left "Arguments are not numbers"
            _                          -> Left $ mkInvalidArgCountErrorText "-" args
      )
    , ("*", \args -> do
        argValues <- mapM evalNode args
        return $ case argValues of
            [LispInt a, LispInt b]     -> Right $ LispInt (a * b)
            [LispFloat a, LispFloat b] -> Right $ LispFloat (a * b)
            [LispInt a, LispFloat b]   -> Right $ LispFloat (fromIntegral a * b)
            [LispFloat a, LispInt b]   -> Right $ LispFloat (a * fromIntegral b)
            [_, _]                     -> Left "Arguments are not numbers"
            _                          -> Left $ mkInvalidArgCountErrorText "*" args
      )
    , ("/", \args -> do
        argValues <- mapM evalNode args
        return $ case argValues of
            [LispInt a, LispInt b]     -> Right $ LispInt (a `div` b)
            [LispFloat a, LispFloat b] -> Right $ LispFloat (a / b)
            [LispInt a, LispFloat b]   -> Right $ LispFloat (fromIntegral a / b)
            [LispFloat a, LispInt b]   -> Right $ LispFloat (a / fromIntegral b)
            [_, _]                     -> Left "Arguments are not numbers"
            _                          -> Left $ mkInvalidArgCountErrorText "/" args
      )
    , ("princ", \args -> do
        argValues <- mapM evalNode args
        case argValues of
            [arg] -> do
                let display = RT.displayLispObjectWith $ \case
                        LispString s -> Just $ T.unpack s
                        _            -> Nothing
                lift $ lift $ RT.writeOutput $ display arg
                return $ Right arg
            _ -> return $ Left $ mkInvalidArgCountErrorText "princ" args
      )
    , ("quote", \args -> pure $ case args of
        [arg] -> Right $ fix (\quote -> \case
            IntNode _ i -> LispInt i
            FloatNode _ f -> LispFloat f
            StringNode _ s -> LispString s
            SymbolNode _ sym -> LispSymbol sym
            ListNode _ nodes -> LispList $ map quote nodes
            ) arg
        _ -> Left $ mkInvalidArgCountErrorText "quote" args
      )
    ]
  where
    mkInvalidArgCountErrorText fnName args = "Invalid number of arguments for " <> unSymbol fnName <> ": " <> argsCountText
      where
        argsCountText = T.pack $ show $ length args
