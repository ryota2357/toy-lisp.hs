{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

module ToyLisp.Evaluator (eval) where

import           Control.Monad        (foldM, forM)
import           Control.Monad.Except (ExceptT, MonadError, runExceptT,
                                       throwError)
import           Control.Monad.State  (StateT, get, gets, modify', put,
                                       runStateT)
import           Control.Monad.Trans  (lift)
import qualified Data.Bifunctor       as BF
import           Data.Function        (fix)
import           Data.List            (uncons)
import qualified Data.Map.Strict      as M
import qualified Data.Text            as T
import qualified ToyLisp.Runtime      as RT
import           ToyLisp.Runtime      (Environment (..), ExecIO,
                                       FunctionInfo (..), LexicalFrame (..),
                                       LispObject (..), RuntimeError (..))
import           ToyLisp.Syntax       (Ast (..), AstNode (..), Symbol,
                                       TextRange, astNodePosition, unSymbol)

eval :: (ExecIO m) => Environment -> Ast -> m (Either RuntimeError LispObject, Environment)
eval env (Ast nodes) = do
    result <- runEvaluator $ mapM evalNode nodes
    pure $ case result of
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
    IntNode _ i -> pure $ LispInt i
    FloatNode _ f -> pure $ LispFloat f
    StringNode _ s -> pure $ LispString s
    SymbolNode pos sym -> do
        env <- get
        case () of
            _ | Just obj <- M.lookup sym systemValueBindingsMap -> pure obj
              | Just obj <- RT.lookupFrameValueBinding sym env.currentLexicalFrame -> pure obj
              | Just obj <- RT.lookupFrameValueBinding sym env.currentSpecialFrame -> pure obj
              | Just obj <- RT.lookupValueBinding sym env.globalBindings -> pure obj
              | otherwise -> throwRuntimeError pos $ "Unbound symbol: " <> unSymbol sym
    ListNode _ [] -> pure $ LispList []
    ListNode _ (SymbolNode fnPos fnName : args) ->
        case M.lookup fnName (systemFunctionBindingsMap @m) of
            Just fn -> do
                result <- fn args
                case result of
                    Right val -> pure val
                    Left err  -> throwRuntimeError fnPos err
            Nothing -> do
                lexicalFrame <- gets currentLexicalFrame
                case RT.lookupFrameFunctionBinding fnName lexicalFrame of
                    Just fnInfo -> do
                        argValues <- mapM evalNode args
                        result <- callFunction fnInfo argValues
                        case result of
                            Right val -> pure val
                            Left err  -> throwRuntimeError fnPos err
                    Nothing -> do
                        -- Evaluate arguments first because the global function may be defined/updated by the arguments themselves
                        argValues <- mapM evalNode args
                        globals <- gets globalBindings -- Get the updated global bindings
                        case RT.lookupFunctionBinding fnName globals of
                            Just fnInfo -> do
                                result <- callFunction fnInfo argValues
                                case result of
                                    Right val -> pure val
                                    Left err  -> throwRuntimeError fnPos err
                            Nothing -> throwRuntimeError fnPos $ "Undefined function: " <> unSymbol fnName
    ListNode _ (notSymbolNode : _) ->
        throwRuntimeError (astNodePosition notSymbolNode) "Function name is not a symbol"

throwWrongNumberOfArgsError :: MonadError T.Text m' => Int -> String -> m' a -- m is already used
throwWrongNumberOfArgsError given expected = throwError errorText
  where
    givenText = T.pack $ show given
    expectedText = T.pack expected
    errorText = "Wrong number of arguments: given " <> givenText <> ", expected " <> expectedText

callFunction :: forall m. (ExecIO m) => FunctionInfo -> [LispObject] -> Evaluator m (Either T.Text LispObject)
callFunction fn args =
    let argCount   = length args
        paramCount = length fn.functionParams
    in runExceptT $ if argCount /= paramCount
        then
            throwWrongNumberOfArgsError argCount (show paramCount)
        else do
            env <- get
            let nextEnv = env
                    { currentLexicalFrame = foldl
                        (\frame (param, arg) -> RT.insertValueBinding param arg frame)
                        fn.functionIntialFrame
                        (zip fn.functionParams args)
                    }
            (result, nextEnv') <- lift . lift . lift $ eval nextEnv fn.functionBody
            case result of
                Left err -> lift $ throwError err
                Right obj  -> do
                    -- Restore the environment with the updated global bindings
                    put env { globalBindings = nextEnv'.globalBindings }
                    pure obj

systemValueBindingsMap :: M.Map Symbol LispObject
systemValueBindingsMap = M.fromList
    [ ("nil", LispList [])
    , ("pi", LispFloat pi)
    , ("t", LispTrue)
    ]

systemFunctionBindingsMap :: forall m. (ExecIO m) => M.Map Symbol ([AstNode] -> Evaluator m (Either T.Text LispObject))
systemFunctionBindingsMap = M.fromList $ map (BF.second (runExceptT <$>)) (
    [ ("+", \args -> do
        argValues <- lift $ mapM evalNode args
        foldM (\acc v -> case (acc, v) of
            (LispInt a, LispInt b)     -> pure $ LispInt (a + b)
            (LispFloat a, LispFloat b) -> pure $ LispFloat (a + b)
            (LispInt a, LispFloat b)   -> pure $ LispFloat (fromIntegral a + b)
            (LispFloat a, LispInt b)   -> pure $ LispFloat (a + fromIntegral b)
            _                          -> throwError "Arguments are not numbers"
            ) (LispInt 0) argValues
      )
    , ("-", \args -> do
        argValues <- lift $ mapM evalNode args
        case argValues of
            [] -> throwWrongNumberOfArgsError 0 ">= 1"
            [e] -> case e of
                LispInt i   -> pure $ LispInt (-i)
                LispFloat f -> pure $ LispFloat (-f)
                _           -> throwError "Argument is not a number"
            e : es -> foldM (\acc v -> case (acc, v) of
                (LispInt a, LispInt b)     -> pure $ LispInt (a - b)
                (LispFloat a, LispFloat b) -> pure $ LispFloat (a - b)
                (LispInt a, LispFloat b)   -> pure $ LispFloat (fromIntegral a - b)
                (LispFloat a, LispInt b)   -> pure $ LispFloat (a - fromIntegral b)
                _                          -> throwError "Arguments are not numbers"
                ) e es
      )
    , ("*", \args -> do
        argValues <- lift $ mapM evalNode args
        foldM (\acc v -> case (acc, v) of
            (LispInt a, LispInt b)     -> pure $ LispInt (a * b)
            (LispFloat a, LispFloat b) -> pure $ LispFloat (a * b)
            (LispInt a, LispFloat b)   -> pure $ LispFloat (fromIntegral a * b)
            (LispFloat a, LispInt b)   -> pure $ LispFloat (a * fromIntegral b)
            _                          -> throwError "Arguments are not numbers"
            ) (LispInt 1) argValues
      )
    , ("/", \args -> do
        argValues <- lift $ mapM evalNode args
        let divideByZeroMsg = "Division by zero"
        case argValues of
            [] -> throwWrongNumberOfArgsError 0 ">= 1"
            [e] -> case e of
                LispInt 0   -> throwError divideByZeroMsg
                LispFloat 0 -> throwError divideByZeroMsg
                LispInt i   -> pure $ LispFloat (1 / fromIntegral i)
                LispFloat f -> pure $ LispFloat (1 / f)
                _           -> throwError "Argument is not a number"
            e : es -> foldM (\acc v -> case (acc, v) of
                (LispInt a, LispInt b)     | b /= 0 -> pure $ if a `mod` b == 0
                                                                then LispInt (a `div` b)
                                                                else LispFloat (fromIntegral a / fromIntegral b)
                (LispFloat a, LispFloat b) | b /= 0 -> pure $ LispFloat (a / b)
                (LispInt a, LispFloat b)   | b /= 0 -> pure $ LispFloat (fromIntegral a / b)
                (LispFloat a, LispInt b)   | b /= 0 -> pure $ LispFloat (a / fromIntegral b)
                (LispInt _, LispInt 0)     -> throwError divideByZeroMsg
                (LispFloat _, LispFloat 0) -> throwError divideByZeroMsg
                (LispInt _, LispFloat 0)   -> throwError divideByZeroMsg
                (LispFloat _, LispInt 0)   -> throwError divideByZeroMsg
                _                          -> throwError "Arguments are not numbers"
                ) e es
      )
    ]
    ++
    map (\(op, opI, opF) -> (op, \args -> do
        argValues <- lift $ mapM evalNode args
        let t = LispTrue; nil = LispList []
        let notNumberMsg = "Argument is not a number"
        case argValues of
            [] -> throwWrongNumberOfArgsError 0 ">= 1"
            [LispInt _] -> pure t
            [LispFloat _] -> pure t
            [_] -> throwError notNumberMsg
            LispInt x : xs -> case () of
                _ | Right xs' <- forM xs (\case LispInt i -> Right i; _ -> Left ()) ->
                    pure $ if all (`opI` x) xs' then t else nil
                  | Right xs' <- forM xs (\case LispInt i -> Right $ fromIntegral i; LispFloat f -> Right f; _ -> Left ()) ->
                    pure $ if all (`opF` fromIntegral x) xs' then t else nil
                  | otherwise -> throwError notNumberMsg
            LispFloat x : xs -> case () of
                _ | Right xs' <- forM xs (\case LispInt i -> Right $ fromIntegral i; LispFloat f -> Right f; _ -> Left ()) ->
                    pure $ if all (`opF` x) xs' then t else nil
                  | otherwise -> throwError notNumberMsg
            _ -> throwError notNumberMsg
        ))
    [ ("=",  (==), (==))
    , ("/=", (/=), (/=))
    ]
    ++
    map (\(op, opI, opF) -> (op, \args -> do
        argValues <- lift $ mapM evalNode args
        let notNumberMsg = "Argument is not a number"
        case argValues of
            [] -> throwWrongNumberOfArgsError 0 ">= 1"
            [LispInt _] -> pure LispTrue
            [LispFloat _] -> pure LispTrue
            [_] -> throwError notNumberMsg
            _ : xs -> case foldM (\_ v -> case v of
                    (LispInt a, LispInt b)     -> if a `opI` b then Right () else Left True
                    (LispFloat a, LispFloat b) -> if a `opF` b then Right () else Left True
                    (LispInt a, LispFloat b)   -> if fromIntegral a `opF` b then Right () else Left True
                    (LispFloat a, LispInt b)   -> if a `opF` fromIntegral b then Right () else Left True
                    _ -> Left False
                    ) () $ zip argValues xs
                of
                    Right _    -> pure LispTrue
                    Left True  -> pure $ LispList []
                    Left False -> throwError notNumberMsg
        ))
    [ (">", (>), (>))
    , ("<", (<), (<))
    , (">=", (>=), (>=))
    , ("<=", (<=), (<=))
    ]
    ++
    [ ("car", \args -> do
        argValues <- lift $ mapM evalNode args
        case argValues of
            [LispList []]      -> pure $ LispList []
            [LispList (x : _)] -> pure x
            [_]                -> throwError "Argument is not a list"
            _                  -> throwWrongNumberOfArgsError (length args) "1"
      )
    , ("cdr", \args -> do
        argValues <- lift $ mapM evalNode args
        case argValues of
            [LispList []]       -> pure $ LispList []
            [LispList (_ : xs)] -> pure $ LispList xs
            [_]                 -> throwError "Argument is not a list"
            _                   -> throwWrongNumberOfArgsError (length args) "1"
      )
    , ("defun", \case
        SymbolNode _ fnName : ListNode _ params : body -> if fnName `M.member` (systemFunctionBindingsMap @m)
            then throwError $ "Redefining " <> unSymbol fnName <> " is not allowed"
            else case forM params (\case SymbolNode _ s -> Right s; _ -> Left ()) of
                Right params' -> do
                    frame <- gets currentLexicalFrame
                    let fnInfo = FunctionInfo params' (Ast body) frame
                    modify' $ \env -> env
                        { globalBindings = RT.insertFunctionBinding fnName fnInfo env.globalBindings
                        }
                    pure $ LispFunction fnInfo
                Left _ -> throwError "Function parameters are not a list of symbols"
        SymbolNode _ _ : invalidParam | not $ null invalidParam ->
            throwError "Function parameters are not a list"
        _ : _ : _ -> throwError "Function name is not a symbol"
        args -> throwWrongNumberOfArgsError (length args) ">= 2"
      )
    , ("if", \case
        [cond, thenExpr] -> do
            condValue <- lift $ evalNode cond
            case condValue of
                LispList [] -> pure $ LispList []
                _           -> lift $ evalNode thenExpr
        [cond, thenExpr, elseExpr] -> do
            condValue <- lift $ evalNode cond
            lift $ case condValue of
                LispList [] -> evalNode elseExpr
                _           -> evalNode thenExpr
        args -> throwWrongNumberOfArgsError (length args) "2 or 3"
      )
    , ("let", \case
        ListNode _ bindings : body -> do
            bindings' <- evalBindings bindings
            env <- get
            let nextEnv = env
                    { currentLexicalFrame = LexicalFrame
                        { lexicalValueBindings = M.fromList bindings'
                        , lexicalFunctionBindings = M.empty
                        , parentLexicalFrame = Just env.currentLexicalFrame
                        }
                    }
            (result, nextEnv') <- lift . lift . lift $ eval nextEnv (Ast body)
            case result of
                Left err  -> lift $ throwError err
                Right obj -> do
                    -- Restore the environment with the updated global bindings
                    put env { globalBindings = nextEnv'.globalBindings }
                    pure obj
          where
            evalBindings = mapM (\case
                ListNode _ (SymbolNode _ sym : values) -> case uncons values of
                    Just (value, []) -> do
                        value' <- lift $ evalNode value
                        pure (sym, value')
                    Just (_, _) -> throwError $ "Binding for " <> unSymbol sym <> " is not a single value"
                    Nothing -> pure (sym, LispList [])
                SymbolNode _ sym -> pure (sym, LispList [])
                _ -> throwError "Variable name is not a symbol")
        _ : _ -> throwError "Bindings are not a list"
        args -> throwWrongNumberOfArgsError (length args) ">= 1"
      )
    , ("list", \args -> do
        argValues <- lift $ mapM evalNode args
        pure $ LispList argValues
      )
    , ("nthcdr", \args -> do
        argValues <- lift $ mapM evalNode args
        case argValues of
            [LispInt n, LispList xs] -> pure $ LispList $ drop (fromIntegral n) xs
            [LispInt _, _]           -> throwError "Second argument is not a list"
            [_, _]                   -> throwError "First argument is not an integer"
            _                        -> throwWrongNumberOfArgsError (length args) "2"
      )
    , ("princ", \args -> do
        argValues <- lift $ mapM evalNode args
        case argValues of
            [arg] -> do
                let display = RT.displayLispObjectWith $ \case
                        LispString s -> Just $ T.unpack s
                        _            -> Nothing
                lift . lift . lift $ RT.writeOutput $ display arg
                pure  arg
            _ -> throwWrongNumberOfArgsError (length args) "1"
      )
    , ("prog1", \args -> do
        argValues <- lift $ mapM evalNode args
        pure $ case uncons argValues of
            Just (first, _) -> first
            Nothing         -> LispList []
      )
    , ("progn", \args -> do
        argValues <- lift $ mapM evalNode args
        pure $ case argValues of
            [] -> LispList []
            _  -> last argValues
      )
    , ("quote", \args -> case args of
        [arg] -> pure $ fix (\quote -> \case
            IntNode _ i -> LispInt i
            FloatNode _ f -> LispFloat f
            StringNode _ s -> LispString s
            SymbolNode _ sym -> LispSymbol sym
            ListNode _ nodes -> LispList $ map quote nodes
            ) arg
        _ -> throwWrongNumberOfArgsError (length args) "1"
      )
    , ("setq", \case
        [SymbolNode _ sym, value] -> if sym `M.member` systemValueBindingsMap
            then throwError $ unSymbol sym <> " is a constant and thus cannot be set"
            else do
                value' <- lift $ evalNode value
                modify' $ \env -> env
                    { globalBindings = RT.insertValueBinding sym value' env.globalBindings
                    }
                pure value'
        [_, _] -> throwError "Variable name is not a symbol"
        args -> throwWrongNumberOfArgsError (length args) "2"
      )
    , ("type-of", \args -> do
        argValues <- lift $ mapM evalNode args
        case argValues of
            [arg] -> pure $ LispSymbol $ case arg of
                    LispInt _      -> "INTEGER"
                    LispFloat _    -> "FLOAT"
                    LispString _   -> "STRING"
                    LispSymbol _   -> "SYMBOL"
                    LispList _     -> "LIST"
                    LispFunction _ -> "FUNCTION"
                    LispTrue       -> "T"
            _ -> throwWrongNumberOfArgsError (length args) "1"
      )
    ])
