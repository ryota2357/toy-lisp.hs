{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module ToyLisp.Evaluator (eval, evalContinue, EvalIO(..)) where

import           Control.Monad.Except (ExceptT, MonadError, runExceptT,
                                       throwError)
import           Control.Monad.State  (StateT, get, lift, runStateT)
import           Data.Function        (fix)
import qualified Data.Map.Strict      as M
import qualified Data.Text            as T
import qualified ToyLisp.Runtime      as RT
import           ToyLisp.Runtime      (CallFrame (..), Environment (..),
                                       GlobalBindings (..), LispObject (..),
                                       RuntimeError (..), SpecialFrame (..))
import           ToyLisp.Syntax       (Ast (..), AstNode (..), Symbol,
                                       TextRange, unSymbol)

class (Monad m) => EvalIO m where
    writeOutput :: String -> m ()
    writeOutputLn :: String -> m ()
    writeOutputLn text = writeOutput $ text ++ "\n"

instance EvalIO IO where
    writeOutput = putStr

eval :: (EvalIO m) => Ast -> m (Either RuntimeError Environment)
eval (Ast nodes) = do
    evalContinue env (Ast nodes)
  where
    env = Environment
        { globalBindings = GlobalBindings M.empty M.empty
        , currentCallFrame = CallFrame
            { valueBindings = M.empty
            , functionBindings = M.empty
            , specialFrameInfo = (SpecialFrame M.empty Nothing, False)
            , parentCallFrame = Nothing
            }
        }

evalContinue :: (EvalIO m) => Environment -> Ast -> m (Either RuntimeError Environment)
evalContinue env (Ast nodes) = do
    result <- runEvaluator $ mapM_ evalNode nodes
    return $ case result of
        Left err            -> Left err
        Right (_, finalEnv) -> Right finalEnv
  where
    runEvaluator evaluator = runExceptT $ runStateT evaluator env

type Evaluator m = StateT Environment (ExceptT RuntimeError m)

throwRuntimeError :: MonadError RuntimeError m => TextRange -> T.Text -> m a
throwRuntimeError pos msg = throwError $ RuntimeError pos msg

evalNode :: forall m. (EvalIO m) => AstNode -> Evaluator m LispObject
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
        argValues <- mapM evalNode args
        case M.lookup fnName (systemFunctionBindingsMap @m) of
            Just fn -> do
                result <- fn argValues
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

systemFunctionBindingsMap :: (EvalIO m) => M.Map Symbol ([LispObject] -> Evaluator m (Either T.Text LispObject))
systemFunctionBindingsMap = M.fromList
    [ ("+", \args -> return $ case args of
            [LispInt a, LispInt b]     -> Right $ LispInt (a + b)
            [LispFloat a, LispFloat b] -> Right $ LispFloat (a + b)
            [LispInt a, LispFloat b]   -> Right $ LispFloat (fromIntegral a + b)
            [LispFloat a, LispInt b]   -> Right $ LispFloat (a + fromIntegral b)
            _                          -> Left "Invalid arguments for +"
      )
    , ("-", \args -> return $ case args of
            [LispInt a, LispInt b]     -> Right $ LispInt (a - b)
            [LispFloat a, LispFloat b] -> Right $ LispFloat (a - b)
            [LispInt a, LispFloat b]   -> Right $ LispFloat (fromIntegral a - b)
            [LispFloat a, LispInt b]   -> Right $ LispFloat (a - fromIntegral b)
            _                          -> Left "Invalid arguments for -"
      )
    , ("*", \args -> return $ case args of
            [LispInt a, LispInt b]     -> Right $ LispInt (a * b)
            [LispFloat a, LispFloat b] -> Right $ LispFloat (a * b)
            [LispInt a, LispFloat b]   -> Right $ LispFloat (fromIntegral a * b)
            [LispFloat a, LispInt b]   -> Right $ LispFloat (a * fromIntegral b)
            _                          -> Left "Invalid arguments for *"
      )
    , ("/", \args -> return $ case args of
            [LispInt a, LispInt b]     -> Right $ LispInt (a `div` b)
            [LispFloat a, LispFloat b] -> Right $ LispFloat (a / b)
            [LispInt a, LispFloat b]   -> Right $ LispFloat (fromIntegral a / b)
            [LispFloat a, LispInt b]   -> Right $ LispFloat (a / fromIntegral b)
            _                          -> Left "Invalid arguments for /"
      )
    , ("princ", \case
            [arg] -> do
                lift $ lift $ writeOutput $ fix (\self -> \case
                    LispInt i      -> show i
                    LispFloat f    -> show f
                    LispString s   -> T.unpack s
                    LispList []    -> "NIL"
                    LispList xs    -> "(" ++ unwords (map self xs) ++ ")"
                    LispTrue       -> "T"
                    LispFunction _ -> "<function>") arg
                return $ Right arg
            _   -> return $ Left "Invalid arguments for PRINC"
      )
    ]

