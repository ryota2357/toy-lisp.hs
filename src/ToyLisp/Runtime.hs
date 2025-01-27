{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData          #-}

module ToyLisp.Runtime where

import qualified Data.Map.Strict as M
import qualified Data.Text       as T
import           ToyLisp.Syntax  (Ast, AstNode, Symbol, TextRange)

data LispObject
    = LispInt Integer
    | LispFloat Double
    | LispString T.Text
    | LispList [LispObject]
    | LispFunction FunctionInfo
    | LispTrue
    deriving (Show, Eq)

data FunctionInfo = FunctionInfo
    { functionArgs :: [AstNode]
    , functionBody :: Ast
    } deriving (Show, Eq)

data CallFrame = CallFrame
    { valueBindings    :: M.Map Symbol LispObject
    , functionBindings :: M.Map Symbol FunctionInfo
    , specialFrameInfo :: (SpecialFrame, Bool)
    , parentCallFrame  :: Maybe CallFrame
    } deriving (Show, Eq)

data SpecialFrame = SpecialFrame
    { specialBindings    :: M.Map Symbol LispObject
    , parentSpecialFrame :: Maybe SpecialFrame
    } deriving (Show, Eq)

data GlobalBindings = GlobalBindings
    { glovalValueBindings    :: M.Map Symbol LispObject
    , glovalFunctionBindings :: M.Map Symbol FunctionInfo
    } deriving (Show, Eq)

data Environment = Environment
    { globalBindings   :: GlobalBindings
    , currentCallFrame :: CallFrame
    } deriving (Show, Eq)

lookupBindingValue :: Symbol -> Environment -> Maybe LispObject
lookupBindingValue symbol env = do
    case () of
        _ | Just obj <- lookupSpecialBindings symbol (fst env.currentCallFrame.specialFrameInfo) -> Just obj
          | Just obj <- lookupLexicalBindings symbol env.currentCallFrame -> Just obj
          | Just obj <- lookupGlobalBindings symbol env.globalBindings -> Just obj
          | otherwise -> Nothing
  where
    lookupSpecialBindings name frame = case M.lookup name frame.specialBindings of
        Just obj -> Just obj
        Nothing  -> case frame.parentSpecialFrame of
            Just parent -> lookupSpecialBindings name parent
            Nothing     -> Nothing
    lookupLexicalBindings name frame = case M.lookup name frame.valueBindings of
        Just obj -> Just obj
        Nothing  -> case frame.parentCallFrame of
            Just parent -> lookupLexicalBindings name parent
            Nothing     -> Nothing
    lookupGlobalBindings name bindings = M.lookup name bindings.glovalValueBindings

lookupFunctinBinding :: Symbol -> Environment -> Maybe FunctionInfo
lookupFunctinBinding symbol env = do
    case () of
         _| Just obj <- lookupLexicalBindings symbol env.currentCallFrame -> Just obj
          | Just obj <- lookupGlobalBindings symbol env.globalBindings -> Just obj
          | otherwise -> Nothing
  where
    lookupLexicalBindings name frame = case M.lookup name frame.functionBindings of
        Just obj -> Just obj
        Nothing  -> case frame.parentCallFrame of
            Just parent -> lookupLexicalBindings name parent
            Nothing     -> Nothing
    lookupGlobalBindings name bindings = M.lookup name bindings.glovalFunctionBindings

data RuntimeError = RuntimeError
    { runtimeErrorPosition :: TextRange
    , runtimeErrorMessage  :: T.Text
    } deriving (Eq)

instance Show RuntimeError where
    show (RuntimeError range message) = "Runtime error at " ++ show range ++ ": " ++ T.unpack message
