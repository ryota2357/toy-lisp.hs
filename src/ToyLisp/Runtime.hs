{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData          #-}

module ToyLisp.Runtime where

import qualified Data.Map.Strict as M
import qualified Data.Text       as T
import           System.IO       (hFlush, hPutStr, stderr, stdout)
import           ToyLisp.Syntax  (Ast, Symbol, TextRange, unSymbol)

class (Monad m) => ExecIO m where
    writeOutput   :: String -> m ()
    writeOutputLn :: String -> m ()
    writeOutputLn = writeOutput . (++ "\n")
    writeError   :: String -> m ()
    writeErrorLn :: String -> m ()
    writeErrorLn = writeError . (++ "\n")
    readInputLine :: m String

instance ExecIO IO where
    writeOutput = (>> hFlush stdout) . putStr
    writeError  = (>> hFlush stderr) . hPutStr stderr
    readInputLine = getLine

data LispObject
    = LispInt Integer
    | LispFloat Double
    | LispString T.Text
    | LispSymbol Symbol
    | LispList [LispObject]
    | LispFunction FunctionInfo
    | LispSystemFunction Symbol
    | LispTrue
    deriving (Show, Eq)

displayLispObjectWith :: (LispObject -> Maybe String) -> LispObject -> String
displayLispObjectWith override obj = case override obj of
    Just str -> str
    Nothing  -> case obj of
        LispInt i      -> show i
        LispFloat f    -> show f
        LispString s   -> "\"" ++ T.unpack s ++ "\""
        LispSymbol s   -> T.unpack $ unSymbol s
        LispList []    -> "NIL"
        LispList xs    -> "(" ++ unwords (map (displayLispObjectWith override) xs) ++ ")"
        LispTrue       -> "T"
        LispFunction _ -> "<function>"
        LispSystemFunction name -> "<function " ++ T.unpack (unSymbol name) ++ ">"

data FunctionInfo = FunctionInfo
    { functionParams      :: [Symbol]
    , functionBody        :: Ast
    , functionIntialFrame :: LexicalFrame
    } deriving (Show, Eq)

class Bindings a where
    lookupValueBinding :: Symbol -> a -> Maybe LispObject
    lookupFunctionBinding :: Symbol -> a -> Maybe FunctionInfo
    insertValueBinding :: Symbol -> LispObject -> a -> a
    insertFunctionBinding :: Symbol -> FunctionInfo -> a -> a

class Bindings a => Frame a where
    parentFrame :: a -> Maybe a

lookupFrameValueBinding :: Frame a => Symbol -> a -> Maybe LispObject
lookupFrameValueBinding name frame = case lookupValueBinding name frame of
    Just obj -> Just obj
    Nothing  -> case parentFrame frame of
        Just parent -> lookupFrameValueBinding name parent
        Nothing     -> Nothing

lookupFrameFunctionBinding :: Frame a => Symbol -> a -> Maybe FunctionInfo
lookupFrameFunctionBinding name frame = case lookupFunctionBinding name frame of
    Just obj -> Just obj
    Nothing  -> case parentFrame frame of
        Just parent -> lookupFrameFunctionBinding name parent
        Nothing     -> Nothing

data LexicalFrame = LexicalFrame
    { lexicalValueBindings    :: M.Map Symbol LispObject
    , lexicalFunctionBindings :: M.Map Symbol FunctionInfo
    , parentLexicalFrame      :: Maybe LexicalFrame
    } deriving (Show, Eq)

instance Bindings LexicalFrame where
    lookupValueBinding name = M.lookup name . lexicalValueBindings
    lookupFunctionBinding name = M.lookup name . lexicalFunctionBindings
    insertValueBinding name value frame = frame
        { lexicalValueBindings = M.insert name value frame.lexicalValueBindings
        }
    insertFunctionBinding name value frame = frame
        { lexicalFunctionBindings = M.insert name value frame.lexicalFunctionBindings
        }

instance Frame LexicalFrame where
    parentFrame = parentLexicalFrame

data SpecialFrame = SpecialFrame
    { specialValueBindings    :: M.Map Symbol LispObject
    , specialFunctionBindings :: M.Map Symbol FunctionInfo
    , parentSpecialFrame      :: Maybe SpecialFrame
    } deriving (Show, Eq)

instance Bindings SpecialFrame where
    lookupValueBinding name = M.lookup name . specialValueBindings
    lookupFunctionBinding name = M.lookup name . specialFunctionBindings
    insertValueBinding name value frame = frame
        { specialValueBindings = M.insert name value frame.specialValueBindings
        }
    insertFunctionBinding name value frame = frame
        { specialFunctionBindings = M.insert name value frame.specialFunctionBindings
        }

instance Frame SpecialFrame where
    parentFrame = parentSpecialFrame

data GlobalBindings = GlobalBindings
    { globalValueBindings    :: M.Map Symbol LispObject
    , globalFunctionBindings :: M.Map Symbol FunctionInfo
    } deriving (Show, Eq)

instance Bindings GlobalBindings where
    lookupValueBinding name = M.lookup name . globalValueBindings
    lookupFunctionBinding name = M.lookup name . globalFunctionBindings
    insertValueBinding name value bindings = bindings
        { globalValueBindings = M.insert name value bindings.globalValueBindings
        }
    insertFunctionBinding name value bindings = bindings
        { globalFunctionBindings = M.insert name value bindings.globalFunctionBindings
        }

data Environment = Environment
    { globalBindings      :: GlobalBindings
    , currentLexicalFrame :: LexicalFrame
    , currentSpecialFrame :: SpecialFrame
    } deriving (Show, Eq)

emptyEnvironment :: Environment
emptyEnvironment = Environment
    { globalBindings = GlobalBindings M.empty M.empty
    , currentLexicalFrame = LexicalFrame M.empty M.empty Nothing
    , currentSpecialFrame = SpecialFrame M.empty M.empty Nothing
    }

data RuntimeError = RuntimeError
    { runtimeErrorPosition :: TextRange
    , runtimeErrorMessage  :: T.Text
    } deriving (Eq)

instance Show RuntimeError where
    show (RuntimeError range message) = "Runtime error at " ++ show range ++ ": " ++ T.unpack message
