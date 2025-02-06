{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

module EvaluatorSpec (spec) where

import           Control.Monad.State (State, gets, modify, runState)
import           Test.Hspec
import           ToyLisp.Evaluator   (eval)
import qualified ToyLisp.Runtime     as RT
import           ToyLisp.Syntax      (Ast (..), AstNode (..), TextRange (..))

data TestIOState = TestIOState
    { testInputs :: [String]
    , testOutput :: String
    , testError  :: String
    }

newtype TestIO a = TestIO { runTestIO :: State TestIOState a }
    deriving (Functor, Applicative, Monad)

instance RT.ExecIO TestIO where
    writeOutput str = TestIO $ modify $ \s -> s { testOutput = s.testOutput ++ str }
    writeError str = TestIO $ modify $ \s -> s { testError = s.testError ++ str }
    readInputLine = TestIO $ do
        gets testInputs >>= \case
            [] -> pure ""
            (x:xs) -> do
                modify $ \s -> s { testInputs = xs }
                pure x

runEval :: Ast -> (Either RT.RuntimeError RT.LispObject, RT.Environment, TestIOState)
runEval = runEvalWith RT.emptyEnvironment

runEvalWith :: RT.Environment -> Ast -> (Either RT.RuntimeError RT.LispObject, RT.Environment, TestIOState)
runEvalWith env ast = (result, env', state)
  where
    ((result, env'), state) = runState (runTestIO $ eval env ast) (TestIOState
        { testInputs = []
        , testOutput = ""
        , testError = ""
        })

runEvalOutput :: Ast -> String
runEvalOutput ast = (third $ runEval ast).testOutput
  where
    third (_, _, x) = x

spec :: Spec
spec = do
    describe "eval" $ do
        it "empty input" $ do
            let ast = Ast []
            let (result, env, io) = runEval ast
            result `shouldBe` Right (RT.LispList [])
            env `shouldBe` RT.emptyEnvironment
            io.testOutput `shouldBe` ""

    describe "system function" $ do
        let s = TextRange 0 0

        describe "princ ok" $ do
            it "integer" $ do -- (princ 42)
                let ast = Ast [ListNode s [SymbolNode s "princ", IntNode s 42]]
                runEvalOutput ast `shouldBe` "42"

            it "float" $ do -- (princ 3.14)
                let ast = Ast [ListNode s [SymbolNode s "princ", FloatNode s 3.14]]
                runEvalOutput ast `shouldBe` "3.14"

            it "string" $ do -- (princ "hello")
                let ast = Ast [ListNode s [SymbolNode s "princ", StringNode s "hello"]]
                runEvalOutput ast `shouldBe` "hello"

            it "list" $ do -- (princ '(1 2 3))
                let ast = Ast [ListNode s [
                            SymbolNode s "princ", ListNode s [
                                SymbolNode s "quote", ListNode s [
                                    IntNode s 1, IntNode s 2, IntNode s 3]]]]
                runEvalOutput ast `shouldBe` "(1 2 3)"

            it "nil" $ do -- (princ nil)
               let nil_ast = Ast [ListNode s [SymbolNode s "princ", SymbolNode s "nil"]]
               runEvalOutput nil_ast `shouldBe` "NIL"
               let lst_ast = Ast [ListNode s [SymbolNode s "princ", ListNode s []]]
               runEvalOutput lst_ast `shouldBe` "NIL"

            it "t" $ do -- (princ t)
                let ast = Ast [ListNode s [SymbolNode s "princ", SymbolNode s "t"]]
                runEvalOutput ast `shouldBe` "T"

            it "multiple values" $ do -- (princ 1) (princ "a b") (princ 2)
                let ast = Ast [ ListNode s [SymbolNode s "princ", IntNode s 1]
                              , ListNode s [SymbolNode s "princ", StringNode s "a b"]
                              , ListNode s [SymbolNode s "princ", IntNode s 2]
                              ]
                runEvalOutput ast `shouldBe` "1a b2"

        describe "princ error" $ do
            it "no arguments" $ do -- (princ)
                let ast = Ast [ListNode (TextRange 0 7) [SymbolNode (TextRange 1 6) "princ"]]
                let (result, _, io) = runEval ast
                io.testOutput `shouldBe` ""
                result `shouldBe` Left (RT.RuntimeError (TextRange 1 6) "Wrong number of arguments: given 0, expected 1")

            it "too many arguments" $ do -- (princ 1 2)
                let ast = Ast [ListNode (TextRange 0 11) [SymbolNode (TextRange 1 6) "princ", IntNode s 1, IntNode s 2]]
                let (result, _, io) = runEval ast
                io.testOutput `shouldBe` ""
                result `shouldBe` Left (RT.RuntimeError (TextRange 1 6) "Wrong number of arguments: given 2, expected 1")

        describe "arithmetic operations" $ do
            let runEvalResultOk ast = let (result, _, _) = runEval ast in case result of
                 Right x -> x
                 Left e  -> error $ "Expected Right but got Left: " ++ show e

            it "addition" $ do
                let ast_0 = Ast [ListNode s [SymbolNode s "+"]]
                runEvalResultOk ast_0 `shouldBe` RT.LispInt 0
                let ast_i1 = Ast [ListNode s [SymbolNode s "+", IntNode s 3]]
                runEvalResultOk ast_i1 `shouldBe` RT.LispInt 3
                let ast_f1 = Ast [ListNode s [SymbolNode s "+", FloatNode s 3.14]]
                runEvalResultOk ast_f1 `shouldBe` RT.LispFloat 3.14
                let ast_is = Ast [ListNode s [SymbolNode s "+", IntNode s 1, IntNode s 2, IntNode s 3, IntNode s 4]]
                runEvalResultOk ast_is `shouldBe` RT.LispInt 10
                let ast_mixed = Ast [ListNode s [SymbolNode s "+", IntNode s 1, FloatNode s 2.5]]
                runEvalResultOk ast_mixed `shouldBe` RT.LispFloat 3.5

            it "subtraction" $ do
                let ast_0 = Ast [ListNode (TextRange 7 10) [SymbolNode (TextRange 8 9) "-"]] -- (princ (-))
                let (result, _, _) = runEvalWith RT.emptyEnvironment ast_0
                result `shouldBe` Left (RT.RuntimeError (TextRange 8 9) "Wrong number of arguments: given 0, expected >= 1")
                let ast_i1 = Ast [ListNode s [SymbolNode s "-", IntNode s 2]]
                runEvalResultOk ast_i1 `shouldBe` RT.LispInt (-2)
                let ast_f1 = Ast [ListNode s [SymbolNode s "-", FloatNode s 2.71]]
                runEvalResultOk ast_f1 `shouldBe` RT.LispFloat (-2.71)
                let ast_is = Ast [ListNode s [SymbolNode s "-", IntNode s 1, IntNode s 2, IntNode s 3, IntNode s 4]]
                runEvalResultOk ast_is `shouldBe` RT.LispInt (-8)
                let ast_mixed = Ast [ListNode s [SymbolNode s "-", IntNode s 2, FloatNode s 2.5]]
                runEvalResultOk ast_mixed `shouldBe` RT.LispFloat (-0.5)

            it "multiplication" $ do
                let ast_0 = Ast [ListNode s [SymbolNode s "*"]]
                runEvalResultOk ast_0 `shouldBe` RT.LispInt 1
                let ast_i1 = Ast [ListNode s [SymbolNode s "*", IntNode s 3]]
                runEvalResultOk ast_i1 `shouldBe` RT.LispInt 3
                let ast_f1 = Ast [ListNode s [SymbolNode s "*", FloatNode s 3.14]]
                runEvalResultOk ast_f1 `shouldBe` RT.LispFloat 3.14
                let ast_is = Ast [ListNode s [SymbolNode s "*", IntNode s 1, IntNode s 2, IntNode s 3, IntNode s 4]]
                runEvalResultOk ast_is `shouldBe` RT.LispInt 24
                let ast_mixed = Ast [ListNode s [SymbolNode s "*", IntNode s 2, FloatNode s 2.5]]
                runEvalResultOk ast_mixed `shouldBe` RT.LispFloat 5

            it "division" $ do
                let ast_0 = Ast [ListNode (TextRange 7 10) [SymbolNode (TextRange 8 9) "/"]] -- (princ (/))
                let (result, _, _) = runEvalWith RT.emptyEnvironment ast_0
                result `shouldBe` Left (RT.RuntimeError (TextRange 8 9) "Wrong number of arguments: given 0, expected >= 1")
                let ast_i1 = Ast [ListNode s [SymbolNode s "/", IntNode s 2]]
                runEvalResultOk ast_i1 `shouldBe` RT.LispFloat 0.5
                let ast_f1 = Ast [ListNode s [SymbolNode s "/", FloatNode s 4]]
                runEvalResultOk ast_f1 `shouldBe` RT.LispFloat 0.25
                let ast_is = Ast [ListNode s [SymbolNode s "/", IntNode s 8, IntNode s 2, IntNode s 4]]
                runEvalResultOk ast_is `shouldBe` RT.LispInt 1
                let ast_mixed = Ast [ListNode s [SymbolNode s "/", IntNode s 2, FloatNode s 2.5]]
                runEvalResultOk ast_mixed `shouldBe` RT.LispFloat 0.8

            it "division by zero" $ do
                let div_by_i1_zero = Ast [ListNode s [SymbolNode s "/", IntNode s 0]]
                let (result_i1, _, _) = runEvalWith RT.emptyEnvironment div_by_i1_zero
                result_i1 `shouldBe` Left (RT.RuntimeError s "Division by zero")
                let div_by_f1_zero = Ast [ListNode s [SymbolNode s "/", FloatNode s 0]]
                let (result_f1, _, _) = runEvalWith RT.emptyEnvironment div_by_f1_zero
                result_f1 `shouldBe` Left (RT.RuntimeError s "Division by zero")
                let div_by_i2_zero = Ast [ListNode s [SymbolNode s "/", IntNode s 2, IntNode s 0]]
                let (result_i2, _, _) = runEvalWith RT.emptyEnvironment div_by_i2_zero
                result_i2 `shouldBe` Left (RT.RuntimeError s "Division by zero")
                let div_by_f2_zero = Ast [ListNode s [SymbolNode s "/", FloatNode s 2, FloatNode s 0]]
                let (result_f2, _, _) = runEvalWith RT.emptyEnvironment div_by_f2_zero
                result_f2 `shouldBe` Left (RT.RuntimeError s "Division by zero")

        describe "mod & rem" $ do
            -- Test case is "Example" in https://cl-community-spec.github.io/pages/f_mod.html
            let run a0 a1 = do
                    let ast_mod = Ast [ListNode s [SymbolNode s "mod", a0, a1]]
                    let ast_rem = Ast [ListNode s [SymbolNode s "rem", a0, a1]]
                    let (result_mod, _, _) = runEval ast_mod
                    let (result_rem, _, _) = runEval ast_rem
                    case (result_mod, result_rem) of
                        (Right m, Right r) -> pure (m, r)
                        _ -> fail $ "mod: " ++ show result_mod ++ ", rem: " ++ show result_rem

            it "(mod -1 5) → 4  (rem -1 5) → -1" $ do
                (result_mod, result_rem) <- run (IntNode s (-1)) (IntNode s 5)
                result_mod `shouldBe` RT.LispInt 4
                result_rem `shouldBe` RT.LispInt (-1)

            it "(mod 13 4) → 1  (rem 13 4) → 1" $ do
                (result_mod, result_rem) <- run (IntNode s 13) (IntNode s 4)
                result_mod `shouldBe` RT.LispInt 1
                result_rem `shouldBe` RT.LispInt 1

            it "(mod -13 4) → 3  (rem -13 4) → -1" $ do
                (result_mod, result_rem) <- run (IntNode s (-13)) (IntNode s 4)
                result_mod `shouldBe` RT.LispInt 3
                result_rem `shouldBe` RT.LispInt (-1)

            it "(mod 13 -4) → -3  (rem 13 -4) → 1" $ do
                (result_mod, result_rem) <- run (IntNode s 13) (IntNode s (-4))
                result_mod `shouldBe` RT.LispInt (-3)
                result_rem `shouldBe` RT.LispInt 1

            it "(mod -13 -4) → -1  (rem -13 -4) → -1" $ do
                (result_mod, result_rem) <- run (IntNode s (-13)) (IntNode s (-4))
                result_mod `shouldBe` RT.LispInt (-1)
                result_rem `shouldBe` RT.LispInt (-1)

            it "(mod 13.4 1) → 0.4  (rem 13.4 1) → 0.4" $ do
                (result_mod, result_rem) <- run (FloatNode s 13.4) (IntNode s 1)
                case (result_mod, result_rem) of
                    (RT.LispFloat m, RT.LispFloat r) -> do
                        abs (m - 0.4) < 0.000001 `shouldBe` True
                        abs (r - 0.4) < 0.000001 `shouldBe` True
                    _ -> fail $ "mod: " ++ show result_mod ++ ", rem: " ++ show result_rem

            it "(mod -13.4 1) → 0.6  (rem -13.4 1) → -0.4" $ do
                (result_mod, result_rem) <- run (FloatNode s (-13.4)) (IntNode s 1)
                case (result_mod, result_rem) of
                    (RT.LispFloat m, RT.LispFloat r) -> do
                        abs (m - 0.6) < 0.000001 `shouldBe` True
                        abs (r + 0.4) < 0.000001 `shouldBe` True
                    _ -> fail $ "mod: " ++ show result_mod ++ ", rem: " ++ show result_rem

        describe "setq ok" $ do
            it "no arguments" $ do -- (setq)
                let ast = Ast [ListNode (TextRange 0 6) [SymbolNode (TextRange 1 5) "setq"]]
                let (result, env, _) = runEval ast
                result `shouldBe` Right (RT.LispList [])
                env `shouldBe` RT.emptyEnvironment

            it "set unbound symbol" $ do -- (setq a 42)
                let ast = Ast [ListNode s [SymbolNode s "setq", SymbolNode s "a", IntNode s 42]]
                let (_, env, _) = runEval ast
                RT.lookupValueBinding "a" env.globalBindings `shouldBe` Just (RT.LispInt 42)

            it "set bound symbol" $ do -- (setq a 3.14) ; a is already bound to 3
                let env = RT.emptyEnvironment
                        { RT.globalBindings = RT.insertValueBinding "a" (RT.LispInt 3) RT.emptyEnvironment.globalBindings
                        }
                let ast = Ast [ListNode s [SymbolNode s "setq", SymbolNode s "a", FloatNode s 3.14]]
                let (_, env', _) = runEvalWith env ast
                RT.lookupValueBinding "a" env'.globalBindings `shouldBe` Just (RT.LispFloat 3.14)

            it "set evaluated value" $ do
                let ast = Ast [ListNode s [
                            SymbolNode s "setq", SymbolNode s "a", ListNode s [
                                SymbolNode s "+", IntNode s 1, IntNode s 2]]]
                let (_, env, _) = runEval ast
                RT.lookupValueBinding "a" env.globalBindings `shouldBe` Just (RT.LispInt 3)

            it "set multiple values" $ do -- (setq a 1 b 2 c 3)
                let ast = Ast [ListNode s [
                            SymbolNode s "setq", SymbolNode s "a", IntNode s 1,
                            SymbolNode s "b", IntNode s 2,
                            SymbolNode s "c", IntNode s 3]]
                let (_, env, _) = runEval ast
                RT.lookupValueBinding "a" env.globalBindings `shouldBe` Just (RT.LispInt 1)
                RT.lookupValueBinding "b" env.globalBindings `shouldBe` Just (RT.LispInt 2)
                RT.lookupValueBinding "c" env.globalBindings `shouldBe` Just (RT.LispInt 3)

            it "set multiple value by eval sequential" $ do -- (setq a 1 b (+ a 2))
                let ast = Ast [ListNode s [
                            SymbolNode s "setq", SymbolNode s "a", IntNode s 1,
                            SymbolNode s "b", ListNode s [
                                SymbolNode s "+", SymbolNode s "a", IntNode s 2]]]
                let (_, env, _) = runEval ast
                RT.lookupValueBinding "a" env.globalBindings `shouldBe` Just (RT.LispInt 1)
                RT.lookupValueBinding "b" env.globalBindings `shouldBe` Just (RT.LispInt 3)

            it "lookup scope precedence: lexical > special > global" $ do -- (setq lex 1)
                let env = RT.emptyEnvironment
                        { RT.currentLexicalFrame = RT.insertValueBinding "lex" (RT.LispInt 3) RT.emptyEnvironment.currentLexicalFrame
                        , RT.currentSpecialFrame = RT.insertValueBinding "lex" (RT.LispInt 2) RT.emptyEnvironment.currentSpecialFrame
                        , RT.globalBindings = RT.insertValueBinding "lex" (RT.LispInt 2) RT.emptyEnvironment.globalBindings
                        }
                let ast = Ast [ListNode s [SymbolNode s "setq", SymbolNode s "lex", IntNode s 3]]
                let (_, env', _) = runEvalWith env ast
                RT.lookupValueBinding "lex" env'.globalBindings `shouldBe` Just (RT.LispInt 2)
                RT.lookupValueBinding "lex" env'.currentSpecialFrame `shouldBe` Just (RT.LispInt 2)
                RT.lookupValueBinding "lex" env'.currentLexicalFrame `shouldBe` Just (RT.LispInt 3)

            it "lookup scope precedence: special > global" $ do -- (setq spe 1)
                let env = RT.emptyEnvironment
                        { RT.currentSpecialFrame = RT.insertValueBinding "lex" (RT.LispInt 2) RT.emptyEnvironment.currentSpecialFrame
                        , RT.globalBindings = RT.insertValueBinding "lex" (RT.LispInt 2) RT.emptyEnvironment.globalBindings
                        }
                let ast = Ast [ListNode s [SymbolNode s "setq", SymbolNode s "lex", IntNode s 3]]
                let (_, env', _) = runEvalWith env ast
                RT.lookupValueBinding "lex" env'.globalBindings `shouldBe` Just (RT.LispInt 2)
                RT.lookupValueBinding "lex" env'.currentSpecialFrame `shouldBe` Just (RT.LispInt 3)
                RT.lookupValueBinding "lex" env'.currentLexicalFrame `shouldBe` Nothing

            it "set to parent lexical frame" $ do -- (setq a 1)
                let env = mkEnv RT.emptyEnvironment
                        { RT.currentLexicalFrame = RT.insertValueBinding "a" (RT.LispInt 2) RT.emptyEnvironment.currentLexicalFrame
                        }
                    mkEnv parent = RT.emptyEnvironment
                        { RT.currentLexicalFrame = RT.emptyEnvironment.currentLexicalFrame
                            { RT.parentLexicalFrame = Just parent.currentLexicalFrame }
                        }
                let ast = Ast [ListNode s [SymbolNode s "setq", SymbolNode s "a", IntNode s 1]]
                let (_, env', _) = runEvalWith env ast
                RT.lookupValueBinding "a" env'.currentLexicalFrame `shouldBe` Nothing
                case RT.parentFrame env'.currentLexicalFrame of
                    Just parent -> RT.lookupValueBinding "a" parent `shouldBe` Just (RT.LispInt 1)
                    Nothing -> fail "parentSpecialFrame is Nothing"

        describe "setq error" $ do
            it "one argument" $ do -- (setq 1)
                let ast = Ast [ListNode (TextRange 0 8) [SymbolNode (TextRange 1 4) "setq", IntNode s 1]]
                let (result, _, _) = runEval ast
                result `shouldBe` Left (RT.RuntimeError (TextRange 1 4) "Wrong number of arguments: given 1, expected even number")

            it "three arguments" $ do -- (setq a 1 2)
                let ast = Ast [ListNode (TextRange 0 13) [SymbolNode (TextRange 1 5) "setq", SymbolNode s "a", IntNode s 1, IntNode s 2]]
                let (result, _, _) = runEval ast
                result `shouldBe` Left (RT.RuntimeError (TextRange 1 5) "Wrong number of arguments: given 3, expected even number")

            it "first argument is not symbol" $ do -- (setq 1 2)
                let ast = Ast [ListNode (TextRange 0 10) [SymbolNode (TextRange 1 5) "setq", IntNode s 1, IntNode s 2]]
                let (result, _, _) = runEval ast
                result `shouldBe` Left (RT.RuntimeError (TextRange 1 5) "Variable name is not a symbol")

            it "redefine built-in function" $ do -- (setq t 1)
                let ast = Ast [ListNode (TextRange 0 19) [SymbolNode (TextRange 1 5) "setq", SymbolNode s "t", IntNode s 1]]
                let (result, _, _) = runEval ast
                result `shouldBe` Left (RT.RuntimeError (TextRange 1 5) "Redefining T is not allowed")

        describe "defun ok" $ do
            it "simple function" $ do -- (defun add (a b) (+ a b))
                let ast = Ast [ListNode (TextRange 0 25) [
                            SymbolNode s "defun", SymbolNode s "add",
                                ListNode (TextRange 11 16) [SymbolNode (TextRange 12 13) "a", SymbolNode (TextRange 14 15) "b"],
                                ListNode (TextRange 17 24) [SymbolNode (TextRange 18 19) "+", SymbolNode s "a", SymbolNode s "b"]]]
                let (_, env, _) = runEval ast
                RT.lookupFunctionBinding "add" env.globalBindings
                    `shouldBe`
                    Just (RT.FunctionInfo
                        { RT.functionParams = ["a", "b"]
                        , RT.functionBody = Ast [ListNode (TextRange 17 24) [SymbolNode (TextRange 18 19) "+", SymbolNode s "a", SymbolNode s "b"]]
                        , RT.functionIntialFrame = env.currentLexicalFrame
                        })

            it "empty body" $ do -- (defun empty ())
                let ast = Ast [ListNode s [SymbolNode s "defun", SymbolNode s "empty", ListNode s []]]
                let (_, env, _) = runEval ast
                RT.lookupFunctionBinding "empty" env.globalBindings
                    `shouldBe`
                    Just (RT.FunctionInfo
                        { RT.functionParams = []
                        , RT.functionBody = Ast []
                        , RT.functionIntialFrame = env.currentLexicalFrame
                        })

            it "many bodies" $ do -- (defun many () 1 2 3)
                let ast = Ast [ListNode s [SymbolNode s "defun", SymbolNode s "many", ListNode s [], IntNode s 1, IntNode s 2, IntNode s 3]]
                let (_, env, _) = runEval ast
                RT.lookupFunctionBinding "many" env.globalBindings
                    `shouldBe`
                    Just (RT.FunctionInfo
                        { RT.functionParams = []
                        , RT.functionBody = Ast [IntNode s 1, IntNode s 2, IntNode s 3]
                        , RT.functionIntialFrame = env.currentLexicalFrame
                        })

        describe "defun error" $ do
            it "no arguments" $ do -- (defun)
                let ast = Ast [ListNode (TextRange 0 6) [SymbolNode (TextRange 1 5) "defun"]]
                let (result, _, _) = runEval ast
                result `shouldBe` Left (RT.RuntimeError (TextRange 1 5) "Wrong number of arguments: given 0, expected >= 2")

            it "one argument" $ do -- (defun add)
                let ast = Ast [ListNode (TextRange 0 11) [SymbolNode (TextRange 1 5) "defun", SymbolNode s "add"]]
                let (result, _, _) = runEval ast
                result `shouldBe` Left (RT.RuntimeError (TextRange 1 5) "Wrong number of arguments: given 1, expected >= 2")

            it "invalid function name" $ do -- (defun 1 ())
                let ast = Ast [ListNode (TextRange 0 15) [SymbolNode (TextRange 1 5) "defun", IntNode s 1, ListNode s []]]
                let (result, _, _) = runEval ast
                result `shouldBe` Left (RT.RuntimeError (TextRange 1 5) "Function name is not a symbol")

            it "parameter is not a list" $ do -- (defun add a)
                let ast = Ast [ListNode (TextRange 0 14) [SymbolNode (TextRange 1 5) "defun", SymbolNode s "add", SymbolNode s "a"]]
                let (result, _, _) = runEval ast
                result `shouldBe` Left (RT.RuntimeError (TextRange 1 5) "Function parameters are not a list")

            it "parameter is not a list of symbols" $ do -- (defun add (a 1))
                let ast = Ast [ListNode (TextRange 0 16) [SymbolNode (TextRange 1 5) "defun", SymbolNode s "add", ListNode s [SymbolNode s "a", IntNode s 1]]]
                let (result, _, _) = runEval ast
                result `shouldBe` Left (RT.RuntimeError (TextRange 1 5) "Function parameters are not a list of symbols")

            it "redefine built-in function" $ do -- (defun princ ())
                let ast = Ast [ListNode (TextRange 0 16) [SymbolNode (TextRange 1 6) "defun", SymbolNode s "princ", ListNode s []]]
                let (result, _, _) = runEval ast
                result `shouldBe` Left (RT.RuntimeError (TextRange 1 6) "Redefining PRINC is not allowed")

        describe "defparameter ok" $ do
            it "set evaluated value" $ do -- (defparameter a (+ 1 2))
                let ast = Ast [ListNode s [
                            SymbolNode s "defparameter", SymbolNode s "a", ListNode s [
                                SymbolNode s "+", IntNode s 1, IntNode s 2]]]
                let (_, env, _) = runEval ast
                RT.lookupValueBinding "a" env.currentSpecialFrame `shouldBe` Just (RT.LispInt 3)

            it "set most parent if unbounded" $ do -- (defparameter a 1)
                let env = mkEnv $ mkEnv RT.emptyEnvironment
                    mkEnv parent = RT.emptyEnvironment
                        { RT.currentSpecialFrame = RT.emptyEnvironment.currentSpecialFrame
                            { RT.parentSpecialFrame = Just parent.currentSpecialFrame }
                        }
                let ast = Ast [ListNode s [SymbolNode s "defparameter", SymbolNode s "a", IntNode s 1]]
                let (_, env', _) = runEvalWith env ast
                RT.lookupValueBinding "a" env'.currentSpecialFrame `shouldBe` Nothing
                case RT.parentFrame env'.currentSpecialFrame of
                    Just parent -> do
                        RT.lookupValueBinding "a" parent `shouldBe` Nothing
                        case RT.parentFrame parent of
                            Just grandparent -> RT.lookupValueBinding "a" grandparent `shouldBe` Just (RT.LispInt 1)
                            Nothing -> fail "grandparentSpecialFrame is Nothing"
                    Nothing -> fail "parentSpecialFrame is Nothing"

            it "set parent if bounded" $ do -- (defparameter a 3)
                let env = mkEnv $ mkChild RT.emptyEnvironment
                    mkEnv parent = RT.emptyEnvironment
                        { RT.currentSpecialFrame = RT.emptyEnvironment.currentSpecialFrame
                            { RT.parentSpecialFrame = Just parent.currentSpecialFrame }
                        }
                    mkChild parent = RT.emptyEnvironment
                        { RT.currentSpecialFrame = RT.insertValueBinding "a" (RT.LispInt 1) parent.currentSpecialFrame
                            { RT.parentSpecialFrame = Just parent.currentSpecialFrame }
                        }
                let ast = Ast [ListNode s [SymbolNode s "defparameter", SymbolNode s "a", IntNode s 3]]
                let (_, env', _) = runEvalWith env ast
                RT.lookupValueBinding "a" env'.currentSpecialFrame `shouldBe` Nothing
                case RT.parentFrame env'.currentSpecialFrame of
                    Just parent -> do
                        RT.lookupValueBinding "a" parent `shouldBe` Just (RT.LispInt 3)
                        case RT.parentFrame parent of
                            Just grandparent -> RT.lookupValueBinding "a" grandparent `shouldBe` Nothing
                            Nothing -> fail "grandparentSpecialFrame is Nothing"
                    Nothing -> fail "parentSpecialFrame is Nothing"

        describe "defparameter error" $ do
            it "not 2 arguments" $ do -- (defparameter), (defparameter a), (defparameter a 1 2)
                let ast_0 = Ast [ListNode (TextRange 0 12) [SymbolNode (TextRange 1 12) "defparameter"]]
                let ast_1 = Ast [ListNode (TextRange 0 15) [SymbolNode (TextRange 1 12) "defparameter", SymbolNode s "a"]]
                let ast_3 = Ast [ListNode (TextRange 0 18) [SymbolNode (TextRange 1 12) "defparameter", SymbolNode s "a", IntNode s 1, IntNode s 2]]
                let (result_0, _, _) = runEval ast_0
                let (result_1, _, _) = runEval ast_1
                let (result_3, _, _) = runEval ast_3
                result_0 `shouldBe` Left (RT.RuntimeError (TextRange 1 12) "Wrong number of arguments: given 0, expected 2")
                result_1 `shouldBe` Left (RT.RuntimeError (TextRange 1 12) "Wrong number of arguments: given 1, expected 2")
                result_3 `shouldBe` Left (RT.RuntimeError (TextRange 1 12) "Wrong number of arguments: given 3, expected 2")

            it "invalid parameter name" $ do -- (defparameter 1 2)
                let ast = Ast [ListNode (TextRange 0 17) [SymbolNode (TextRange 1 12) "defparameter", IntNode s 1, IntNode s 2]]
                let (result, _, _) = runEval ast
                result `shouldBe` Left (RT.RuntimeError (TextRange 1 12) "Variable name is not a symbol")

        describe "defvar ok" $ do
            it "only variable name" $ do -- (defparameter), (defparameter a), (defparameter a 1 2)
                let ast = Ast [ListNode s [SymbolNode s "defvar", SymbolNode s "a"]]
                let (_, env, _) = runEval ast
                RT.lookupValueBinding "a" env.currentSpecialFrame `shouldBe` Just (RT.LispList [])

            it "set evaluated value" $ do -- (defparameter a (+ 1 2))
                let ast = Ast [ListNode s [
                            SymbolNode s "defvar", SymbolNode s "a", ListNode s [
                                SymbolNode s "+", IntNode s 1, IntNode s 2]]]
                let (_, env, _) = runEval ast
                RT.lookupValueBinding "a" env.currentSpecialFrame `shouldBe` Just (RT.LispInt 3)

            it "set most parent if unbounded" $ do -- (defparameter a 1)
                let env = mkEnv $ mkEnv RT.emptyEnvironment
                    mkEnv parent = RT.emptyEnvironment
                        { RT.currentSpecialFrame = RT.emptyEnvironment.currentSpecialFrame
                            { RT.parentSpecialFrame = Just parent.currentSpecialFrame }
                        }
                let ast = Ast [ListNode s [SymbolNode s "defvar", SymbolNode s "a", IntNode s 1]]
                let (_, env', _) = runEvalWith env ast
                RT.lookupValueBinding "a" env'.currentSpecialFrame `shouldBe` Nothing
                case RT.parentFrame env'.currentSpecialFrame of
                    Just parent -> do
                        RT.lookupValueBinding "a" parent `shouldBe` Nothing
                        case RT.parentFrame parent of
                            Just grandparent -> RT.lookupValueBinding "a" grandparent `shouldBe` Just (RT.LispInt 1)
                            Nothing -> fail "grandparentSpecialFrame is Nothing"
                    Nothing -> fail "parentSpecialFrame is Nothing"

            it "do nothing if bounded" $ do -- (defparameter a 3)
                let env = RT.emptyEnvironment
                        { RT.currentSpecialFrame = RT.insertValueBinding "a" (RT.LispInt 2) RT.emptyEnvironment.currentSpecialFrame
                        }
                let ast = Ast [ListNode s [SymbolNode s "defvar", SymbolNode s "a", IntNode s 3]]
                let (_, env', _) = runEvalWith env ast
                env `shouldBe` env'

            it "do nothing if bounded in parent" $ do -- (defparameter a 3)
                let env = mkEnv $ mkChild RT.emptyEnvironment
                    mkEnv parent = RT.emptyEnvironment
                        { RT.currentSpecialFrame = RT.insertValueBinding "a" (RT.LispInt 1) parent.currentSpecialFrame
                            { RT.parentSpecialFrame = Just parent.currentSpecialFrame }
                        }
                    mkChild parent = RT.emptyEnvironment
                        { RT.currentSpecialFrame = RT.emptyEnvironment.currentSpecialFrame
                            { RT.parentSpecialFrame = Just parent.currentSpecialFrame }
                        }
                let ast = Ast [ListNode s [SymbolNode s "defvar", SymbolNode s "a", IntNode s 3]]
                let (_, env', _) = runEvalWith env ast
                env `shouldBe` env'

        describe "defvar error" $ do
            it "not 1 or 2 arguments" $ do -- (defvar), (defvar a 1 2)
                let ast_0 = Ast [ListNode (TextRange 0 6) [SymbolNode (TextRange 1 5) "defvar"]]
                let ast_3 = Ast [ListNode (TextRange 0 15) [SymbolNode (TextRange 1 5) "defvar", SymbolNode s "a", IntNode s 1, IntNode s 2]]
                let (result_0, _, _) = runEval ast_0
                let (result_3, _, _) = runEval ast_3
                result_0 `shouldBe` Left (RT.RuntimeError (TextRange 1 5) "Wrong number of arguments: given 0, expected 1 or 2")
                result_3 `shouldBe` Left (RT.RuntimeError (TextRange 1 5) "Wrong number of arguments: given 3, expected 1 or 2")

        describe "function ok" $ do
            it "passed lambda" $ do -- (function (lambda (a b) (+ a b)))
                let ast = Ast [ListNode s [
                            SymbolNode s "function", ListNode s [
                                SymbolNode s "lambda", ListNode s [
                                    SymbolNode s "a", SymbolNode s "b"], ListNode s [
                                    SymbolNode s "+", SymbolNode s "a", SymbolNode s "b"]]]]
                let (result, env, _) = runEval ast
                env `shouldBe` RT.emptyEnvironment
                result `shouldBe` Right (RT.LispFunction RT.FunctionInfo
                        { RT.functionParams = ["a", "b"]
                        , RT.functionBody = Ast [ListNode s [SymbolNode s "+", SymbolNode s "a", SymbolNode s "b"]]
                        , RT.functionIntialFrame = env.currentLexicalFrame
                        })
