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
            [] -> return ""
            (x:xs) -> do
                modify $ \s -> s { testInputs = xs }
                return x

runEval :: Ast -> (Either RT.RuntimeError RT.LispObject, RT.Environment, TestIOState)
runEval ast = (result, env, state)
  where
    ((result, env), state) = runState (runTestIO $ eval RT.emptyEnvironment ast) (TestIOState
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
    describe "system function" $ do
        let s = TextRange 0 0
        describe "princ ok" $ do
            it "integer" $ do
                let ast = Ast [ListNode s [SymbolNode s "princ", IntNode s 42]]
                runEvalOutput ast `shouldBe` "42"

            it "float" $ do
                let ast = Ast [ListNode s [SymbolNode s "princ", FloatNode s 3.14]]
                runEvalOutput ast `shouldBe` "3.14"

            it "string" $ do
                let ast = Ast [ListNode s [SymbolNode s "princ", StringNode s "hello"]]
                runEvalOutput ast `shouldBe` "hello"

            it "list" $ do
                let ast = Ast [ListNode s [
                            SymbolNode s "princ", ListNode s [
                                SymbolNode s "quote", ListNode s [
                                    IntNode s 1, IntNode s 2, IntNode s 3]]]]
                runEvalOutput ast `shouldBe` "(1 2 3)"

            it "nil" $ do
               let nil_ast = Ast [ListNode s [SymbolNode s "princ", SymbolNode s "nil"]]
               runEvalOutput nil_ast `shouldBe` "NIL"
               let lst_ast = Ast [ListNode s [SymbolNode s "princ", ListNode s []]]
               runEvalOutput lst_ast `shouldBe` "NIL"

            it "t" $ do
                let ast = Ast [ListNode s [SymbolNode s "princ", SymbolNode s "t"]]
                runEvalOutput ast `shouldBe` "T"

            it "multiple values" $ do
                let ast = Ast [ ListNode s [SymbolNode s "princ", IntNode s 1]
                              , ListNode s [SymbolNode s "princ", StringNode s "a b"]
                              , ListNode s [SymbolNode s "princ", IntNode s 2]
                              ]
                runEvalOutput ast `shouldBe` "1a b2"

        describe "princ error" $ do
            it "no arguments" $ do
                let ast = Ast [ListNode (TextRange 0 7) [SymbolNode (TextRange 1 6) "princ"]]
                let (result, _, io) = runEval ast
                io.testOutput `shouldBe` ""
                result `shouldBe` Left (RT.RuntimeError (TextRange 1 6) "Invalid number of arguments for PRINC: 0")

            it "too many arguments" $ do
                let ast = Ast [ListNode (TextRange 0 11) [
                                SymbolNode (TextRange 1 6) "princ",
                                IntNode (TextRange 7 8) 1,
                                IntNode (TextRange 9 10) 2]]
                let (result, _, io) = runEval ast
                io.testOutput `shouldBe` ""
                result `shouldBe` Left (RT.RuntimeError (TextRange 1 6) "Invalid number of arguments for PRINC: 2")
