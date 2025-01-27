{-# LANGUAGE OverloadedStrings #-}

module EvaluatorSpec (spec) where

import           Control.Monad.State (State, modify, runState)
import           Test.Hspec
import           ToyLisp.Evaluator
import           ToyLisp.Runtime
import           ToyLisp.Syntax

newtype TestIO a = TestIO { runTestIO :: State String a }
    deriving (Functor, Applicative, Monad)

instance EvalIO TestIO where
    writeOutput str = TestIO $ modify (++ str)
    writeError _ = error "Unexpected error output"

runEval :: Ast -> (Either RuntimeError Environment, String)
runEval ast = runState (runTestIO $ eval ast) ""

runEvalOutput :: Ast -> String
runEvalOutput ast = snd (runEval ast)

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

            -- it "list" $ do
            --     let ast = Ast [ListNode s [
            --                 SymbolNode s "princ", ListNode s [
            --                     SymbolNode s "quote", ListNode s [
            --                         IntNode s 1, IntNode s 2, IntNode s 3]]]]
            --     runEvalOutput ast `shouldBe` "(1 2 3)"

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
                let (result, output) = runEval ast
                output `shouldBe` ""
                result `shouldBe` Left (RuntimeError (TextRange 1 6) "Invalid number of arguments for PRINC: 0")

            it "too many arguments" $ do
                let ast = Ast [ListNode (TextRange 0 11) [
                                SymbolNode (TextRange 1 6) "princ",
                                IntNode (TextRange 7 8) 1,
                                IntNode (TextRange 9 10) 2]]
                let (ret, output) = runEval ast
                output `shouldBe` ""
                ret `shouldBe` Left (RuntimeError (TextRange 1 6) "Invalid number of arguments for PRINC: 2")
