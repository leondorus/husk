module ParserSpec (spec) where

import Test.Hspec

import Ast
import Lexer
import Parser
import Token

kIden = Iden "K"
kComb = Fun [Iden "x", Iden "y"] (cExIden "x")
appKK = App (cExIden "K") (cExIden "K")

sIden = Iden "S"
sComb = Fun [Iden "x", Iden "y", Iden "z"] (App (App (cExIden "x") (cExIden "z")) (App (cExIden "y") (cExIden "z")))
appSKK = App (App (cExIden "S") (cExIden "K")) (cExIden "K")

spec :: Spec
spec = do
    describe "ast parser" $ do
        it "parses single fun" $ do
            runParse ast [FUN, IDEN "x", IDEN "y", ARROW, IDEN "x", SMCLMN]
                `shouldBe` Just (Ast [] $ Expr kComb)

        it "parses statement and fun" $ do
            runParse ast [LET, IDEN "K", EQL, FUN, IDEN "x", IDEN "y", ARROW, IDEN "x", SMCLMN, FUN, IDEN "x", IDEN "y", ARROW, IDEN "x", SMCLMN]
                `shouldBe` Just (Ast [Let kIden kComb] $ Expr kComb)

        it "parses single letin" $ do
            runParse ast [LET, IDEN "K", EQL, FUN, IDEN "x", IDEN "y", ARROW, IDEN "x", IN, LPAR, IDEN "K", IDEN "K", RPAR, SMCLMN]
                `shouldBe` Just
                    ( Ast
                        []
                        $ Expr
                        $ LetIn kIden kComb appKK
                    )

        it "parses let and app" $ do
            runParse ast [LET, IDEN "K", EQL, FUN, IDEN "x", IDEN "y", ARROW, IDEN "x", SMCLMN, LPAR, IDEN "K", IDEN "K", RPAR, SMCLMN]
                `shouldBe` Just
                    ( Ast
                        [ Let kIden kComb -- let K = fun x y = x;
                        ]
                        $ Expr appKK
                    )

        it "parses let K, let S, and SKK" $ do
            runParse ast [LET, IDEN "K", EQL, FUN, IDEN "x", IDEN "y", ARROW, IDEN "x", SMCLMN, LET, IDEN "S", EQL, FUN, IDEN "x", IDEN "y", IDEN "z", ARROW, LPAR, LPAR, IDEN "x", IDEN "z", RPAR, LPAR, IDEN "y", IDEN "z", RPAR, RPAR, SMCLMN, LPAR, LPAR, IDEN "S", IDEN "K", RPAR, IDEN "K", RPAR, SMCLMN]
                `shouldBe` Just
                    ( Ast
                        [ Let kIden kComb
                        , Let sIden sComb
                        ]
                        $ Expr appSKK
                    )

        it "parses plus operator right" $ do
            runParse ast [CONSTANT $ Numb 54, OPERATOR Plus, CONSTANT $ Numb 1024, SMCLMN] `shouldBe` Just (Ast [] $ Expr (App (App (BuiltIn Plus) (Constant $ Numb 54)) (Constant $ Numb 1024)))
            runParse ast [IDEN "K", CONSTANT $ Numb 53, CONSTANT $ Numb 54, OPERATOR Plus, CONSTANT $ Numb 1024, SMCLMN] `shouldBe` Just (Ast [] $ Expr (App (App (BuiltIn Plus) (App (App (cExIden "K") (Constant $ Numb 53)) (Constant $ Numb 54))) (Constant $ Numb 1024)))

        it "parses implicit parenthesis right" $ do
            runParse ast [IDEN "S", IDEN "K", IDEN "K", SMCLMN] `shouldBe` Just (Ast [] $ Expr appSKK)
            runParse ast [IDEN "S", IDEN "K", IDEN "K", IDEN "K", SMCLMN]
                `shouldBe` Just (Ast [] $ Expr (App appSKK (cExIden "K")))

        it "parses numbers" $ do
            runParse ast [IDEN "plus", CONSTANT $ Numb 54, CONSTANT $ Numb 1024, SMCLMN]
                `shouldBe` Just (Ast [] $ Expr (App (App (cExIden "plus") (Constant $ Numb 54)) (Constant $ Numb 1024)))

        it "does not parse invalid programs" $ do
            runParse ast [LET, IDEN "S", EQL, SMCLMN] `shouldBe` Nothing
            runParse ast [LET, IDEN "S", EQL, LPAR, IDEN "K", SMCLMN] `shouldBe` Nothing
