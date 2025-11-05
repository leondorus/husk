module LexerSpec (spec) where

import Ast (BuiltIn (..), Constant (..))
import Lexer
import Test.Hspec
import Token

spec :: Spec
spec = do
    describe "uncomment" $ do
        it "does not change code without comment" $ do
            uncomment "asdf asdf jkl; jkl;\naaa bbb\ncccccc\n" `shouldBe` "asdf asdf jkl; jkl;\naaa bbb\ncccccc\n"

        it "deletes comment till newline" $ do
            uncomment "asdf #wasd \njkl;" `shouldBe` "asdf \njkl;"
            uncomment "asdf #wasd \njkl; #wasd \nasdf" `shouldBe` "asdf \njkl; \nasdf"

        it "ignores nested comments" $ do
            uncomment "asdf #wa#sd# \njkl;" `shouldBe` "asdf \njkl;"

        it "ends with line end" $ do
            uncomment "asdf #wasd\njkl; #wasd" `shouldBe` "asdf \njkl; "

    describe "tokenize" $ do
        it "tokenizes one function" $ do
            tokenize "fun x y -> x ;" `shouldBe` Just [FUN, IDEN "x", IDEN "y", ARROW, IDEN "x", SMCLMN]
        it "tokenizes one function nospace" $ do
            tokenize "fun x y -> x;" `shouldBe` Just [FUN, IDEN "x", IDEN "y", ARROW, IDEN "x", SMCLMN]
        it "tokenizes letin and functions" $
            do
                tokenize "let K = fun x y -> x in\nlet S = fun x y z -> ( x z ) ( y z ) in\nS K K ;"
                `shouldBe` Just
                    [LET, IDEN "K", EQL, FUN, IDEN "x", IDEN "y", ARROW, IDEN "x", IN, LET, IDEN "S", EQL, FUN, IDEN "x", IDEN "y", IDEN "z", ARROW, LPAR, IDEN "x", IDEN "z", RPAR, LPAR, IDEN "y", IDEN "z", RPAR, IN, IDEN "S", IDEN "K", IDEN "K", SMCLMN]

        it "tokenizes letin and functions nospace" $
            do
                tokenize "let K=fun x y->x in\nlet S=fun x y z -> (x z) (y z) in\nS K K;"
                `shouldBe` Just
                    [LET, IDEN "K", EQL, FUN, IDEN "x", IDEN "y", ARROW, IDEN "x", IN, LET, IDEN "S", EQL, FUN, IDEN "x", IDEN "y", IDEN "z", ARROW, LPAR, IDEN "x", IDEN "z", RPAR, LPAR, IDEN "y", IDEN "z", RPAR, IN, IDEN "S", IDEN "K", IDEN "K", SMCLMN]

        it "tokenizes numbers" $
            do
                tokenize "57 2098 12 0934 34" `shouldBe` Just [CONSTANT $ Numb 57, CONSTANT $ Numb 2098, CONSTANT $ Numb 12, CONSTANT $ Numb 934, CONSTANT $ Numb 34]

        it "tokenizes reserved words and symbols" $ do
            tokenize "fun let in if then else ( ) -> ; = true false not and or cond plus minus mul div mod less lesseq greater greatereq eq neq" `shouldBe` Just [FUN, LET, IN, IF, THEN, ELSE, LPAR, RPAR, ARROW, SMCLMN, EQL, CONSTANT $ Bol True, CONSTANT $ Bol False, BUILTIN Not, BUILTIN And, BUILTIN Or, BUILTIN Cond, BUILTIN Plus, BUILTIN Minus, BUILTIN Mul, BUILTIN Div, BUILTIN Mod, BUILTIN Less, BUILTIN LessEq, BUILTIN Greater, BUILTIN GreaterEq, BUILTIN Equal, BUILTIN NEqual]
