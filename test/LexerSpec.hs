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
                    [ LET
                    , IDEN "K"
                    , EQL
                    , FUN
                    , IDEN "x"
                    , IDEN "y"
                    , ARROW
                    , IDEN "x"
                    , IN
                    , LET
                    , IDEN "S"
                    , EQL
                    , FUN
                    , IDEN "x"
                    , IDEN "y"
                    , IDEN "z"
                    , ARROW
                    , LPAR
                    , IDEN "x"
                    , IDEN "z"
                    , RPAR
                    , LPAR
                    , IDEN "y"
                    , IDEN "z"
                    , RPAR
                    , IN
                    , IDEN "S"
                    , IDEN "K"
                    , IDEN "K"
                    , SMCLMN
                    ]

        it "tokenizes letin and functions nospace" $
            do
                tokenize "let K=fun x y->x in\nlet S=fun x y z -> (x z) (y z) in\nS K K;"
                `shouldBe` Just
                    [ LET
                    , IDEN "K"
                    , EQL
                    , FUN
                    , IDEN "x"
                    , IDEN "y"
                    , ARROW
                    , IDEN "x"
                    , IN
                    , LET
                    , IDEN "S"
                    , EQL
                    , FUN
                    , IDEN "x"
                    , IDEN "y"
                    , IDEN "z"
                    , ARROW
                    , LPAR
                    , IDEN "x"
                    , IDEN "z"
                    , RPAR
                    , LPAR
                    , IDEN "y"
                    , IDEN "z"
                    , RPAR
                    , IN
                    , IDEN "S"
                    , IDEN "K"
                    , IDEN "K"
                    , SMCLMN
                    ]

        it "tokenizes numbers" $
            do
                tokenize "57 2098 12 0934 34"
                    `shouldBe` Just [CONSTANT $ Num 57, CONSTANT $ Num 2098, CONSTANT $ Num 12, CONSTANT $ Num 934, CONSTANT $ Num 34]

        it "tokenizes reserved words and symbols" $ do
            tokenize
                "fun let in if then else ( ) -> ; = true false not and or cond plus minus mul div mod less lesseq greater greatereq eq neq"
                `shouldBe` Just
                    [ FUN
                    , LET
                    , IN
                    , IF
                    , THEN
                    , ELSE
                    , LPAR
                    , RPAR
                    , ARROW
                    , SMCLMN
                    , EQL
                    , CONSTANT $ Bol True
                    , CONSTANT $ Bol False
                    , BUILTIN Not
                    , BUILTIN And
                    , BUILTIN Or
                    , BUILTIN Cond
                    , BUILTIN Plus
                    , BUILTIN Minus
                    , BUILTIN Mul
                    , BUILTIN Div
                    , BUILTIN Mod
                    , BUILTIN Less
                    , BUILTIN LessEq
                    , BUILTIN Greater
                    , BUILTIN GreaterEq
                    , BUILTIN Equal
                    , BUILTIN NEqual
                    ]

        it "tokenizes chars" $
            do
                tokenize "'a' 'b' 'c' 'q'"
                    `shouldBe` Just [CONSTANT $ Chr 'a', CONSTANT $ Chr 'b', CONSTANT $ Chr 'c', CONSTANT $ Chr 'q']
                tokenize " '\"' '\\'' '\\n' '\\\\'"
                    `shouldBe` Just [CONSTANT $ Chr '"', CONSTANT $ Chr '\'', CONSTANT $ Chr '\n', CONSTANT $ Chr '\\']
                tokenize "'ab'"
                    `shouldBe` Nothing
                tokenize "';kj'"
                    `shouldBe` Nothing

        it "tokenizes strings" $
            do
                tokenize "\"asdf\" \"jkl;\" \"qwerty\""
                    `shouldBe` Just [STRING "asdf", STRING "jkl;", STRING "qwerty"]
                tokenize "\"as\\\"df\" \"jkl;\" \"qw\\\"erty\""
                    `shouldBe` Just [STRING "as\"df", STRING "jkl;", STRING "qw\"erty"]
                tokenize "\"as\\ndf\" \"jkl;\" \"qw\\nerty\""
                    `shouldBe` Just [STRING "as\ndf", STRING "jkl;", STRING "qw\nerty"]
                tokenize "\"asdf\" \"jkl;\\\\\" \"qwerty\""
                    `shouldBe` Just [STRING "asdf", STRING "jkl;\\", STRING "qwerty"]

    describe "wordStringsAndChars" $ do
        it "does not change program without strings and chars" $ do
            wordStringsAndChars "" `shouldBe` Just [""]
            wordStringsAndChars "asdfasdf" `shouldBe` Just ["asdfasdf"]
            wordStringsAndChars "asdfas asdlkfj a;ldka asdf !@#$ asdf @#$%@#$ _)*&)(&"
                `shouldBe` Just ["asdfas asdlkfj a;ldka asdf !@#$ asdf @#$%@#$ _)*&)(&"]

        it "words empty string" $ do
            wordStringsAndChars "\"\"" `shouldBe` Just ["\"\""]
            wordStringsAndChars "asdf\"\"" `shouldBe` Just ["asdf", "\"\""]
            wordStringsAndChars "\"\"asdf" `shouldBe` Just ["\"\"", "asdf"]
            wordStringsAndChars "asdf\"\"asdf" `shouldBe` Just ["asdf", "\"\"", "asdf"]

        it "words some string" $ do
            wordStringsAndChars "\"jkl;\"" `shouldBe` Just ["\"jkl;\""]
            wordStringsAndChars "asdf\"jkl;\"" `shouldBe` Just ["asdf", "\"jkl;\""]
            wordStringsAndChars "\"jkl;\"asdf" `shouldBe` Just ["\"jkl;\"", "asdf"]
            wordStringsAndChars "asdf\"jkl;\"asdf" `shouldBe` Just ["asdf", "\"jkl;\"", "asdf"]

        it "words multiple strings" $ do
            wordStringsAndChars "\"jkl;\"\"xxxx\"" `shouldBe` Just ["\"jkl;\"", "\"xxxx\""]
            wordStringsAndChars "\"xxxx\"asdf\"jkl;\"" `shouldBe` Just ["\"xxxx\"", "asdf", "\"jkl;\""]
            wordStringsAndChars "abobus\"xxxx\"asdf\"jkl;\"" `shouldBe` Just ["abobus", "\"xxxx\"", "asdf", "\"jkl;\""]
            wordStringsAndChars "abobus\"xxxx\"asdf\"jkl;\"bebra"
                `shouldBe` Just ["abobus", "\"xxxx\"", "asdf", "\"jkl;\"", "bebra"]
            wordStringsAndChars "abobus\"xxxx\"\"jkl;\"bebra" `shouldBe` Just ["abobus", "\"xxxx\"", "\"jkl;\"", "bebra"]
            wordStringsAndChars "\"lazy\"\"man\"abobus\"xxxx\"asdf\"jkl;\"bebra\"end\""
                `shouldBe` Just ["\"lazy\"", "\"man\"", "abobus", "\"xxxx\"", "asdf", "\"jkl;\"", "bebra", "\"end\""]

        it "words string with \\\"" $ do
            wordStringsAndChars "\"j\\\"kl;\"" `shouldBe` Just ["\"j\\\"kl;\""]
            wordStringsAndChars "asdf\"j\\\"kl;\"" `shouldBe` Just ["asdf", "\"j\\\"kl;\""]
            wordStringsAndChars "\"j\\\"kl;\"asdf" `shouldBe` Just ["\"j\\\"kl;\"", "asdf"]
            wordStringsAndChars "asdf\"j\\\"kl;\"asdf" `shouldBe` Just ["asdf", "\"j\\\"kl;\"", "asdf"]

        it "words string with '" $ do
            wordStringsAndChars "\"j'kl;\"" `shouldBe` Just ["\"j'kl;\""]
            wordStringsAndChars "asdf\"j'kl;\"" `shouldBe` Just ["asdf", "\"j'kl;\""]
            wordStringsAndChars "\"j'kl;\"asdf" `shouldBe` Just ["\"j'kl;\"", "asdf"]
            wordStringsAndChars "asdf\"j'kl;\"asdf" `shouldBe` Just ["asdf", "\"j'kl;\"", "asdf"]

        it "words empty char" $ do
            wordStringsAndChars "''" `shouldBe` Just ["''"]
            wordStringsAndChars "asdf''" `shouldBe` Just ["asdf", "''"]
            wordStringsAndChars "''asdf" `shouldBe` Just ["''", "asdf"]
            wordStringsAndChars "asdf''asdf" `shouldBe` Just ["asdf", "''", "asdf"]

        it "words some string" $ do
            wordStringsAndChars "'jkl;'" `shouldBe` Just ["'jkl;'"]
            wordStringsAndChars "asdf'jkl;'" `shouldBe` Just ["asdf", "'jkl;'"]
            wordStringsAndChars "'jkl;'asdf" `shouldBe` Just ["'jkl;'", "asdf"]
            wordStringsAndChars "asdf'jkl;'asdf" `shouldBe` Just ["asdf", "'jkl;'", "asdf"]

        it "words multiple strings" $ do
            wordStringsAndChars "'jkl;''xxxx'" `shouldBe` Just ["'jkl;'", "'xxxx'"]
            wordStringsAndChars "'xxxx'asdf'jkl;'" `shouldBe` Just ["'xxxx'", "asdf", "'jkl;'"]
            wordStringsAndChars "abobus'xxxx'asdf'jkl;'" `shouldBe` Just ["abobus", "'xxxx'", "asdf", "'jkl;'"]
            wordStringsAndChars "abobus'xxxx'asdf'jkl;'bebra" `shouldBe` Just ["abobus", "'xxxx'", "asdf", "'jkl;'", "bebra"]
            wordStringsAndChars "abobus'xxxx''jkl;'bebra" `shouldBe` Just ["abobus", "'xxxx'", "'jkl;'", "bebra"]
            wordStringsAndChars "'lazy''man'abobus'xxxx'asdf'jkl;'bebra'end'"
                `shouldBe` Just ["'lazy'", "'man'", "abobus", "'xxxx'", "asdf", "'jkl;'", "bebra", "'end'"]

        it "words chars with \\'" $ do
            wordStringsAndChars "'j\\'kl;'" `shouldBe` Just ["'j\\'kl;'"]
            wordStringsAndChars "asdf'j\\'kl;'" `shouldBe` Just ["asdf", "'j\\'kl;'"]
            wordStringsAndChars "'j\\'kl;'asdf" `shouldBe` Just ["'j\\'kl;'", "asdf"]
            wordStringsAndChars "asdf'j\\'kl;'asdf" `shouldBe` Just ["asdf", "'j\\'kl;'", "asdf"]

        it "words chars with \"" $ do
            wordStringsAndChars "'j\"kl;'" `shouldBe` Just ["'j\"kl;'"]
            wordStringsAndChars "asdf'j\"kl;'" `shouldBe` Just ["asdf", "'j\"kl;'"]
            wordStringsAndChars "'j\"kl;'asdf" `shouldBe` Just ["'j\"kl;'", "asdf"]
            wordStringsAndChars "asdf'j\"kl;'asdf" `shouldBe` Just ["asdf", "'j\"kl;'", "asdf"]

        it "words chars and strings" $ do
            wordStringsAndChars "'jkl;'\"xxxx\"" `shouldBe` Just ["'jkl;'", "\"xxxx\""]
            wordStringsAndChars "\"xxxx\"'jkl;'" `shouldBe` Just ["\"xxxx\"", "'jkl;'"]
            wordStringsAndChars "'jkl;'asdf\"xxxx\"" `shouldBe` Just ["'jkl;'", "asdf", "\"xxxx\""]
            wordStringsAndChars "\"xxxx\"asdf'jkl;'" `shouldBe` Just ["\"xxxx\"", "asdf", "'jkl;'"]
            wordStringsAndChars "'lazy'\"man\"abobus'xxxx'asdf\"jkl;\"bebra'end'"
                `shouldBe` Just ["'lazy'", "\"man\"", "abobus", "'xxxx'", "asdf", "\"jkl;\"", "bebra", "'end'"]
