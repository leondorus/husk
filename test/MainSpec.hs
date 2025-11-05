module MainSpec (spec) where

import Combined
import Test.Hspec

fibDef =
    "let fib x -> \
    \              if less x 2 then \
    \                  x \
    \              else \
    \                  plus (fib (minus x 1)) (fib (minus x 2)) "
fibOpDef = readFile "./test/res/def/fib.hu"
oddEvenDef = readFile "./test/res/def/odd-even.hu"
isPrimeDef = readFile "./test/res/def/is-prime.hu"
listLengthDef = readFile "./test/res/def/list-len.hu"
mergeDef = readFile "./test/res/def/merge.hu"
mergeSortDef = readFile "./test/res/def/mergesort.hu"

spec :: Spec
spec = do
    describe "simple stuff" $ do
        it "evaluates simple combinators" $ do
            evaluateString "let I = fun x -> x; I 17;" `shouldBe` "17\n"
            evaluateString "let K = fun x y -> x; let S = fun x y z -> (x z) (y z); K 1 2;" `shouldBe` "1\n"
            evaluateString "let K = fun x y -> x; let S = fun x y z -> (x z) (y z); S K K 7;" `shouldBe` "7\n"

            evaluateString "let I x -> x; I 17;" `shouldBe` "17\n"
            evaluateString "let K x y -> x; let S x y z -> (x z) (y z); K 1 2;" `shouldBe` "1\n"
            evaluateString "let K x y -> x; let S x y z -> (x z) (y z); S K K 7;" `shouldBe` "7\n"

        it "works with builtin function" $ do
            -- Arithmetic
            evaluateString "plus 7 5;" `shouldBe` "12\n"
            evaluateString "plus 5 7;" `shouldBe` "12\n"
            evaluateString "minus 7 5;" `shouldBe` "2\n"
            evaluateString "minus 5 7;" `shouldBe` "-2\n"
            evaluateString "mul 7 5;" `shouldBe` "35\n"
            evaluateString "mul 5 7;" `shouldBe` "35\n"
            evaluateString "div 100 5;" `shouldBe` "20\n"
            evaluateString "div 103 5;" `shouldBe` "20\n"
            evaluateString "mod 100 5;" `shouldBe` "0\n"
            evaluateString "mod 104 5;" `shouldBe` "4\n"
            -- Arithmetic Operators
            evaluateString "+ 5;" `shouldBe` "5\n"
            evaluateString "- 5;" `shouldBe` "-5\n"
            evaluateString "+5;" `shouldBe` "5\n"
            evaluateString "-5;" `shouldBe` "-5\n"
            evaluateString "7 + 5;" `shouldBe` "12\n"
            evaluateString "7 + 5;" `shouldBe` "12\n"
            evaluateString "5 + 7;" `shouldBe` "12\n"
            evaluateString "7 - 5;" `shouldBe` "2\n"
            evaluateString "5 - 7;" `shouldBe` "-2\n"
            evaluateString "7 * 5;" `shouldBe` "35\n"
            evaluateString "5 * 7;" `shouldBe` "35\n"
            evaluateString "100 / 5;" `shouldBe` "20\n"
            evaluateString "103 / 5;" `shouldBe` "20\n"
            evaluateString "100 % 5;" `shouldBe` "0\n"
            evaluateString "104 % 5;" `shouldBe` "4\n"
            -- Comparison of numbers
            evaluateString "less 0 7;" `shouldBe` "true\n"
            evaluateString "less 7 0;" `shouldBe` "false\n"
            evaluateString "less 170 170;" `shouldBe` "false\n"
            evaluateString "lesseq 0 7;" `shouldBe` "true\n"
            evaluateString "lesseq 7 0;" `shouldBe` "false\n"
            evaluateString "lesseq 170 170;" `shouldBe` "true\n"
            evaluateString "greater 0 7;" `shouldBe` "false\n"
            evaluateString "greater 7 0;" `shouldBe` "true\n"
            evaluateString "greater 170 170;" `shouldBe` "false\n"
            evaluateString "greatereq 0 7;" `shouldBe` "false\n"
            evaluateString "greatereq 7 0;" `shouldBe` "true\n"
            evaluateString "greatereq 170 170;" `shouldBe` "true\n"
            evaluateString "eq 17 17;" `shouldBe` "true\n"
            evaluateString "eq 17 16;" `shouldBe` "false\n"
            evaluateString "neq 17 17;" `shouldBe` "false\n"
            evaluateString "neq 17 16;" `shouldBe` "true\n"
            -- Comparison by operators
            evaluateString "0 < 7;" `shouldBe` "true\n"
            evaluateString "7 < 0;" `shouldBe` "false\n"
            evaluateString "170 < 170;" `shouldBe` "false\n"
            evaluateString "0 <= 7;" `shouldBe` "true\n"
            evaluateString "7 <= 0;" `shouldBe` "false\n"
            evaluateString "170 <= 170;" `shouldBe` "true\n"
            evaluateString "0 > 7;" `shouldBe` "false\n"
            evaluateString "7 > 0;" `shouldBe` "true\n"
            evaluateString "170 > 170;" `shouldBe` "false\n"
            evaluateString "0 >= 7;" `shouldBe` "false\n"
            evaluateString "7 >= 0;" `shouldBe` "true\n"
            evaluateString "170 >= 170;" `shouldBe` "true\n"
            evaluateString "17 == 17;" `shouldBe` "true\n"
            evaluateString "17 == 16;" `shouldBe` "false\n"
            evaluateString "17 != 17;" `shouldBe` "false\n"
            evaluateString "17 != 16;" `shouldBe` "true\n"
            -- Boolean
            evaluateString "not true;" `shouldBe` "false\n"
            evaluateString "not false;" `shouldBe` "true\n"
            evaluateString "and false false;" `shouldBe` "false\n"
            evaluateString "and true false;" `shouldBe` "false\n"
            evaluateString "and false true;" `shouldBe` "false\n"
            evaluateString "and true true;" `shouldBe` "true\n"
            evaluateString "or false false;" `shouldBe` "false\n"
            evaluateString "or true false;" `shouldBe` "true\n"
            evaluateString "or false true;" `shouldBe` "true\n"
            evaluateString "or true true;" `shouldBe` "true\n"
            evaluateString "cond true 0 1;" `shouldBe` "0\n"
            evaluateString "cond false 0 1;" `shouldBe` "1\n"
            -- Boolean operators
            evaluateString "! true;" `shouldBe` "false\n"
            evaluateString "! false;" `shouldBe` "true\n"
            evaluateString "!true;" `shouldBe` "false\n"
            evaluateString "!false;" `shouldBe` "true\n"
            evaluateString "false && false;" `shouldBe` "false\n"
            evaluateString "true && false;" `shouldBe` "false\n"
            evaluateString "false && true;" `shouldBe` "false\n"
            evaluateString "true && true;" `shouldBe` "true\n"
            evaluateString "false || false;" `shouldBe` "false\n"
            evaluateString "true || false;" `shouldBe` "true\n"
            evaluateString "false || true;" `shouldBe` "true\n"
            evaluateString "true || true;" `shouldBe` "true\n"

        it "calculates expressions with operators right" $ do
            evaluateString "53 + 1023;" `shouldBe` "1076\n"
            evaluateString "53 + 1023 - 71;" `shouldBe` "1005\n"
            evaluateString "6 + 2 * 3;" `shouldBe` "12\n"
            evaluateString "6 + 55 / 5;" `shouldBe` "17\n"
            evaluateString "6 + plus 1 3 * 3;" `shouldBe` "18\n"
            evaluateString "(150 * 3 + 60 / 4 - 25) % 7 * 20 + (minus 80 3 * 3);" `shouldBe` "351\n"
            evaluateString "false && true || false;" `shouldBe` "false\n"
            evaluateString "5 > 6 == 5 <= 6;" `shouldBe` "false\n"
            evaluateString "6 + 2 * 3 != plus 6 2 * 3;" `shouldBe` "true\n"
            evaluateString "10 / 2 % 3 == plus 0 1 == true;" `shouldBe` "false\n"
            evaluateString " 5 > 6 == 5 <= 6 && 6 + 2 * 3 != plus 6 2 * 3 || 10 / 2 % 3 == plus 0 1 == true;" `shouldBe` "false\n"

        it "works with letin" $ do
            evaluateString "let plusFive = fun x -> plus 5 x; let y = plusFive 8 in let z = plusFive 1 in plus z y;" `shouldBe` "19\n"
            evaluateString "let plusFive = fun x -> 5 + x; let y = plusFive 8 in let z = plusFive 1 in z + y;" `shouldBe` "19\n"
            evaluateString "let plusFive = fun x -> let y = plus x 2 in plus y 3; plusFive 7;" `shouldBe` "12\n"
            evaluateString "let plusFive = fun x -> let y = x + 2 in y + 3; plusFive 7;" `shouldBe` "12\n"

        it "does nested ifs right" $ do
            evaluateString "if true then if true then 0 else 1 else 2;" `shouldBe` "0\n"
            evaluateString "if true then if false then 0 else 1 else 2;" `shouldBe` "1\n"
            evaluateString "if false then if false then 0 else 1 else 2;" `shouldBe` "2\n"
            evaluateString "if false then if false then 0 else 1 else 2;" `shouldBe` "2\n"

            evaluateString "if true then 0 else if true then 1 else 2;" `shouldBe` "0\n"
            evaluateString "if true then 0 else if false then 1 else 2;" `shouldBe` "0\n"
            evaluateString "if false then 0 else if true then 1 else 2;" `shouldBe` "1\n"
            evaluateString "if false then 0 else if false then 1 else 2;" `shouldBe` "2\n"

        it "works with lists" $ do
            evaluateString "hd [1, 2, 3, 4, 5];" `shouldBe` "1\n"
            evaluateString "hd [1, 2, 3, 4, 5,];" `shouldBe` "1\n"
            evaluateString "tl [1];" `shouldBe` "nil\n"
            evaluateString "tl [2];" `shouldBe` "nil\n"
            evaluateString "tl (tl (tl (tl (tl [1, 2, 3, 4, 5]))));" `shouldBe` "nil\n"

    describe "some programs" $ do
        it "does not evaluate not needed arguments" $ do
            evaluateString "let loop = fun x -> loop x; let K = fun x y -> x; K 5 (loop 7);" `shouldBe` "5\n"
            evaluateString "let loop = fun x -> loop x; let K = fun x y -> y; K (loop 7) 5;" `shouldBe` "5\n"
            evaluateString "let loop = fun x -> loop x; cond true 5 (loop 7);" `shouldBe` "5\n"
            evaluateString "let loop = fun x -> loop x; cond false (loop 7) 5;" `shouldBe` "5\n"

        it "evaluates fibonacci numbers" $ do
            evaluateString (fibDef ++ " in fib 20;") `shouldBe` "6765\n"
            evaluateString (fibDef ++ " in fib 21;") `shouldBe` "10946\n"
            evaluateString (fibDef ++ " in fib 22;") `shouldBe` "17711\n"

            evaluateString (fibDef ++ "; fib 0;") `shouldBe` "0\n"
            evaluateString (fibDef ++ "; fib 1;") `shouldBe` "1\n"
            evaluateString (fibDef ++ "; fib 2;") `shouldBe` "1\n"
            evaluateString (fibDef ++ "; fib 3;") `shouldBe` "2\n"
            evaluateString (fibDef ++ "; fib 4;") `shouldBe` "3\n"
            evaluateString (fibDef ++ "; fib 5;") `shouldBe` "5\n"
            evaluateString (fibDef ++ "; fib 6;") `shouldBe` "8\n"
            evaluateString (fibDef ++ "; fib 7;") `shouldBe` "13\n"
            evaluateString (fibDef ++ "; fib 8;") `shouldBe` "21\n"
            evaluateString (fibDef ++ "; fib 20;") `shouldBe` "6765\n"
            evaluateString (fibDef ++ "; fib 21;") `shouldBe` "10946\n"
            evaluateString (fibDef ++ "; fib 22;") `shouldBe` "17711\n"

        it "evaluate fibonnaci numbers with operators" $ do
            evaluateString . (++ "fib 0;") <$> fibOpDef `shouldReturn` "0\n"
            evaluateString . (++ "fib 1;") <$> fibOpDef `shouldReturn` "1\n"
            evaluateString . (++ "fib 2;") <$> fibOpDef `shouldReturn` "1\n"
            evaluateString . (++ "fib 3;") <$> fibOpDef `shouldReturn` "2\n"
            evaluateString . (++ "fib 4;") <$> fibOpDef `shouldReturn` "3\n"
            evaluateString . (++ "fib 5;") <$> fibOpDef `shouldReturn` "5\n"
            evaluateString . (++ "fib 6;") <$> fibOpDef `shouldReturn` "8\n"
            evaluateString . (++ "fib 7;") <$> fibOpDef `shouldReturn` "13\n"
            evaluateString . (++ "fib 8;") <$> fibOpDef `shouldReturn` "21\n"
            evaluateString . (++ "fib 20;") <$> fibOpDef `shouldReturn` "6765\n"
            evaluateString . (++ "fib 21;") <$> fibOpDef `shouldReturn` "10946\n"
            evaluateString . (++ "fib 22;") <$> fibOpDef `shouldReturn` "17711\n"

        it "evaluates odd and even numbers" $ do
            evaluateString . (++ " odd 0;") <$> oddEvenDef `shouldReturn` "false\n"
            evaluateString . (++ " even 0;") <$> oddEvenDef `shouldReturn` "true\n"
            evaluateString . (++ " odd 1;") <$> oddEvenDef `shouldReturn` "true\n"
            evaluateString . (++ " even 1;") <$> oddEvenDef `shouldReturn` "false\n"
            evaluateString . (++ " odd 2;") <$> oddEvenDef `shouldReturn` "false\n"
            evaluateString . (++ " even 2;") <$> oddEvenDef `shouldReturn` "true\n"
            evaluateString . (++ " odd 200;") <$> oddEvenDef `shouldReturn` "false\n"
            evaluateString . (++ " even 200;") <$> oddEvenDef `shouldReturn` "true\n"
            evaluateString . (++ " odd 201;") <$> oddEvenDef `shouldReturn` "true\n"
            evaluateString . (++ " even 201;") <$> oddEvenDef `shouldReturn` "false\n"
            evaluateString . (++ " odd 202;") <$> oddEvenDef `shouldReturn` "false\n"
            evaluateString . (++ " even 202;") <$> oddEvenDef `shouldReturn` "true\n"

            evaluateString "let even = fun x -> cond (eq x 0) true (odd (minus x 1)); let odd = fun x -> mnot (even x); let mnot = fun x -> cond x false true; odd 1001;" `shouldBe` "true\n"
            evaluateString "let even = fun x -> cond (eq x 0) true (odd (minus x 1)); let odd = fun x -> mnot (even x); let mnot = fun x -> cond x false true; even 1001;" `shouldBe` "false\n"

        it "evaluates prime numbers" $ do
            evaluateString . (++ "isprime 2;") <$> isPrimeDef `shouldReturn` "true\n"
            evaluateString . (++ "isprime 3;") <$> isPrimeDef `shouldReturn` "true\n"
            evaluateString . (++ "isprime 4;") <$> isPrimeDef `shouldReturn` "false\n"
            evaluateString . (++ "isprime 5;") <$> isPrimeDef `shouldReturn` "true\n"
            evaluateString . (++ "isprime 6;") <$> isPrimeDef `shouldReturn` "false\n"
            evaluateString . (++ "isprime 7;") <$> isPrimeDef `shouldReturn` "true\n"
            evaluateString . (++ "isprime 8;") <$> isPrimeDef `shouldReturn` "false\n"
            evaluateString . (++ "isprime 9;") <$> isPrimeDef `shouldReturn` "false\n"
            evaluateString . (++ "isprime 10;") <$> isPrimeDef `shouldReturn` "false\n"
            evaluateString . (++ "isprime 15;") <$> isPrimeDef `shouldReturn` "false\n"
            evaluateString . (++ "isprime 1087;") <$> isPrimeDef `shouldReturn` "true\n"
            evaluateString . (++ "isprime 1088;") <$> isPrimeDef `shouldReturn` "false\n"
            evaluateString . (++ "isprime 1089;") <$> isPrimeDef `shouldReturn` "false\n"
            evaluateString . (++ "isprime 1090;") <$> isPrimeDef `shouldReturn` "false\n"
            evaluateString . (++ "isprime 1091;") <$> isPrimeDef `shouldReturn` "true\n"
            evaluateString . (++ "isprime 1092;") <$> isPrimeDef `shouldReturn` "false\n"
            evaluateString . (++ "isprime 1093;") <$> isPrimeDef `shouldReturn` "true\n"
            evaluateString . (++ "isprime 7919;") <$> isPrimeDef `shouldReturn` "true\n"

        it "calculates list lengthes" $ do
            evaluateString . (++ "length nil;") <$> listLengthDef `shouldReturn` "0\n"
            evaluateString . (++ "length [];") <$> listLengthDef `shouldReturn` "0\n"
            evaluateString . (++ "length [0];") <$> listLengthDef `shouldReturn` "1\n"
            evaluateString . (++ "length [0, 3, 4, 1];") <$> listLengthDef `shouldReturn` "4\n"
            evaluateString . (++ "let l = length [0, 3, 4, 1] in l;") <$> listLengthDef `shouldReturn` "4\n"
            evaluateString . (++ "length [0, 3, 4, 1];") <$> listLengthDef `shouldReturn` "4\n"
            evaluateString . (++ "length (createList 10 15);") <$> listLengthDef `shouldReturn` "15\n"
            evaluateString . (++ "length (createList 10 1000);") <$> listLengthDef `shouldReturn` "1000\n"
            evaluateString . (++ "createList 2 4;") <$> listLengthDef `shouldReturn` "[2, 2, 2, 2]\n"

        it "merges lists" $ do
            evaluateString . (++ "merge [0, 1, 2] [3, 4, 5];") <$> mergeDef `shouldReturn` "[0, 1, 2, 3, 4, 5]\n"
            evaluateString . (++ "merge [0, 3, 4, 7, 7, 8] [1, 2, 2, 5, 6, 8, 10];") <$> mergeDef `shouldReturn` "[0, 1, 2, 2, 3, 4, 5, 6, 7, 7, 8, 8, 10]\n"

        it "sorts lists" $ do
            evaluateString . (++ "sort [];") <$> mergeSortDef `shouldReturn` "nil\n"
            evaluateString . (++ "sort [1];") <$> mergeSortDef `shouldReturn` "[1]\n"
            evaluateString . (++ "sort [1, 2];") <$> mergeSortDef `shouldReturn` "[1, 2]\n"
            evaluateString . (++ "sort [1, 43, 8, 2, 3, 1, 73, 9, 12];") <$> mergeSortDef `shouldReturn` "[1, 1, 2, 3, 8, 9, 12, 43, 73]\n"
