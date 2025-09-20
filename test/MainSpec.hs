module MainSpec (spec) where

import Combined
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (SomeException, try)
import Test.Hspec

fibDef =
    "let fib = fun x -> \
    \              if less x 2 then \
    \                  x \
    \              else \
    \                  plus (fib (minus x 1)) (fib (minus x 2)) "

oddEvenDef =
    "let even = fun x -> cond (eq x 0) true (odd (minus x 1)); \
    \ # Test comment                                         \n \
    \ let odd = fun x -> cond (eq x 0) false (even (minus x 1));"

isPrimeDef =
    "let isdiv = fun n m -> eq (mod n m) 0; \
    \ let isprimecounter = fun i n -> \
    \   if lesseq (mul i i) n then \
    \       if isdiv n i then \
    \           false \
    \       else \
    \           isprimecounter (plus i 1) n \
    \   else \
    \       true; \
    \ let isprime = fun n -> isprimecounter 2 n;"

listLengthDef =
    "let length = fun l -> if eq l nil then \
    \   0 \
    \ else \
    \   plus 1 (length (tl l));\
    \ let createList = fun el n -> if eq n 0 then nil else cons el (createList el (minus n 1));"

mergeDef =
    "let append = fun l k -> if eq l nil then                   \
    \         k                                                 \
    \     else                                                  \
    \         cons (hd l) (append (tl l) k);                    \
    \ let merge = fun l k -> if or (eq l nil) (eq k nil) then   \
    \         append l k                                        \
    \     else                                                  \
    \         if (less (hd l) (hd k)) then                      \
    \             cons (hd l) (merge (tl l) k)                  \
    \         else                                              \
    \             cons (hd k) (merge l (tl k));"

mergeSortDef =
    "let append = fun l k ->                                    \
    \     if eq l nil then                                      \
    \         k                                                 \
    \     else                                                  \
    \         cons (hd l) (append (tl l) k);                    \
    \ let merge = fun l k ->                                    \
    \     if or (eq l nil) (eq k nil) then                      \
    \         append l k                                        \
    \     else                                                  \
    \         if (less (hd l) (hd k)) then                      \
    \             cons (hd l) (merge (tl l) k)                  \
    \         else                                              \
    \             cons (hd k) (merge l (tl k));                 \
    \ let length = fun l ->                                     \
    \     if eq l nil then                                      \
    \         0                                                 \
    \     else                                                  \
    \         plus 1 (length (tl l));                           \
    \ let takeFirst = fun n l ->                                \
    \     if eq n 0 then                                        \
    \         nil                                               \
    \     else                                                  \
    \         cons (hd l) (takeFirst (minus n 1) (tl l));       \
    \ let dropFirst = fun n l ->                                \
    \     if eq n 0 then                                        \
    \         l                                                 \
    \     else                                                  \
    \         dropFirst (minus n 1) (tl l);                     \
    \ let sort = fun l ->                                       \
    \     let len = length l in                                 \
    \     let halfLen = div len 2 in                            \
    \     let fHalf = takeFirst halfLen l in                    \
    \     let sHalf = dropFirst halfLen l in                    \
    \     if less len 2 then l else                             \
    \     merge (sort fHalf) (sort sHalf);"

spec :: Spec
spec = do
    describe "simple stuff" $ do
        it "evaluates simple combinators" $ do
            evaluateString "let I = fun x -> x; I 17;" `shouldBe` "17\n"
            evaluateString "let K = fun x y -> x; let S = fun x y z -> (x z) (y z); K 1 2;" `shouldBe` "1\n"
            evaluateString "let K = fun x y -> x; let S = fun x y z -> (x z) (y z); S K K 7;" `shouldBe` "7\n"

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

        it "works with letin" $ do
            evaluateString "let plusFive = fun x -> plus 5 x; let y = plusFive 8 in let z = plusFive 1 in plus z y;" `shouldBe` "19\n"
            evaluateString "let plusFive = fun x -> let y = plus x 2 in plus y 3; plusFive 7;" `shouldBe` "12\n"

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

        it "evaluates odd and even numbers" $ do
            evaluateString (oddEvenDef ++ " odd 0;") `shouldBe` "false\n"
            evaluateString (oddEvenDef ++ " even 0;") `shouldBe` "true\n"
            evaluateString (oddEvenDef ++ " odd 1;") `shouldBe` "true\n"
            evaluateString (oddEvenDef ++ " even 1;") `shouldBe` "false\n"
            evaluateString (oddEvenDef ++ " odd 2;") `shouldBe` "false\n"
            evaluateString (oddEvenDef ++ " even 2;") `shouldBe` "true\n"
            evaluateString (oddEvenDef ++ " odd 200;") `shouldBe` "false\n"
            evaluateString (oddEvenDef ++ " even 200;") `shouldBe` "true\n"
            evaluateString (oddEvenDef ++ " odd 201;") `shouldBe` "true\n"
            evaluateString (oddEvenDef ++ " even 201;") `shouldBe` "false\n"
            evaluateString (oddEvenDef ++ " odd 202;") `shouldBe` "false\n"
            evaluateString (oddEvenDef ++ " even 202;") `shouldBe` "true\n"

            evaluateString "let even = fun x -> cond (eq x 0) true (odd (minus x 1)); let odd = fun x -> mnot (even x); let mnot = fun x -> cond x false true; odd 1001;" `shouldBe` "true\n"
            evaluateString "let even = fun x -> cond (eq x 0) true (odd (minus x 1)); let odd = fun x -> mnot (even x); let mnot = fun x -> cond x false true; even 1001;" `shouldBe` "false\n"

        it "evaluates prime numbers" $ do
            evaluateString (isPrimeDef ++ "isprime 2;") `shouldBe` "true\n"
            evaluateString (isPrimeDef ++ "isprime 3;") `shouldBe` "true\n"
            evaluateString (isPrimeDef ++ "isprime 4;") `shouldBe` "false\n"
            evaluateString (isPrimeDef ++ "isprime 5;") `shouldBe` "true\n"
            evaluateString (isPrimeDef ++ "isprime 6;") `shouldBe` "false\n"
            evaluateString (isPrimeDef ++ "isprime 7;") `shouldBe` "true\n"
            evaluateString (isPrimeDef ++ "isprime 8;") `shouldBe` "false\n"
            evaluateString (isPrimeDef ++ "isprime 9;") `shouldBe` "false\n"
            evaluateString (isPrimeDef ++ "isprime 10;") `shouldBe` "false\n"
            evaluateString (isPrimeDef ++ "isprime 15;") `shouldBe` "false\n"
            evaluateString (isPrimeDef ++ "isprime 1087;") `shouldBe` "true\n"
            evaluateString (isPrimeDef ++ "isprime 1088;") `shouldBe` "false\n"
            evaluateString (isPrimeDef ++ "isprime 1089;") `shouldBe` "false\n"
            evaluateString (isPrimeDef ++ "isprime 1090;") `shouldBe` "false\n"
            evaluateString (isPrimeDef ++ "isprime 1091;") `shouldBe` "true\n"
            evaluateString (isPrimeDef ++ "isprime 1092;") `shouldBe` "false\n"
            evaluateString (isPrimeDef ++ "isprime 1093;") `shouldBe` "true\n"
            evaluateString (isPrimeDef ++ "isprime 7919;") `shouldBe` "true\n"

        it "calculates list lengthes" $ do
            evaluateString (listLengthDef ++ "length nil;") `shouldBe` "0\n"
            evaluateString (listLengthDef ++ "length [];") `shouldBe` "0\n"
            evaluateString (listLengthDef ++ "length [0];") `shouldBe` "1\n"
            evaluateString (listLengthDef ++ "length [0, 3, 4, 1];") `shouldBe` "4\n"
            evaluateString (listLengthDef ++ "let l = length [0, 3, 4, 1] in l;") `shouldBe` "4\n"
            evaluateString (listLengthDef ++ "length [0, 3, 4, 1];") `shouldBe` "4\n"
            evaluateString (listLengthDef ++ "length (createList 10 15);") `shouldBe` "15\n"
            evaluateString (listLengthDef ++ "length (createList 10 1000);") `shouldBe` "1000\n"
            evaluateString (listLengthDef ++ "createList 2 4;") `shouldBe` "[2, 2, 2, 2]\n"

        it "merges lists" $ do
            evaluateString (mergeDef ++ "merge [0, 1, 2] [3, 4, 5];") `shouldBe` "[0, 1, 2, 3, 4, 5]\n"
            evaluateString (mergeDef ++ "merge [0, 3, 4, 7, 7, 8] [1, 2, 2, 5, 6, 8, 10];") `shouldBe` "[0, 1, 2, 2, 3, 4, 5, 6, 7, 7, 8, 8, 10]\n"

        it "sorts lists" $ do
            evaluateString (mergeSortDef ++ "sort [];") `shouldBe` "nil\n"
            evaluateString (mergeSortDef ++ "sort [1];") `shouldBe` "[1]\n"
            evaluateString (mergeSortDef ++ "sort [1, 2];") `shouldBe` "[1, 2]\n"
            evaluateString (mergeSortDef ++ "sort [1, 43, 8, 2, 3, 1, 73, 9, 12];") `shouldBe` "[1, 1, 2, 3, 8, 9, 12, 43, 73]\n"
