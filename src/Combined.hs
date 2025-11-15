module Combined (evaluateString, toString) where

import Control.Monad.ST (ST, runST)
import Data.Char (toLower)
import Data.STRef (readSTRef)

import Ast (Constant (..))
import AstToGraph
import Graph
import Lexer (uncommentAndTokenize)
import Parser (parseAst)
import ReductionMachine (reduceGraph)

evaluateString :: String -> String
evaluateString s = runST $ do
    gr <- astToGraph ast
    res <- reduceGraph gr
    str <- toString res
    return $ str ++ "\n"
  where
    tokens = case uncommentAndTokenize s of
        (Just t) -> t
        Nothing -> error "Lexer error"
    ast = case parseAst tokens of
        (Just a) -> a
        Nothing -> error "Parsing error"

toString :: GraphRef s -> ST s String
toString graph = do
    g <- readSTRef graph
    toString' g
  where
    toString' :: Graph s -> ST s [Char]
    toString' (Comb c) = return $ show c
    toString' (Constant (Bol b)) = return $ (map toLower . show) b
    toString' (Constant (Numb i)) = return $ show i
    toString' (Constant Nil) = return "nil"
    toString' (BuiltIn f) = return $ show f
    toString' (Var s) = return $ "var(" ++ s ++ ")"
    toString' p@(Pair{}) = do
        l <- printList p
        return $ "[" ++ l ++ "]"
    toString' (App gr1 gr2) = do
        g1 <- readSTRef gr1
        lStr <- toString' g1
        g2 <- readSTRef gr2
        rStr <- toString' g2
        return $ "(" ++ lStr ++ " " ++ rStr ++ ")"

    printList :: Graph s -> ST s String
    printList (Constant Nil) = do
        return ""
    printList (Pair gr1 gr2) = do
        g1 <- reduceGraph gr1 >>= readSTRef
        lStr <- toString' g1
        g2 <- reduceGraph gr2 >>= readSTRef
        rStr <- printList g2
        let connector = if g2 == Constant Nil then "" else ", "
        return $ lStr ++ connector ++ rStr
    printList _ = error "Cannot print resulting list: there is (Cons a b), where b is not Cons"
