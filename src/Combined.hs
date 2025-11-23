module Combined (evaluateString, toString) where

import Control.Monad.ST (ST, runST)
import Data.Char (toLower)
import Data.STRef (readSTRef)

import Ast (Constant (..))
import AstToGraph
import Data.Maybe (fromJust, isJust)
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
        toString' (Constant (Chr c)) = return $ show c
        toString' (Constant (Num i)) = return $ show i
        toString' (Constant Nil) = return "nil"
        toString' (BuiltIn f) = return $ show f
        toString' (Var s) = return $ "var(" ++ s ++ ")"
        toString' p@(Pair{}) = do
            l <- convertList p
            if isString l
                then
                    return $ map (fromJust . getChr) l
                else
                    ( do
                        res <- printList l
                        return $ "[" ++ res ++ "]"
                    )
        toString' (App gr1 gr2) = do
            g1 <- readSTRef gr1
            lStr <- toString' g1
            g2 <- readSTRef gr2
            rStr <- toString' g2
            return $ "(" ++ lStr ++ " " ++ rStr ++ ")"

        getChr (Constant (Chr c)) = Just c
        getChr _ = Nothing
        isString = all (isJust . getChr)

        convertList :: Graph s -> ST s [Graph s]
        convertList (Constant Nil) = do
            return []
        convertList (Pair gr1 gr2) = do
            g1 <- reduceGraph gr1 >>= readSTRef
            g2 <- reduceGraph gr2 >>= readSTRef
            rest <- convertList g2
            return $ g1 : rest
        convertList _ = error "Cannot convert resulting list: there is (Cons a b), where b is not Cons"
        printList :: [Graph s] -> ST s String
        printList [] = return ""
        printList [g] = toString' g
        printList (g : gs) = do
            lStr <- toString' g
            rStr <- printList gs
            return $ lStr ++ ", " ++ rStr
