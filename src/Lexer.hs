{-# OPTIONS_GHC -Wno-x-partial #-}

module Lexer (
    uncomment,
    tokenize,
    uncommentAndTokenize,
) where

import Control.Applicative (Alternative (..), asum)
import Data.Char (isAlpha, isAlphaNum, isNumber, isSpace)
import Data.Foldable (foldl')
import Data.Maybe (fromJust, isJust)
import Text.Read (readMaybe)

import Ast (BuiltIn (..), Constant (..))
import ParserLib
import Token

tokenizeOne :: String -> Maybe Token
tokenizeOne s
    | any isSpace s = Nothing
    -- Keywords
    | s == "fun" = Just FUN
    | s == "let" = Just LET
    | s == "in" = Just IN
    | s == "if" = Just IF
    | s == "then" = Just THEN
    | s == "else" = Just ELSE
    -- Symbols
    | s == "(" = Just LPAR
    | s == ")" = Just RPAR
    | s == "->" = Just ARROW
    | s == ";" = Just SMCLMN
    | s == "=" = Just EQL
    -- Boolean
    | s == "true" = Just $ CONSTANT $ Bol True
    | s == "false" = Just $ CONSTANT $ Bol False
    | s == "not" = Just $ BUILTIN Not
    | s == "and" = Just $ BUILTIN And
    | s == "or" = Just $ BUILTIN Or
    | s == "cond" = Just $ BUILTIN Cond
    | s == "!" = Just $ OPERATOR Not
    | s == "&&" = Just $ OPERATOR And
    | s == "||" = Just $ OPERATOR Or
    -- Arithmetic
    | s == "plus" = Just $ BUILTIN Plus
    | s == "minus" = Just $ BUILTIN Minus
    | s == "mul" = Just $ BUILTIN Mul
    | s == "div" = Just $ BUILTIN Div
    | s == "mod" = Just $ BUILTIN Mod
    | s == "+" = Just $ OPERATOR Plus
    | s == "-" = Just $ OPERATOR Minus
    | s == "*" = Just $ OPERATOR Mul
    | s == "/" = Just $ OPERATOR Div
    | s == "%" = Just $ OPERATOR Mod
    -- Comparison of numbers
    | s == "less" = Just $ BUILTIN Less
    | s == "lesseq" = Just $ BUILTIN LessEq
    | s == "greater" = Just $ BUILTIN Greater
    | s == "greatereq" = Just $ BUILTIN GreaterEq
    | s == "eq" = Just $ BUILTIN Equal
    | s == "neq" = Just $ BUILTIN NEqual
    | s == "<" = Just $ OPERATOR Less
    | s == "<=" = Just $ OPERATOR LessEq
    | s == ">" = Just $ OPERATOR Greater
    | s == ">=" = Just $ OPERATOR GreaterEq
    | s == "==" = Just $ OPERATOR Equal
    | s == "!=" = Just $ OPERATOR NEqual
    -- Lists
    | s == "[" = Just LBRK
    | s == "]" = Just RBRK
    | s == "," = Just COMMA
    | s == ":" = Just $ OPERATOR Cons
    | s == "cons" = Just $ BUILTIN Cons
    | s == "nil" = Just $ CONSTANT Nil
    | s == "hd" = Just $ BUILTIN Hd
    | s == "tl" = Just $ BUILTIN Tl
    -- Comparison of numbers
    | suitableNum s = CONSTANT . Numb <$> readMaybe s
    | suitableName s = Just $ IDEN s
    | otherwise = Nothing

suitableName :: String -> Bool
suitableName [] = False
suitableName (c : cs) = isAlpha c && all (\o -> isAlphaNum o || o == '_') cs

suitableNum :: String -> Bool
suitableNum [] = False
suitableNum cs = all isNumber cs

uncommentAndTokenize :: String -> Maybe [Token]
uncommentAndTokenize = tokenize . uncomment

uncomment :: String -> String
uncomment [] = []
uncomment ('#' : cs) = incomment cs
uncomment (c : cs) = c : uncomment cs
incomment :: [Char] -> [Char]
incomment [] = []
incomment str@('\n' : _) = uncomment str
incomment (_ : cs) = incomment cs

tokenize :: String -> Maybe [Token]
tokenize = mapM tokenizeOne . concatMap breakOneWord . concatMap breakWordIntoConseq . words

-- Breaks one word into words each is either [a-zA-Z][a-zA-Z0-9|-|_]* or [0-9]* or (!AlphaNum)*
breakWordIntoConseq :: String -> [String]
breakWordIntoConseq "" = []
breakWordIntoConseq s = fst . fromJust $ parse breakWordIntoConseq' s
breakWordIntoConseq' :: Parser Char [String]
breakWordIntoConseq' = do
    some $ asum [idenWord, numbWord, symbWord]
  where
    idenWord = do
        c <- conv isAlpha
        cs <- many (conv (\ch -> isAlphaNum ch || ch == '_'))
        return $ c : cs
    numbWord = some (conv isNumber)
    symbWord = some (conv (not . isAlphaNum))

breakOneWord :: String -> [String]
breakOneWord "" = []
breakOneWord s@(c : _) = if isAlphaNum c then [s] else breakSymbolWord s

breakSymbolWord :: String -> [String]
breakSymbolWord s
    | isJust $ tokenizeOne s = [s]
    | otherwise = reverse (foldl' breakOneWord' [] s)

breakOneWord' :: [String] -> Char -> [String]
breakOneWord' [] c = [[c]]
breakOneWord' full@(s : rest) c
    | (isJust . tokenizeOne) (s ++ [c]) = (s ++ [c]) : rest
    | otherwise = [c] : full
