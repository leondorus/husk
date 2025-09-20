{-# OPTIONS_GHC -Wno-x-partial #-}

module Lexer (
    Token (..),
    uncomment,
    tokenize,
    uncommentAndTokenize,
) where

import Data.Char (isAlpha, isAlphaNum, isNumber, isSpace)
import Data.Foldable (foldl')
import Data.Maybe (isJust)
import Text.Read (readMaybe)

import Ast (BuiltIn (..), Constant (..))

data Token
    = -- Keywords
      FUN
    | LET
    | IN
    | IF
    | THEN
    | ELSE
    | -- Symbols
      LPAR
    | RPAR
    | ARROW
    | SMCLMN
    | EQL
    | -- Lists
      LBRK
    | RBRK
    | COMMA
    | -- Values
      IDEN String -- User variables
    | CONSTANT Constant
    | BUILTIN BuiltIn -- BuiltIn functions
    deriving (Show, Eq)

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
    -- Arithmetic
    | s == "plus" = Just $ BUILTIN Plus
    | s == "minus" = Just $ BUILTIN Minus
    | s == "mul" = Just $ BUILTIN Mul
    | s == "div" = Just $ BUILTIN Div
    | s == "mod" = Just $ BUILTIN Mod
    -- Comparison of numbers
    | s == "less" = Just $ BUILTIN Less
    | s == "lesseq" = Just $ BUILTIN LessEq
    | s == "greater" = Just $ BUILTIN Greater
    | s == "greatereq" = Just $ BUILTIN GreaterEq
    | s == "eq" = Just $ BUILTIN Equal
    | s == "neq" = Just $ BUILTIN NEqual
    -- Lists
    | s == "[" = Just LBRK
    | s == "]" = Just RBRK
    | s == "," = Just COMMA
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

tokenize :: String -> Maybe [Token]
tokenize = mapM tokenizeOne . concatMap breakOneWord . words

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

breakOneWord :: String -> [String]
breakOneWord s
    | isJust $ tokenizeOne s = [s]
    | otherwise = (map reverse . reverse) (foldl' breakOneWord' [] s)

breakOneWord' :: [String] -> Char -> [String]
breakOneWord' [] c = [[c]]
breakOneWord' full@(s : rest) c
    | not (isAlpha c) && (isJust . tokenizeOne . reverse) (c : s) = (c : s) : rest
    | c == '>' = if s == "-" then ">-" : rest else ">" : full
    | not $ isAlphaNum c = [c] : full
    | otherwise = if isAlphaNum $ head s then (c : s) : rest else [c] : full
