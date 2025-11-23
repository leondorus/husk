module Token where

import Ast

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
    | STRING String
    | BUILTIN BuiltIn -- BuiltIn functions
    | OPERATOR BuiltIn -- Operators
    deriving (Show, Eq)
