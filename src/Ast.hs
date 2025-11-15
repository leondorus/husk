module Ast where
import Data.Foldable (foldl')

data Ast = Ast [Stat] Exec deriving (Show, Eq)

data Stat = Let {letIden :: Iden, letVal :: Expr} deriving (Show, Eq)
newtype Exec = Expr Expr deriving (Show, Eq)

data Expr
    = Fun {args :: [Iden], body :: Expr}
    | App {lAppExpr :: Expr, rAppExpr :: Expr}
    | LetIn {letInIden :: Iden, letInVal :: Expr, letInBody :: Expr}
    | Constant Constant
    | ExIden Iden
    | BuiltIn BuiltIn
    deriving (Show, Eq)
newtype Iden = Iden String deriving (Show, Eq)
data Constant
    = Numb Int
    | Bol Bool
    | Nil
    deriving (Show, Eq)

data BuiltIn
    = -- Arithmetic
      Plus
    | Minus
    | Mul
    | Div
    | Mod
    | -- Comparison of numbers
      Less
    | LessEq
    | Greater
    | GreaterEq
    | Equal
    | NEqual
    | -- Boolean
      Not
    | And
    | Or
    | Cond
    | -- List
      Cons
    | Hd
    | Tl
    deriving (Show, Eq)

cExIden :: String -> Expr
cExIden s = ExIden $ Iden s

applyApp :: [Expr] -> Expr
applyApp (e : es) = foldl' App e es
applyApp [] = undefined
