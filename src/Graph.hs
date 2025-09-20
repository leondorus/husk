module Graph where

import Data.STRef (
    STRef,
 )

import Ast (BuiltIn, Constant (..))

data Graph s
    = App (GraphRef s) (GraphRef s)
    | Comb Combinator
    | Constant Constant
    | Var String
    | Pair (GraphRef s) (GraphRef s)
    | BuiltIn BuiltIn
    deriving (Eq)
type GraphRef s = STRef s (Graph s)

data Combinator = S | K | I | Y deriving (Show, Eq)
