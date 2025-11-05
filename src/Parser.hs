{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Parser (Parser, ast, runParse, parseAst) where

import Control.Applicative

import Ast
import ParserLib
import Token

type TParser = Parser Token

-- Exported functions
runParse :: TParser a -> [Token] -> Maybe a
runParse p ts = fst <$> parse p ts

parseAst :: [Token] -> Maybe Ast
parseAst = runParse ast

ast :: TParser Ast
ast = do
    ss <-
        many
            ( do
                endIfNoContext
                stat
            )
    e <- exec
    checkEnd
    return $ Ast ss e

-- Top-level parsing functions
stat :: TParser Stat
stat = do
    (i, e) <- plet <|> pletfun
    _ <- token SMCLMN
    return $ Let i e

exec :: TParser Exec
exec = do
    e <- expr
    skiptoken SMCLMN
    return $ Expr e

-- Expressions and operators
expr :: TParser Expr
expr = opexpr opList

-- TODO add associacivity to list and add cons
opList :: [[BuiltIn]]
opList =
    [ [Or]
    , [And]
    , [Equal, NEqual]
    , [Less, LessEq, Greater, GreaterEq]
    , [Plus, Minus]
    , [Mul, Div, Mod]
    ]

opexpr :: [[BuiltIn]] -> TParser Expr
opexpr (bs : bss) = do
    fe <- opexpr bss
    es <-
        many
            ( do
                op <- opconv (`elem` bs)
                e <- opexpr bss
                return (op, e)
            )
    return $ chainOpExList fe es
  where
    chainOpExList :: Expr -> [(BuiltIn, Expr)] -> Expr
    chainOpExList e [] = e
    chainOpExList fe [(b, e)] = App (App (BuiltIn b) fe) e
    chainOpExList fe ((b, e) : es) = chainOpExList (App (App (BuiltIn b) fe) e) es
opexpr [] = prefixedExpr

prefixedExpr :: TParser Expr
prefixedExpr = do
    prefOp <- optional (opconv (`elem` [Plus, Minus, Not]))
    e <- appexpr
    return $ handlePrefOp prefOp e
  where
    handlePrefOp Nothing e = e
    handlePrefOp (Just Plus) e = e
    handlePrefOp (Just Minus) e = App (App (BuiltIn Minus) (Constant $ Numb 0)) e
    handlePrefOp (Just Not) e = App (BuiltIn Not) e
    handlePrefOp _ _ = undefined

appexpr :: TParser Expr
appexpr = do
    es <- some noappexpr
    return $ chainAppExprs es
  where
    chainAppExprs :: [Expr] -> Expr
    chainAppExprs [e] = e
    chainAppExprs (e1 : e2 : es) = chainAppExprs $ App e1 e2 : es
    chainAppExprs [] = undefined

noappexpr :: TParser Expr
noappexpr =
    asum
        [ do
            skiptoken LPAR
            e <- expr
            skiptoken RPAR
            return e
        , fun
        , pletin
        , pletinfun
        , ifthenelse
        , listlit
        , BuiltIn <$> builtin
        , Constant <$> constant
        , ExIden <$> iden
        ]

opconv :: (BuiltIn -> Bool) -> TParser BuiltIn
opconv p = do
    t <- takeOne
    case t of
        (OPERATOR f) -> if p f then return f else empty
        _ -> empty

-- Smaller parsing functions
fun :: TParser Expr
fun = do
    skiptoken FUN
    as <- some iden
    skiptoken ARROW
    b <- expr
    return $ Fun as b

plet :: TParser (Iden, Expr)
plet = do
    skiptoken LET
    i <- iden
    skiptoken EQL
    e <- expr
    return (i, e)

pletfun :: TParser (Iden, Expr)
pletfun = do
    skiptoken LET
    i <- iden
    as <- some iden
    skiptoken ARROW
    e <- expr
    return (i, Fun as e)

pletin :: TParser Expr
pletin = do
    skiptoken LET
    i <- iden
    skiptoken EQL
    v <- expr
    skiptoken IN
    e <- expr
    return $ LetIn i v e

pletinfun :: TParser Expr
pletinfun = do
    skiptoken LET
    i <- iden
    as <- some iden
    skiptoken ARROW
    v <- expr
    skiptoken IN
    e <- expr
    return $ LetIn i (Fun as v) e

listlit :: TParser Expr
listlit = do
    skiptoken LBRK
    es <-
        asum
            [ do
                es <- many exprWithComma
                e <- expr
                return $ es ++ [e]
            , do
                some exprWithComma
            , do
                return []
            ]
    skiptoken RBRK
    return $ foldList es
  where
    exprWithComma = do
        e <- expr
        skiptoken COMMA
        return e
    foldList [] = Constant Nil
    foldList (e : es) = App (App (BuiltIn Cons) e) (foldList es)

ifthenelse :: TParser Expr
ifthenelse = do
    skiptoken IF
    b <- expr
    skiptoken THEN
    e1 <- expr
    skiptoken ELSE
    e2 <- expr
    return $ App (App (App (BuiltIn Cond) b) e1) e2

builtin :: TParser BuiltIn
builtin = do
    t <- takeOne
    case t of
        (BUILTIN f) -> return f
        _ -> empty

constant :: TParser Constant
constant = do
    t <- takeOne
    case t of
        (CONSTANT c) -> return c
        _ -> empty

iden :: TParser Iden
iden = do
    t <- takeOne
    case t of
        (IDEN i) -> return $ Iden i
        _ -> empty
