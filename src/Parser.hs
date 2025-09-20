{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Parser (Parser, runParse, ast, parseAst) where

import Control.Applicative

import Ast
import Lexer

-- import Debug.Trace (trace)
-- traceContext :: String -> Parser ()
-- traceContext mess =
--     P
--         ( \s ->
--             trace (mess ++ show s) Just ((), s)
--         )

newtype Parser a = P {parse :: [Token] -> Maybe (a, [Token])}

runParse :: Parser a -> [Token] -> Maybe a
runParse p ts = fst <$> parse p ts

parseAst :: [Token] -> Maybe Ast
parseAst = runParse ast

instance Functor Parser where
    fmap f p =
        P
            ( \s -> case parse p s of
                Nothing -> Nothing
                Just (el, ss) -> Just (f el, ss)
            )
instance Applicative Parser where
    pure x = P (\s -> Just (x, s))
    (<*>) pF pA =
        P
            ( \s -> case parse pF s of
                Nothing -> Nothing
                Just (f, ss) -> parse (fmap f pA) ss
            )
instance Monad Parser where
    return = pure
    (>>=) p f =
        P
            ( \s -> case parse p s of
                Nothing -> Nothing
                Just (el, ss) -> parse (f el) ss
            )
instance Alternative Parser where
    empty = P (const Nothing)
    p1 <|> p2 = P (\s -> parse p1 s <|> parse p2 s)

takeOne :: Parser Token
takeOne =
    P
        ( \case
            [] -> Nothing
            (x : xs) -> Just (x, xs)
        )

-- peekOne :: Parser Token
-- peekOne =
--     P
--         ( \case
--             [] -> Nothing
--             s@(c : _) -> Just (c, s)
--         )

conv :: (Token -> Bool) -> Parser Token
conv pr = do
    t <- takeOne
    if pr t then return t else empty

token :: Token -> Parser Token
token t = conv (== t)

skiptoken :: Token -> Parser ()
skiptoken t = do
    _ <- conv (== t)
    return ()

checkEnd :: Parser ()
checkEnd =
    P
        ( \s -> case s of
            [] -> Just ((), s)
            _ -> Nothing
        )

endIfNoContext :: Parser ()
endIfNoContext =
    P
        ( \s -> case s of
            [] -> Nothing
            _ -> Just ((), s)
        )

{-

STAT -> LET ;
EXEC -> EXPR ;

EXPR -> FUN | LET IN | APP | IDEN | NUMB | BOL | BUILTIN | IFTHENELSE | STRING | LIST
APP -> EXPR EXPR | (EXPR EXPR)

IDENS -> IDEN | IDEN IDENS
FUN -> FUN IDENS ARROW EXPR

-}

ast :: Parser Ast
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

stat :: Parser Stat
stat = do
    (i, e) <- plet
    _ <- token SMCLMN
    return $ Let i e

exec :: Parser Exec
exec = do
    e <- expr
    skiptoken SMCLMN
    return $ Expr e

plet :: Parser (Iden, Expr)
plet = do
    skiptoken LET
    i <- iden
    skiptoken EQL
    e <- expr
    return (i, e)

expr :: Parser Expr
expr = do
    es <- some noappexpr
    return $ chainExprs es
  where
    chainExprs [e] = e
    chainExprs [e1, e2] = App e1 e2
    chainExprs (e1 : e2 : es) = chainExprs $ App e1 e2 : es
    chainExprs [] = undefined

noappexpr :: Parser Expr
noappexpr =
    asum
        [ do
            skiptoken LPAR
            e <- expr
            skiptoken RPAR
            return e
        , fun
        , pletin
        , ifthenelse
        , listlit
        , BuiltIn <$> builtin
        , Constant <$> constant
        , ExIden <$> iden
        ]

fun :: Parser Expr
fun = do
    skiptoken FUN
    as <- some iden
    skiptoken ARROW
    b <- expr
    return $ Fun as b

listlit :: Parser Expr
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

pletin :: Parser Expr
pletin = do
    skiptoken LET
    i <- iden
    skiptoken EQL
    v <- expr
    skiptoken IN
    e <- expr
    return $ LetIn i v e

ifthenelse :: Parser Expr
ifthenelse = do
    skiptoken IF
    b <- expr
    skiptoken THEN
    e1 <- expr
    skiptoken ELSE
    e2 <- expr
    return $ App (App (App (BuiltIn Cond) b) e1) e2

builtin :: Parser BuiltIn
builtin = do
    t <- takeOne
    case t of
        (BUILTIN f) -> return f
        _ -> empty

iden :: Parser Iden
iden = do
    t <- takeOne
    case t of
        (IDEN i) -> return $ Iden i
        _ -> empty

constant :: Parser Constant
constant = do
    t <- takeOne
    case t of
        (CONSTANT c) -> return c
        _ -> empty
