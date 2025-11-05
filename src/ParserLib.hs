{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module ParserLib where

import Control.Applicative
import Debug.Trace (trace)

newtype Parser t a = P {parse :: [t] -> Maybe (a, [t])}

instance Functor (Parser t) where
    fmap f p =
        P
            ( \s -> case parse p s of
                Nothing -> Nothing
                Just (el, ss) -> Just (f el, ss)
            )
instance Applicative (Parser t) where
    pure x = P (\s -> Just (x, s))
    (<*>) pF pA =
        P
            ( \s -> case parse pF s of
                Nothing -> Nothing
                Just (f, ss) -> parse (fmap f pA) ss
            )
instance Monad (Parser t) where
    return = pure
    (>>=) p f =
        P
            ( \s -> case parse p s of
                Nothing -> Nothing
                Just (el, ss) -> parse (f el) ss
            )
instance Alternative (Parser t) where
    empty = P (const Nothing)
    p1 <|> p2 = P (\s -> parse p1 s <|> parse p2 s)

takeOne :: Parser t t
takeOne =
    P
        ( \case
            [] -> Nothing
            (x : xs) -> Just (x, xs)
        )

peekOne :: Parser t t
peekOne =
    P
        ( \case
            [] -> Nothing
            s@(c : _) -> Just (c, s)
        )

conv :: (t -> Bool) -> Parser t t
conv pr = do
    t <- takeOne
    if pr t then return t else empty

token :: (Eq t) => t -> Parser t t
token t = conv (== t)

skiptoken :: (Eq t) => t -> Parser t ()
skiptoken t = do
    _ <- conv (== t)
    return ()

checkEnd :: Parser t ()
checkEnd =
    P
        ( \s -> case s of
            [] -> Just ((), s)
            _ -> Nothing
        )

endIfNoContext :: Parser t ()
endIfNoContext =
    P
        ( \s -> case s of
            [] -> Nothing
            _ -> Just ((), s)
        )

traceContext :: (Show t) => String -> Parser t ()
traceContext mess =
    P
        ( \s ->
            trace (mess ++ show s) Just ((), s)
        )
