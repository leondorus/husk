{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module ReductionMachine (reduceGraph) where

import Control.Monad.ST (ST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)

import Ast (BuiltIn (..), Constant (..))
import Graph

instance MonadFail (ST s) where
    fail = error

reduceGraph :: GraphRef s -> ST s (GraphRef s)
reduceGraph mainGref = do
    stackRef <- newSTRef [mainGref]

    reduceGraph' stackRef
    where
        reduceGraph' stackRef = do
            res <- step stackRef
            case res of
                Nothing -> reduceGraph' stackRef
                Just s -> return s

step :: STRef s [GraphRef s] -> ST s (Maybe (GraphRef s))
step stackRef = do
    stack <- readSTRef stackRef
    let topRef = head stack
    top <- readSTRef topRef
    ( case top of
            (Constant _) -> return $ Just topRef
            (Pair{}) -> return $ Just topRef
            -- (Pair gr1 gr2) ->
            --     ( do
            --         _ <- reduceGraph gr1 >>= readSTRef
            --         _ <- reduceGraph gr2 >>= readSTRef
            --         return $ Just topRef
            --     )
            (App lgr _) ->
                ( do
                    writeSTRef stackRef (lgr : stack)
                    return Nothing
                )
            (Comb S) ->
                ( do
                    case stack of
                        (_ : ap3 : ap2 : ap1 : rest) ->
                            ( do
                                (App _ f) <- readSTRef ap3
                                (App _ g) <- readSTRef ap2
                                (App _ x) <- readSTRef ap1
                                lap <- newSTRef (App f x)
                                rap <- newSTRef (App g x)
                                writeSTRef ap1 (App lap rap)
                                writeSTRef stackRef (ap1 : rest)
                                return Nothing
                            )
                        _ -> return $ fmap Just last stack
                )
            (Comb K) ->
                ( do
                    case stack of
                        (_ : ap2 : ap1 : rest) ->
                            ( do
                                (App _ xRef) <- readSTRef ap2
                                (App _ _) <- readSTRef ap1
                                combI <- newSTRef (Comb I)
                                writeSTRef ap1 (App combI xRef)
                                writeSTRef stackRef (ap1 : rest)
                                return Nothing
                            )
                        _ -> return $ fmap Just last stack
                )
            (Comb Y) ->
                ( do
                    case stack of
                        (_ : ap : rest) ->
                            ( do
                                (App _ fRef) <- readSTRef ap
                                writeSTRef ap (App fRef ap)
                                writeSTRef stackRef (ap : rest)
                                return Nothing
                            )
                        _ -> return $ fmap Just last stack
                )
            (Comb I) ->
                ( do
                    case stack of
                        (_ : ap : rest) ->
                            ( do
                                (App _ xRef) <- readSTRef ap
                                writeSTRef stackRef (xRef : rest)
                                return Nothing
                            )
                        _ -> return $ fmap Just last stack
                )
            -- Arithmetics
            (BuiltIn Plus) -> performBinNumOp stackRef (+)
            (BuiltIn Minus) -> performBinNumOp stackRef (-)
            (BuiltIn Mul) -> performBinNumOp stackRef (*)
            (BuiltIn Div) -> performBinNumOp stackRef div
            (BuiltIn Mod) -> performBinNumOp stackRef mod
            -- Comparison
            (BuiltIn Less) -> performBinComp stackRef (<)
            (BuiltIn LessEq) -> performBinComp stackRef (<=)
            (BuiltIn Greater) -> performBinComp stackRef (>)
            (BuiltIn GreaterEq) -> performBinComp stackRef (>=)
            (BuiltIn Equal) -> performEqComp stackRef (==)
            (BuiltIn NEqual) -> performEqComp stackRef (/=)
            -- Boolean operations
            (BuiltIn And) -> performBinBoolOp stackRef (&&)
            (BuiltIn Or) -> performBinBoolOp stackRef (||)
            (BuiltIn Not) ->
                ( do
                    case stack of
                        (_ : ap1 : rest) ->
                            ( do
                                (App _ bRef) <- readSTRef ap1
                                (Constant (Bol b)) <- reduceGraph bRef >>= readSTRef
                                icomb <- newSTRef $ Comb I
                                curRes <- newSTRef $ Constant $ Bol $ not b
                                writeSTRef ap1 (App icomb curRes)
                                writeSTRef stackRef (ap1 : rest)
                                return Nothing
                            )
                        _ -> return $ fmap Just last stack
                )
            (BuiltIn Cond) ->
                ( do
                    case stack of
                        (_ : ap3 : ap2 : ap1 : rest) ->
                            ( do
                                (App _ bRef) <- readSTRef ap3
                                (App _ xRef) <- readSTRef ap2
                                (App _ yRef) <- readSTRef ap1
                                (Constant (Bol b)) <- reduceGraph bRef >>= readSTRef
                                icomb <- newSTRef $ Comb I
                                let curRes = if b then xRef else yRef
                                writeSTRef ap1 (App icomb curRes)
                                writeSTRef stackRef (ap1 : rest)
                                return Nothing
                            )
                        _ -> return $ fmap Just last stack
                )
            (BuiltIn Cons) ->
                ( do
                    case stack of
                        (_ : ap2 : ap1 : rest) ->
                            ( do
                                (App _ xRef) <- readSTRef ap2
                                (App _ yRef) <- readSTRef ap1
                                icomb <- newSTRef $ Comb I
                                curRes <- newSTRef $ Pair xRef yRef
                                writeSTRef ap1 (App icomb curRes)
                                writeSTRef stackRef (ap1 : rest)
                                return Nothing
                            )
                        _ -> return $ fmap Just last stack
                )
            (BuiltIn Hd) ->
                ( do
                    case stack of
                        (_ : ap : rest) ->
                            ( do
                                (App _ pRef) <- readSTRef ap
                                probPair <- readSTRef pRef
                                (Pair gr1 _) <- case probPair of
                                    (Pair{}) -> readSTRef pRef
                                    _ -> reduceGraph pRef >>= readSTRef
                                icomb <- newSTRef $ Comb I
                                writeSTRef ap (App icomb gr1)
                                writeSTRef stackRef (ap : rest)
                                return Nothing
                            )
                        _ -> return $ fmap Just last stack
                )
            (BuiltIn Tl) ->
                ( do
                    case stack of
                        (_ : ap : rest) ->
                            ( do
                                (App _ pRef) <- readSTRef ap
                                probPair <- readSTRef pRef
                                (Pair _ gr2) <- case probPair of
                                    (Pair{}) -> readSTRef pRef
                                    _ -> reduceGraph pRef >>= readSTRef
                                icomb <- newSTRef $ Comb I
                                writeSTRef ap (App icomb gr2)
                                writeSTRef stackRef (ap : rest)
                                return Nothing
                            )
                        _ -> return $ fmap Just last stack
                )
            (Var nm) -> error $ "Tried to reduce " ++ nm ++ " variable"
        )

performBinNumOp :: STRef s [GraphRef s] -> (Int -> Int -> Int) -> ST s (Maybe (GraphRef s))
performBinNumOp stackRef op = do
    stack <- readSTRef stackRef
    case stack of
        (_ : ap2 : ap1 : rest) ->
            ( do
                (App _ xRef) <- readSTRef ap2
                (App _ yRef) <- readSTRef ap1
                (Constant (Num x)) <- reduceGraph xRef >>= readSTRef
                (Constant (Num y)) <- reduceGraph yRef >>= readSTRef
                icomb <- newSTRef $ Comb I
                curRes <- newSTRef $ Constant $ Num (op x y)
                writeSTRef ap1 (App icomb curRes)
                writeSTRef stackRef (ap1 : rest)
                return Nothing
            )
        _ -> return $ fmap Just last stack

performBinComp :: STRef s [GraphRef s] -> (Int -> Int -> Bool) -> ST s (Maybe (GraphRef s))
performBinComp stackRef op = do
    stack <- readSTRef stackRef
    case stack of
        (_ : ap2 : ap1 : rest) ->
            ( do
                (App _ xRef) <- readSTRef ap2
                (App _ yRef) <- readSTRef ap1
                (Constant (Num x)) <- reduceGraph xRef >>= readSTRef
                (Constant (Num y)) <- reduceGraph yRef >>= readSTRef
                icomb <- newSTRef $ Comb I
                curRes <- newSTRef $ Constant $ Bol (op x y)
                writeSTRef ap1 (App icomb curRes)
                writeSTRef stackRef (ap1 : rest)
                return Nothing
            )
        _ -> return $ fmap Just last stack

performEqComp :: STRef s [GraphRef s] -> (Graph s -> Graph s -> Bool) -> ST s (Maybe (GraphRef s))
performEqComp stackRef op = do
    stack <- readSTRef stackRef
    case stack of
        (_ : ap2 : ap1 : rest) ->
            ( do
                (App _ xRef) <- readSTRef ap2
                (App _ yRef) <- readSTRef ap1
                g1 <- reduceGraph xRef >>= readSTRef
                g2 <- reduceGraph yRef >>= readSTRef
                icomb <- newSTRef $ Comb I
                curRes <- newSTRef $ Constant $ Bol (op g1 g2)
                writeSTRef ap1 (App icomb curRes)
                writeSTRef stackRef (ap1 : rest)
                return Nothing
            )
        _ -> return $ fmap Just last stack

performBinBoolOp :: STRef s [GraphRef s] -> (Bool -> Bool -> Bool) -> ST s (Maybe (GraphRef s))
performBinBoolOp stackRef op = do
    stack <- readSTRef stackRef
    case stack of
        (_ : ap2 : ap1 : rest) ->
            ( do
                (App _ xRef) <- readSTRef ap2
                (App _ yRef) <- readSTRef ap1
                (Constant (Bol x)) <- reduceGraph xRef >>= readSTRef
                (Constant (Bol y)) <- reduceGraph yRef >>= readSTRef
                icomb <- newSTRef $ Comb I
                curRes <- newSTRef $ Constant $ Bol (op x y)
                writeSTRef ap1 (App icomb curRes)
                writeSTRef stackRef (ap1 : rest)
                return Nothing
            )
        _ -> return $ fmap Just last stack
