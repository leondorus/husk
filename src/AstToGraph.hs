module AstToGraph (astToGraph) where

import Control.Monad (forM_)
import Control.Monad.ST (ST)
import Data.Map (elems, insert)
import qualified Data.Map as Map
import Data.STRef (
    modifySTRef,
    newSTRef,
    readSTRef,
    writeSTRef,
 )

import Ast
import Graph

astToGraph :: Ast -> ST s (GraphRef s)
astToGraph (Ast stats (Expr finexpr)) = do
    mref <- newSTRef Map.empty

    forM_ stats $ \s -> do
        (key, val) <- statToPair s
        modifySTRef mref (insert key val)
    fingr <- astToTree finexpr

    m <- readSTRef mref

    forM_ (fingr : elems m) $ \gr -> do
        vars <- findVars gr
        forM_ vars $ \vref -> do
            v <- readSTRef vref
            case v of
                (Var nm) ->
                    let mbRealGraphRef = Map.lookup nm m
                     in maybe
                            (return ())
                            ( \realGraphRef -> do
                                realGraph <- readSTRef realGraphRef
                                writeSTRef vref realGraph
                            )
                            mbRealGraphRef
                _ -> error "Find var finded not var"

    return fingr

statToPair :: Stat -> ST s (String, GraphRef s)
statToPair (Let (Iden nm) e) = do
    gr <- astToTree e
    return (nm, gr)

astToTree :: Expr -> ST s (GraphRef s)
astToTree = termToGraph . astToTerm

data LTerm
    = LApp LTerm LTerm
    | LAbs String LTerm
    | LComb Combinator
    | LConstant Constant
    | LBuiltIn BuiltIn
    | LVar String
    deriving (Show, Eq)

astToTerm :: Expr -> LTerm
astToTerm (ExIden (Iden str)) = LVar str
astToTerm (Ast.Constant c) = LConstant c
astToTerm (Ast.BuiltIn f) = LBuiltIn f
astToTerm (Ast.App e1 e2) = LApp (astToTerm e1) (astToTerm e2)
astToTerm (LetIn nm val bdy) =
    if doesHaveIden nm val
        then LApp (modTerm nm (astToTerm bdy)) (LApp (LComb Y) (modTerm nm (astToTerm val)))
        else LApp (modTerm nm (astToTerm bdy)) (astToTerm val)
astToTerm (Fun [] _) = error "Lambda function with no arguments"
astToTerm (Fun [a] (Fun as bdy)) = astToTerm (Fun (a : as) bdy)
astToTerm (Fun [a] e) = modTerm a (astToTerm e)
astToTerm (Fun (a1 : a2 : as) bdy) = modTerm a1 (astToTerm (Fun (a2 : as) bdy))

modTerm :: Iden -> LTerm -> LTerm
modTerm _ c@(LConstant{}) = LApp (LComb K) c
modTerm _ c@(LComb{}) = LApp (LComb K) c
modTerm _ f@(LBuiltIn{}) = LApp (LComb K) f
modTerm (Iden i) (LVar j) = if i == j then LComb I else LApp (LComb K) (LVar j)
modTerm i (LApp e1 e2) = LApp (LApp (LComb S) (modTerm i e1)) (modTerm i e2)
modTerm i (LAbs nm bdy) = modTerm i (modTerm (Iden nm) bdy)

doesHaveIden :: Iden -> Expr -> Bool
doesHaveIden _ (Ast.Constant{}) = False
doesHaveIden _ (Ast.BuiltIn{}) = False
doesHaveIden i (Fun _ bdy) = doesHaveIden i bdy
doesHaveIden (Iden i) (ExIden (Iden j)) = i == j
doesHaveIden i (Ast.App e1 e2) = doesHaveIden i e1 || doesHaveIden i e2
doesHaveIden i (LetIn nm val bdy) = (nm /= i) && (doesHaveIden i val || doesHaveIden i bdy)

termToGraph :: LTerm -> ST s (GraphRef s)
termToGraph (LConstant c) = newSTRef $ Graph.Constant c
termToGraph (LVar nm) = newSTRef $ Graph.Var nm
termToGraph (LComb c) = newSTRef $ Graph.Comb c
termToGraph (LBuiltIn f) = newSTRef $ Graph.BuiltIn f
termToGraph (LApp t1 t2) = do
    g1 <- termToGraph t1
    g2 <- termToGraph t2
    newSTRef $ Graph.App g1 g2
termToGraph (LAbs{}) = undefined

-- TODO efficient list concatination
findVars :: GraphRef s -> ST s [GraphRef s]
findVars gref = do
    g <- readSTRef gref
    case g of
        (Comb _) -> return []
        (Graph.Constant _) -> return []
        (Graph.BuiltIn _) -> return []
        (Var _) -> return [gref]
        (Pair g1 g2) -> do
            res1 <- findVars g1
            res2 <- findVars g2
            return $ res1 ++ res2
        (Graph.App g1 g2) -> do
            res1 <- findVars g1
            res2 <- findVars g2
            return $ res1 ++ res2
