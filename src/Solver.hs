module Solver
 ( solve
 , Solution
 , module Types
 ) where

import Types
import qualified Data.Map as M
import Data.Maybe (fromJust, mapMaybe)

type Solution = M.Map Variable Int

type PartialSolution = M.Map Variable [Int]

type Occurrences = M.Map Variable [Constraint]

type Assignment = ([Variable], [Int])

solve :: [Constraint] -> PartialSolution -> Solution
solve cs initial = solve' cs initial (genOcc (M.keys initial) cs)

genOcc :: [Variable] -> [Constraint] -> Occurrences
genOcc [] _ = M.empty
genOcc (v:vs) cs = M.insert v (filter appearsFree cs) (genOcc vs cs)
    where
        appearsFree (Constraint _ _ e2) = v `elem` freeVars e2

solve' :: [Constraint] -> PartialSolution -> Occurrences -> Solution
solve' [] ps _ = M.map head ps
solve' (c:cs) ps occs = 
        if length (newPs M.! v) < length (ps M.! v)
        then solve' (cs ++ occs M.! v) newPs occs
        else solve' cs newPs occs
    where
        newPs = discharge c ps
        Constraint (Var v) _ _ = c

discharge :: Constraint -> PartialSolution -> PartialSolution
discharge (Constraint (Var v) r expr) ps = case M.lookup v ps of
  Nothing -> error "unable to discharge"
  Just vs -> foldl eliminateFromFv (M.insert v legalVs ps) (zip [0..] fv)
    where
      combinations :: [[Int]]
      combinations = mapM (ps M.!) fv
      
      allSolutions :: M.Map [Int] Int
      allSolutions = foldl (\solutionMap c -> M.insert c (solveAssignment expr (fv, c)) solutionMap) M.empty combinations
      
      legalVs :: [Int]
      legalVs = filter (\i -> any (solveRelation r i) (M.elems allSolutions)) vs

      fv :: [Variable]
      fv = freeVars expr

      ks :: [[Int]]
      ks = M.keys allSolutions

      eliminateFromFv :: PartialSolution -> (Int, Variable) -> PartialSolution
      eliminateFromFv ps (i,v) = 
        let vs' = foldl (\acc v' -> let fks = filter (\a -> a !! i == v') ks in if any (`elem` legalVs) (lookupMany fks allSolutions) then v':acc else acc)
                        []
                        (fromJust $ M.lookup v ps)
        in M.insert v vs' ps
discharge _ _ = error "Constraint is not in NF"

lookupMany :: (Ord k) => [k] -> M.Map k a -> [a]
lookupMany ks m = mapMaybe (`M.lookup` m) ks

solveAssignment :: Expression -> Assignment -> Int
solveAssignment expr (vars, vals) = solveExpr $ foldl (uncurry . instantiate) expr (zip vars vals)

solveExpr :: Expression -> Int
solveExpr (Const i) = i
solveExpr (Var v) = error $ "unsolved var " ++ show v
solveExpr (Expr e1 op e2) = case op of
  Times -> solveExpr e1 * solveExpr e2
  Plus -> solveExpr e1 + solveExpr e2
  Minus -> solveExpr e1 - solveExpr e2

solveRelation :: (Ord a) => Relation -> (a -> a -> Bool)
solveRelation Equal = (==)
solveRelation Greater = (>)
solveRelation Smaller = (<)

instantiate :: Expression -> Variable -> Int -> Expression
instantiate (Var v1) v2 i | v1 == v2 = Const i
                          | otherwise = Var v1
instantiate (Expr e1 op e2) v i = Expr (instantiate e1 v i) op (instantiate e2 v i) 
instantiate c _ _ = c

freeVars :: Expression -> [Variable]
freeVars (Expr e1 _ e2) = freeVars e1 ++ freeVars e2
freeVars (Var v) = [v]
freeVars _ = []
