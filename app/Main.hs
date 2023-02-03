module Main (main) where

import Solver
import qualified Data.Map as M

main :: IO ()
main = do
  print s1
  print s2

vars :: [Variable]
vars = ["a", "b", "c", "d", "e", "f", "g"]

initial ::  M.Map Variable [Int]
initial = M.fromList $ zip vars (repeat [1..5])

constraints :: [Constraint]
constraints = [
      "a = g"
    , "d = b + 1"
    , "e = d + 2"
    , "f = a - 2"
    , "b = f + 1"
    , "c = a + 1"
    ]

    
s1 :: Solution
s1 = solve [ "a = b + c" ] $ M.fromList $ zip ["a", "b", "c"] (repeat [1..5])

s2 :: Solution
s2 = solve constraints initial
