module Cwlib (buildNeighbors) where

import qualified Data.Map as M
 
testInput = M.fromList [
     (("a", "b"), 33),
	(("a", "c"), 40),
	(("b", "d"), 3)
	]
	
buildNeighbors :: Ord a => M.Map (a, a) b -> M.Map a [(a, b)]
buildNeighbors input =
	M.unionsWith (++) 
	  [ M.singleton a [(b,count)]
	  | ((a,b),count) <- M.toList input ]
	
test = 
	testOutput == buildNeighbors testInput
	
testOutput = M.fromList [("a",[("b", 33),("c", 40)]),
	("b",[("d", 3)])
	]