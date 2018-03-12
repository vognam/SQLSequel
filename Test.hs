
--finds which variables have been swapped form the first two list of strings
--then swaps the respective strings in the given list of lists
--e.g. swap ["x1", "x2"] ["x2", "x1"] [["a", "b"],["c","d"]]
-- [["b", "a"], ["d", "c"]]
swap :: [String] -> [String] -> [[String]] -> [[String]]
swap originalVars sortedVars rows = swapElementsAtRecursive (findSwaps originalVars sortedVars) rows

--finds the positions at which variables have been swapped
findSwaps :: [String] -> [String] -> [(Int, Int)]
findSwaps originalVars sortedVars = removeSwapDuplicates [(one, two) | let varLength = (length originalVars) - 1,
                                          one <- [0..varLength], two <- [0..varLength],
                                          originalVars !! one == sortedVars !! two]

--removes swaps are semantically equivalent
removeSwapDuplicates :: [(Int, Int)] -> [(Int, Int)]
removeSwapDuplicates pairs = [(first, second) | (first, second) <- pairs, first <= second]

--recursively calls swapElementsAt given multiple positions and multiple lists
swapElementsAtRecursive :: [(Int, Int)] -> [[String]] -> [[String]]
swapElementsAtRecursive (x:[]) rows = [swapElementsAt (fst x) (snd x) row | row <- rows]
swapElementsAtRecursive (x:xs) rows = swapElementsAtRecursive xs [swapElementsAt (fst x) (snd x) row | row <- rows]

--swaps two elements in list given two positions
swapElementsAt :: Int -> Int -> [a] -> [a]
swapElementsAt f s xs = map snd . foldr (\x a ->
        if fst x == f then ys !! s : a
        else if fst x == s then ys !! f : a
        else x : a) [] $ ys
    where ys = zip [0..] xs
