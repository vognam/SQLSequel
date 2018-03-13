import System.IO
import Tokens
import Grammar
import Data.List.Split
import Data.List

--helper functions to remove data constructors
toVarList :: Vars -> [String]
toVarList (Vars v) = v

toCondList :: Conditions -> [(String, String)]
toCondList (Equal conds) = [ (v1,v2) | (Var v1, Var v2) <- conds] 

--main ENTRY POINT
main = do programFile <- openFile "Sample Program.txt" ReadMode
          program <- hGetContents programFile
          tokens <- return (alexScanTokens program)
          expr <- return (parseCalc tokens)
          interpreted <- interpret expr
          putStr (show interpreted)



--Interpreter functions (Pattern match AST here)    
interpret :: Exp -> IO ([[String]])
interpret (Output vars (File files)) = do 
                                          let outputVarsValid = checkOutputVarsUsed (toVarList vars) files
                                          if outputVarsValid then putStr ("") else putStr (show outputVarsValid)
                                          let outputVarsUnique = checkOutputVarsUnique (toVarList vars)
                                          if outputVarsUnique then putStr ("") else putStr (show outputVarsUnique)
                                          let varsUnique = checkVarsUnique files
                                          if varsUnique then putStr ("") else putStr (show varsUnique)
                                          
                                          dat <- mapM getDataFromFile files --get list of [[var, [column]]] i.e files
                                          let matched = [ matchVars vars d | d <- dat]
                                          let ordered = [ orderByRow m | m <- matched, m /= [] ]
                                          let perm = permuteOutputRows ordered
                                          let swapped = swap (toVarList vars) (fst perm) (snd perm)
                                          let sorted = sort swapped
                                          return sorted

interpret (OutputCond vars (File files) (Equal conds)) = do 
                                            let outputVarsValid = checkOutputVarsUsed (toVarList vars) files
                                            if outputVarsValid then putStr ("") else putStr (show outputVarsValid)
                                            let outputVarsUnique = checkOutputVarsUnique (toVarList vars)
                                            if outputVarsUnique then putStr ("") else putStr (show outputVarsUnique)
                                            let varsUnique = checkVarsUnique files
                                            if varsUnique then putStr ("") else putStr (show varsUnique)
                                          
                                            dat <- mapM getDataFromFile files --get list of [[var, [column]]] i.e files
                                            let ordered = [ orderByRow d | d <- dat, d /= [] ]
                                            let perm = permuteOutputRows ordered
                                            let filtered = recursivelyFilter conds perm
                                            let matched = removeCols (matchCols vars (fst filtered)) filtered
                                            let swapped = swap (toVarList vars) (fst matched) (snd matched)
                                            let sorted = sort swapped
                                            return sorted
                                          
--Data Processing functions 

--returns all permutations of multiple rows of files joined together
recursivelyFilter :: [(Vars, Vars)] -> ([String], [[String]]) -> ([String], [[String]])
recursivelyFilter [] dat = dat
recursivelyFilter (x:xs) dat = recursivelyFilter xs (filterWhere x dat) 

filterWhere :: (Vars, Vars) -> ([String], [[String]]) -> ([String], [[String]])
filterWhere ((Var v1), (Var v2)) (vars, dat) = (vars, [ row | row <- dat, (row !! i) == (row !! j)])
 where (i,j) = getIndicies v1 v2 vars
 
getIndicies :: String -> String -> [String] -> (Int, Int)
getIndicies v1 v2 vars | (length is) == 2 = ((is !! 0), (is !! 1))
                       | otherwise = ((is !! 0), (is !! 0))
 where is = [ i | (i,v) <- zip [0..] vars, (v == v1 || v == v2)] 

--find which columns to keep 
matchCols :: Vars -> [String] -> [Int]
matchCols (Vars v_out) v_dat = [ i | (i,v) <- zip [0..] v_dat, elem v v_out]
 
removeCols :: [Int] -> ([String],[[String]]) -> ([String],[[String]])
removeCols indicies (vars, dat) = ((getDataFromRow vars indicies), [ getDataFromRow row indicies | row <- dat]) 

getDataFromRow :: [String] -> [Int] -> [String]
getDataFromRow row indicies = [ row !! i | i <- indicies]
 
permuteOutputRows :: [([String], [[String]])] -> ([String],[[String]])
permuteOutputRows dat = foldr1 foldPermute dat

foldPermute :: ([String],[[String]]) -> ([String],[[String]]) -> ([String],[[String]])
foldPermute (file1Vars, file1Rows) (file2Vars, file2Rows) = ( (file1Vars ++ file2Vars) , [ f1row ++ f2row  | f1row <- file1Rows, f2row <- file2Rows] )

--combines lists of columns and returns lists of rows
orderByRow :: [(String, [String])] -> ([String],[[String]])
orderByRow dat = ([ var | (var, x_i) <- dat] , [[ x_i !! index | (var, x_i) <- dat] | index <- [0..(length (snd (head dat)) -1) ]])

--checks which OUTPUT VARS match the list of [[var, [column]]] and returns the relevant ones
matchVars :: Vars -> [(String, [String])] -> [(String,[String])]
matchVars vars dat = [(var_file, d) | var_output <- varList, (var_file, d) <- dat, var_output == var_file]          
 where varList = toVarList vars 

 
--error handling functions
checkVarsUnique :: [(Vars, String)] -> Bool
checkVarsUnique files | valid = True
                      | otherwise = error "Runtime error: You cannot use the same variable name more than once when TAKING from files"
 where valid = not (containsDuplicates (checkVarsUnique' files) [])

checkVarsUnique' :: [(Vars, String)] -> [String]
checkVarsUnique' [] = []
checkVarsUnique' ((Vars vars, file):xs) = vars ++ checkVarsUnique' xs

containsDuplicates :: [String] -> [String] -> Bool
containsDuplicates [] _ = False
containsDuplicates (x:xs) alreadyUsed | elem x alreadyUsed = True
                                      | otherwise = containsDuplicates xs (alreadyUsed ++ [x])
 
checkOutputVarsUsed :: [String] -> [(Vars, String)] -> Bool
checkOutputVarsUsed vars dat | valid = True
                             | otherwise = error "Runtime error: You have tried to output a variable which has not been taken from a file"
 where valid = and [ or [ elem v (toVarList file_vars) | (file_vars, filePath) <- dat] | v <- vars ]  

checkOutputVarsUnique :: [String] -> Bool
checkOutputVarsUnique vars | valid = True
                           | otherwise = error "Runtime error: You have tried to output the same variable more than once"
 where valid = not (containsDuplicates vars [])
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
--File Reading Functions                                            
                 
getDataFromFile :: (Vars, String) -> IO [(String, [String])]
getDataFromFile (vars, filePath) = getDataFromFile' (toVarList vars) filePath

getDataFromFile' :: [String] -> String -> IO [(String, [String])]
getDataFromFile' vars filePath = do lines <- getLines filePath
                                    let valid = checkVarsinCSV vars lines
                                    if valid then putStr (show valid) else return ()
                                    let zippedData = zip vars [[ x !! index | x <- lines] | index <- [0.. ((length vars) - 1)]]
                                    return zippedData

--check if number of variables in each row of CSV is greater than or equal to variables required to extract from it
checkVarsinCSV :: [String] -> [[String]] -> Bool
checkVarsinCSV vars lines = if not (foldr (&&) True [length line >= length vars | line <- lines]) 
                                      then error "Runtime error: number of variables in CSV do not match number of variables stated"
                                      else False
                                    
getLines :: String -> IO [[String]]
getLines filePath = do
           contents <- readFile filePath
           let linesOfFile = lines contents
           return [splitOn "," line | line <- linesOfFile]
                         
