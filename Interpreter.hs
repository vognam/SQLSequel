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
interpret :: Exp -> IO ([String],[[String]])
interpret (Output vars (File files)) = do dat <- mapM getDataFromFile files
                                          let output = permuteOutputRows [(orderByRow (matchVars vars d)) | d <- dat]
                                          return output
                                   
--Data Processing functions (e.g lexographical order etc)

 
getColByVar :: String -> [(String, [String])] -> [String]
getColByVar var dat = head [d | (var_file, d) <- dat, var_file == var]

permuteOutputRows :: [([String], [[String]])] -> ([String],[[String]])
permuteOutputRows dat = foldr1 foldPermute dat

foldPermute :: ([String],[[String]]) -> ([String],[[String]]) -> ([String],[[String]])
foldPermute (file1Vars, file1Rows) (file2Vars, file2Rows) = ( (file1Vars ++ file2Vars) , [ f1row ++ f2row  | f1row <- file1Rows, f2row <- file2Rows] )

orderByRow :: [(String, [String])] -> ([String],[[String]])
orderByRow dat = ([ var | (var, x_i) <- dat] , [[ x_i !! index | (var, x_i) <- dat] | index <- [0..(length (snd (head dat)) -1) ]])

matchVars :: Vars -> [(String, [String])] -> [(String,[String])]
matchVars vars dat = [(var_file, d) | var_output <- varList, (var_file, d) <- dat, var_output == var_file]          
 where varList = toVarList vars 

--checkOutputVarsUsed :: [String] -> [(String, [String])] -> Bool
--checkOutputVarsUsed vars dat = and [ or [v == v_file | (v_file,d) <- dat] | v <- vars ]  
                   
--File Reading Functions                                            
                 
getDataFromFile :: (Vars, String) -> IO [(String, [String])]
getDataFromFile (vars, filePath) = getDataFromFile' (toVarList vars) filePath

getDataFromFile' :: [String] -> String -> IO [(String, [String])]
getDataFromFile' vars filePath = do lines <- getLines filePath
                                    let zippedData = zip vars [[ x !! index | x <- lines] | index <- [0.. ((length vars) - 1)]]
                                    return zippedData
                                    
getLines :: String -> IO [[String]]
getLines filePath = do
           contents <- readFile filePath
           let linesOfFile = lines contents
           return [splitOn "," line | line <- linesOfFile]
                         
