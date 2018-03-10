import System.IO
import Tokens
import Grammar
import Data.List.Split
import Data.List

--helper function
toVarList :: Vars -> [String]
toVarList (Vars v) = v 

--main ENTRY POINT
main = do programFile <- openFile "Sample Program.txt" ReadMode
          program <- hGetContents programFile
          tokens <- return (alexScanTokens program)
          expr <- return (parseCalc tokens)
          interpreted <- interpret expr
          putStr (show interpreted)



--Interpreter functions (Pattern match AST here)    
interpret :: Exp -> IO [[String]]
interpret (Output vars files) = do dat <- storeData files
                                   let output = orderByOutputVar (matchVars vars dat)
                                   return output

                                   
--Data Processing functions (e.g lexographical order etc)


orderByOutputVar :: [[String]] -> [[String]]
orderByOutputVar dat = [ [ x_i !! index | x_i <- dat] | index <- [0..(length (head dat) -1) ]]

matchVars :: Vars -> [(String, [String])] -> [[String]]
matchVars vars dat | checkOutputVarsUsed varList dat
                     = [d | var_output <- varList, (var_file, d) <- dat, var_output == var_file]
                   
                   | otherwise = [] -- ie error because not all output variables have been assigned
 where varList = toVarList vars 

checkOutputVarsUsed :: [String] -> [(String, [String])] -> Bool
checkOutputVarsUsed vars dat = and [ or [v == v_file | (v_file,d) <- dat] | v <- vars ]  
                   
--File Reading Functions                      
storeData :: Files -> IO [(String, [String])]
storeData (File files) = getDataFromFile (files !! 0) --temporarily only reading first file due to type errors
                          

                 
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
                         
