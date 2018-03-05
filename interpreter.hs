import System.IO
import Tokens
import Grammar
import Data.List.Split

main = do programFile <- openFile "Sample Program.txt" ReadMode
          program <- hGetContents programFile
          tokens <- return (alexScanTokens program)
          expr <- return (parseCalc tokens)
          --putStr (show expr)
          putStr (interpret expr)
          
interpret :: Exp -> String
interpret (Output vars files) = show (storeData files)

toVarList :: Vars -> [String]
toVarList (Vars v) = v 

storeData :: Files -> [(String, [String])]
storeData (File xs) = storeData' xs

storeData' :: [(Vars, String)] -> [(String, [String])]
storeData' [] = []
storeData' (x:xs) = (getDataFromFile x) ++ (storeData' xs)

getDataFromFile :: (Vars, String) -> [(String, [String])]
getDataFromFile (vars, file) = getDataFromFile' (toVarList vars) file

getDataFromFile' :: [String] -> String -> [(String, [String])]
getDataFromFile' vars filePath = zipVarsData vars (getLines filePath)

zipVarsData :: [String] -> [[String]] -> [(String, [String])] 
zipVarsData vars lines = zip vars [[ x !! index | x <- lines] | index <- [0.. ((length vars) - 1)]]

getLines :: String -> IO [[String]]
getLines filePath = do
           contents <- readFile filePath
           let linesOfFile = lines contents
           return [splitOn "," line | line <- linesOfFile]
                         
                                    