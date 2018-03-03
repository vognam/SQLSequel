import System.IO
import Tokens
import Grammar

main = do programFile <- openFile "Sample Program.txt" ReadMode
          program <- hGetContents programFile
          tokens <- return (alexScanTokens program)
          expr <- return (parseCalc tokens)
          --putStr (show tokens)
          putStr (show expr)
