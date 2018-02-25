{ 
module Tokens where 
}

%wrapper "posn" 
$digit = 0-9     
-- digits 
$alpha = [a-zA-Z]    
-- alphabetic characters

tokens :-
  $white+       ; 
  "--".*        ; 
  OUTPUT                            { \p s -> TokenOutput p } 
  TAKING                            { \p s -> TokenTaking p } 
  FROM                              { \p s -> TokenFrom p } 
  AND                               { \p s -> TokenAnd p }  
  WHERE                             { \p s -> TokenWhere p }  
  \=                                { \p s -> TokenEqual p }   
  $alpha [$alpha $digit \_ \’]*     { \p s -> TokenVar p s } 
  \,             ;

{ 
-- Each action has type :: String -> Token 
-- The token type: 
data Token = TokenOutput AlexPosn |
             TokenTaking AlexPosn |
             TokenFrom AlexPosn   |
             TokenAnd AlexPosn    |
             TokenWhere AlexPosn  |
             TokenEqual AlexPosn  |
             TokenVar AlexPosn String   
 deriving (Eq,Show) 
  
  
tokenPosn :: Token -> (Int, Int)
tokenPosn (TokenOutput (AlexPn _ line col)) = (line, col)
tokenPosn (TokenTaking (AlexPn _ line col)) = (line, col)
tokenPosn (TokenFrom (AlexPn _ line col)) = (line, col)
tokenPosn (TokenAnd (AlexPn _ line col)) = (line, col)
tokenPosn (TokenWhere (AlexPn _ line col)) = (line, col)
tokenPosn (TokenEqual (AlexPn _ line col)) = (line, col)
tokenPosn (TokenVar (AlexPn _ line col) s) = (line, col)
}

