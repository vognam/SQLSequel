{
module Grammar where
import Tokens
}

%name parseCalc
%tokentype { Token }
%error { parseError }

%token
    OUTPUT                   { TokenOutput _ }
    VAR                      { TokenVar _ $$ }
    TAKING                   { TokenTaking _ }
    FROM                     { TokenFrom _ }
    FILE                     { TokenFile _ $$ }
    AND                      { TokenAnd _ }
    WHERE                    { TokenWhere _ }
    '='                      {TokenEqual _ }

%%

Exp         : OUTPUT Vars TAKING Files                    { Output $2 $4 }
            | OUTPUT Vars TAKING Files WHERE Conditions   { OutputCond $2 $4 $6}

Vars        : VarProd                                     { Vars $1 }

VarProd     : VAR                                         { [$1] }
            | VAR VarProd                                 { $1 : $2 }

Files       : FileProd                                    { File $1 }

FileProd    : Vars FROM FILE                              { [($1, $3)] }
            | Vars FROM FILE AND FileProd                 { ($1, $3) : $5 }

Conditions  : EqualProd                                   { Equal $1 }

EqualProd   : VAR '=' VAR                                 { [(Var $1, Var $3)] }
            | VAR '=' VAR AND EqualProd                   { (Var $1, Var $3) : $5 }

{
parseError :: [Token] -> a
parseError _ = error "Parse Error"

data Exp = Output Vars Files
         | OutputCond Vars Files Conditions
         deriving Show

data Vars = Vars [String]
          | Var String
          deriving Show

data Files = File [(Vars, String)]
          deriving Show

data Conditions = Equal [(Vars, Vars)]
          deriving Show
}
