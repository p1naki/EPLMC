{
module BFormParser where

import BoolToken
import BoolForm
}

%name boolform
%tokentype { BFToken }
%error { parseBFError }

%token
    ATOM  { BFTokenSym $$ }
    '&'   { BFTokenAnd }
    '|'   { BFTokenOr }
    '~'   { BFTokenNeg }
    '('   { BFTokenLParen }
    ')'   { BFTokenRParen }

%left '|'
%left '&'
%left '~'

%%

BForm : '(' BForm '|' BForm ')'     { BOr $2 $4 }
       | BForm '|' BForm            { BOr $1 $3 }
       | '(' BForm '&' BForm ')'    { BAnd $2 $4 }
       | BForm '&' BForm            { BAnd $1 $3 }
       | '(' '~' BForm ')'          { BNeg $3 }
       | '~' BForm                  { BNeg $2 }
       | ATOM                       { PrpB $1 }

{
parseBFError :: [BFToken] -> a
parseBFError _ = error "Parse error"

parseBForm :: String -> BForm
parseBForm = boolform . scanBFTokens
}
