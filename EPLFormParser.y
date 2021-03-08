{
module PrtParser where

import PrtToken
import PrtExpr
import BoolForm

import BFormParser (parseBForm)
}

%name prtexpr
%tokentype { PtToken }
%error { parsePtErr }

%token
    ATOM  { PtTokenSym $$ }
    FORM  { PtTokenTest $$ }
    '+'   { PtTokenAlt }
    '.'   { PtTokenCat }
    '*'   { PtTokenStar }
    '@'   { PtTokenDel }
    '$'   { PtTokenEps }
    '('   { PtTokenLParen }
    ')'   { PtTokenRParen }

%left '+'
%left '.'
%right '*'
%%

PrtExpr : '(' PrtExpr '+' PrtExpr ')'    { PtAlt $2 $4 }
       | PrtExpr '+' PrtExpr             { PtAlt $1 $3 }
       | '(' PrtExpr '.' PrtExpr ')'     { PtCat $2 $4 }
       | PrtExpr '.' PrtExpr             { PtCat $1 $3 }
       | '(' PrtExpr ')' '*'             { PtStar $2 }
       | PrtExpr '*'                     { PtStar $1}
       | FORM                            { PtTest $1 }
       | '(' FORM ')'                    { PtTest $2 }
       | ATOM                            { PtLit $1 }
       | '$'                             { PtDel }
       | '@'                             { PtEps }

{
parsePtErr :: [PtToken] -> a
parsePtErr _ = error "Parse error"

parsePrtExpr :: String -> PrtExpr
parsePrtExpr = prtexpr . scanPtTokens
}
