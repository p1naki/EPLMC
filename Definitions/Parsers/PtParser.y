{
module Parsers.PtParser where

import TokenDefs.PtToken
import Definitions.PtExpr
import Definitions.BoolForm (Prp(..),BForm)

import Parsers.BFormParser (parseBForm)
}

%name ptexpr
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

PtExpr : '(' PtExpr '+' PtExpr ')'    { PtAlt $2 $4 }
       | PtExpr '+' PtExpr            { PtAlt $1 $3 }
       | '(' PtExpr '.' PtExpr ')'    { PtCat $2 $4 }
       | PtExpr '.' PtExpr            { PtCat $1 $3 }
       | '(' PtExpr ')' '*'           { PtStar $2 }
       | PtExpr '*'                   { PtStar $1}
       | FORM                         { PtTest $1 }
       | '(' FORM ')'                 { PtTest $2 }
       | ATOM                         { PtLit $1 }
       | '$'                          { PtDel }
       | '@'                          { PtEps }

{
parsePtErr :: [PtToken] -> a
parsePtErr _ = error "Parse error"

parsePtExpr :: String -> PtExpr
parsePtExpr = ptexpr . scanPtTokens
}
