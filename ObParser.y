{
module ObParser where

import ObToken
import ObExpr
}

%name obexpr
%tokentype { ObToken }
%error { parseObErr }

%token
    ATOM  { ObTokenSym $$ }
    '+'   { ObTokenAlt }
    '.'   { ObTokenCat }
    '*'   { ObTokenStar }
    '@'   { ObTokenDel }
    '$'   { ObTokenEps }
    '('   { ObTokenLParen }
    ')'   { ObTokenRParen }

%left '+'
%left '.'
%right '*'
%%

ObExpr : '(' ObExpr '+' ObExpr ')'    { ObAlt $2 $4 }
       | ObExpr '+' ObExpr            { ObAlt $1 $3 }
       | '(' ObExpr '.' ObExpr ')'    { ObCat $2 $4 }
       | ObExpr '.' ObExpr            { ObCat $1 $3 }
       | '(' ObExpr ')' '*'           { ObStar $2 }
       | ObExpr '*'                   { ObStar $1}
       | ATOM                         { ObLit $1 }
       | '$'                          { ObDel }
       | '@'                          { ObEps }

{
parseObErr :: [ObToken] -> a
parseObErr _ = error "Parse error"

parseObExpr :: String -> ObExpr
parseObExpr = obexpr . scanObTokens
}
