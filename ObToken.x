{
{-# OPTIONS_GHC -w #-}
module ObToken (ObToken(..),scanObTokens) where
import ObExpr
}

%wrapper "basic"

$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-

  $eol                          ;
  $white+                       ;
  [@]                           { \s -> ObTokenEps }
  [\$]                          { \s -> ObTokenDel }
  [\+]                          { \s -> ObTokenAlt }
  [\.]                          { \s -> ObTokenCat }
  [\*]                          { \s -> ObTokenStar }
  \(                            { \s -> ObTokenLParen }
  \)                            { \s -> ObTokenRParen }
  $alpha                        { \s -> ObTokenSym (head s) }

{

data ObToken = ObTokenEps
             | ObTokenDel
             | ObTokenSym Char
             | ObTokenAlt
             | ObTokenCat
             | ObTokenStar
             | ObTokenLParen
             | ObTokenRParen
             deriving (Eq,Show)

scanObTokens = alexScanTokens

}
