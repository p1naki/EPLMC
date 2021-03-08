{
{-# OPTIONS_GHC -w #-}
module POLToken (POLToken(..),scanPOLTokens) where
import ObExpr
import EPLMC
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
  \(                            { \s -> EPLTokenLParen }
  \)                            { \s -> EPLTokenRParen }
  \[                            { \s -> EPLTokenLList }
  \]                            { \s -> EPLTokenRList }
  $alpha                        { \s -> ObTokenSym (head s) }

{

data EPLToken = EPLTokenSym Proposition
              | EPLTokenConj
              | EPLTokenDisj
              | EPLTokenNeg
              | EPLTokenLParen
              | EPLTokenRParen
              | EPLTokenLList
              | EPLTokenRList
              deriving (Eq,Show)

scanObTokens = alexScanTokens

}
