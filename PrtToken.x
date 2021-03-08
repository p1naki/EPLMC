{
{-# OPTIONS_GHC -w #-}
module PrtToken (PtToken(..),scanPtTokens) where

import BoolForm
import BFormParser
}

%wrapper "basic"

$alpha = [a-zA-Z]
$symbols = [ & \| \~ \( \) ]
$digits = [0-9]
$eol   = [\n]

tokens :-

  $eol                             ;
  $white+                          ;
  [@]                              { \s -> PtTokenEps }
  [\$]                             { \s -> PtTokenDel }
  [\+]                             { \s -> PtTokenAlt }
  [\.]                             { \s -> PtTokenCat }
  [\*]                             { \s -> PtTokenStar }
  \(                               { \s -> PtTokenLParen }
  \)                               { \s -> PtTokenRParen }
  $alpha                           { \s -> PtTokenSym (head s) }
  "?" [$symbols $digits ]+         { \s -> PtTokenTest ((parseBForm . tail) s) }

{

data PtToken = PtTokenEps
             | PtTokenDel
             | PtTokenSym Char
             | PtTokenTest BForm
             | PtTokenAlt
             | PtTokenCat
             | PtTokenStar
             | PtTokenLParen
             | PtTokenRParen
             deriving (Eq,Show)

scanPtTokens = alexScanTokens

}