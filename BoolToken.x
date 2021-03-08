{
{-# OPTIONS_GHC -w #-}
module BoolToken (BFToken(..),scanBFTokens) where
import BoolForm
}

%wrapper "basic"

$digit = [0-9]
$eol   = [\n]

tokens :-

  $eol                          ;
  $white+                       ;
  [&]                           { \s -> BFTokenAnd }
  [\|]                          { \s -> BFTokenOr }
  [\~]                          { \s -> BFTokenNeg }
  \(                            { \s -> BFTokenLParen }
  \)                            { \s -> BFTokenRParen }
  $digit [$digit]*              { \s -> BFTokenSym ((P . read) s) }

{

data BFToken = BFTokenSym Proposition
             | BFTokenAnd
             | BFTokenOr
             | BFTokenNeg
             | BFTokenLParen
             | BFTokenRParen
             deriving (Eq,Show)

scanBFTokens = alexScanTokens

}
