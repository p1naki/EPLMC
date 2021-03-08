module Definitions.BoolForm where

newtype Prp = P Int deriving (Eq,Ord)

showPrp :: Prp -> String
showPrp (P n) = "P" ++ show n

instance Show Prp where
    show = showPrp

data BForm = PrpA Prp
           | BNeg BForm
           | BAnd BForm BForm
           | BOr BForm BForm
  deriving (Eq)

readPrp :: String -> Prp
readPrp s = P (read s::Int)

showBForm :: BForm -> String
showBForm (PrpA p)    = show p
showBForm (BNeg e)     = "~" ++ showBForm e
showBForm (BAnd e1 e2) = "(" ++ showBForm e1 ++  "&" ++ showBForm e2 ++ ")"
showBForm (BOr e1 e2)  = "(" ++ showBForm e1 ++ "|" ++ showBForm e2 ++ ")"


instance Show BForm where
    show = showBForm

isValid :: [Prp] -> BForm -> Bool
isValid ps (PrpA p)     = p `elem` ps
isValid ps (BNeg f)     = not (isValid ps f)
isValid ps (BOr f1 f2)  = (isValid ps f1) || (isValid ps f2)
isValid ps (BAnd f1 f2) = (isValid ps f1) && (isValid ps f2)