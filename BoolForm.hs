module BoolForm where

-- parseBForm "(1|2)" = BOr (P 1) (P 2)

data Proposition = P Int
    deriving (Eq,Ord)

instance Show Proposition where
    show (P n) = "P" ++ show n

data BForm = PrpB Proposition
           | BNeg BForm
           | BAnd BForm BForm
           | BOr BForm BForm
  deriving (Eq)

-- readPrp :: String -> Prp
-- readPrp s = P (read s::Int)

showBForm :: BForm -> String
showBForm (PrpB p)    = show p
showBForm (BNeg e)     = "~" ++ showBForm e
showBForm (BAnd e1 e2) = "(" ++ showBForm e1 ++  "&" ++ showBForm e2 ++ ")"
showBForm (BOr e1 e2)  = "(" ++ showBForm e1 ++ "|" ++ showBForm e2 ++ ")"

instance Show BForm where
    show = showBForm

isValid :: [Proposition] -> BForm -> Bool
isValid ps (PrpB p)     = p `elem` ps
isValid ps (BNeg f)     = not (isValid ps f)
isValid ps (BOr f1 f2)  = (isValid ps f1) || (isValid ps f2)
isValid ps (BAnd f1 f2) = (isValid ps f1) && (isValid ps f2)