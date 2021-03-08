{-# LANGUAGE DeriveGeneric #-}
module ObExpr where

import Data.Set as S
-- import Data.Aeson
-- import GHC.Generics

data ObExpr = ObDel
            | ObEps
            | ObLit Char
            | ObAlt ObExpr ObExpr
            | ObCat ObExpr ObExpr
            | ObStar ObExpr
  deriving (Eq,Ord)

showObExpr :: ObExpr -> String
showObExpr ObDel         = "$"
showObExpr ObEps         = "@"
showObExpr (ObLit c)     = [c]
showObExpr (ObAlt e1 e2) = "(" ++ showObExpr e1 ++  "+" ++ showObExpr e2 ++ ")"
showObExpr (ObCat e1 e2) = "(" ++ showObExpr e1 ++ "." ++ showObExpr e2 ++ ")"
showObExpr (ObStar e)   = showObExpr e ++ "*"

instance Show ObExpr where
    show = showObExpr

-- instance FromJSON ObExpr
-- instance ToJSON ObExpr

-- accObExpr :: ObExpr -> String -> Bool
-- accObExpr = (accepts . buildNFA)

-- matchString :: String -> String -> Bool
-- matchString = (accObExpr . parseObExpr)

obRedux :: ObExpr -> ObExpr
obRedux ObDel           = ObDel
obRedux ObEps           = ObEps
obRedux (ObLit e)       = ObLit e
obRedux (ObAlt e ObDel) = e
obRedux (ObAlt ObDel e) = e
obRedux (ObCat e ObDel) = ObDel
obRedux (ObCat ObDel e) = ObDel
obRedux (ObCat e ObEps) = e
obRedux (ObCat ObEps e) = e
obRedux (ObStar ObDel)  = ObEps
obRedux (ObStar ObEps)  = ObEps
obRedux (ObAlt e1 e2)   = if (e1 == e2) then e1 else  ObAlt (obRedux e1) (obRedux e2)
obRedux (ObCat e1 e2)   = ObCat (obRedux e1) (obRedux e2)
obRedux (ObStar e)      = ObStar (obRedux e)

obAugment :: ObExpr -> ObExpr
obAugment e = ObCat e (ObLit '#')

obAux :: ObExpr -> ObExpr
obAux ObDel         = ObDel
obAux ObEps         = ObEps
obAux (ObLit e)     = ObDel
obAux (ObAlt e1 e2) = obRedux(ObAlt (obRedux(obAux e1)) (obRedux(obAux e2)))
obAux (ObCat e1 e2) = obRedux(ObCat (obRedux(obAux e1)) (obRedux(obAux e2)))
obAux (ObStar e)    = ObEps

obNullable :: ObExpr -> Bool
obNullable r = if obRedux(obAux r) == ObDel then False else True

obQ :: ObExpr -> ObExpr -> ObExpr
obQ e (ObCat l1 l2) = obRedux( obQ (obRedux (obQ e l1) ) l2)
obQ (ObDel) _     = ObDel
obQ (ObEps) l   = if (ObEps == obRedux(l)) then ObEps else ObDel
obQ (ObLit e) l = if (ObLit e /= l) then ObDel else ObEps
obQ (ObStar e) l    =  obRedux(ObCat (obRedux (obQ e l)) (obRedux (ObStar e)))
obQ (ObCat e1 e2) l = obRedux(ObAlt (obRedux(ObCat (obRedux(obQ e1 l)) e2))
                             (obRedux(ObCat (obAux e1) (obRedux(obQ e2 l)))))
obQ (ObAlt e1 e2) l =  obRedux(ObAlt (obRedux(obQ e1 l)) (obRedux(obQ e2 l)))

makeObs :: String -> ObExpr
makeObs [] = ObEps
makeObs (x:xs) = obRedux(ObCat (ObLit x) (makeObs xs))

findCharSet :: ObExpr -> Set Char
findCharSet ObDel = error "Empty Language"
findCharSet ObEps = S.fromList []
findCharSet (ObLit a) = S.fromList[a]
findCharSet (ObCat a b) = S.union (findCharSet a)  (findCharSet b)
findCharSet (ObAlt a b) = S.union (findCharSet a)  (findCharSet b)
findCharSet (ObStar a) = findCharSet a

-- findString :: ObExpr -> String
-- findString ObDel       = error "Invalid Input: Empty Language"
-- findString ObEps     = []
-- findString (ObLit c) = [c]
-- findString (ObStar e)    = findString e
-- findString (ObAlt e1 e2) = findString e1
-- findString (ObCat e1 e2) = findString e1 ++ findString e2