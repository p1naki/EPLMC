module Definitions.ObExpr where

import Definitions.NFA
import Data.Set as S

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

buildNFA :: ObExpr -> Nfa Int
buildNFA (ObLit c)     = NFA (fromList [0, 1]) (singleton (Move 0 c 1)) 0 (singleton 1)
buildNFA (ObAlt e1 e2) = nfaAlter (buildNFA e1) (buildNFA e2)
buildNFA (ObCat e1 e2) = nfaConCat (buildNFA e1) (buildNFA e2)
buildNFA (ObStar e)    = nfaStar (buildNFA e)
buildNFA _             = error "No valid DFA can be built"

accObExpr :: ObExpr -> String -> Bool
accObExpr = (accepts . buildNFA)

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
obRedux (ObAlt e1 e2)   = if (e1 == e2) then e1 else  obRedux (ObAlt (obRedux e1) (obRedux e2))
obRedux (ObCat e1 e2)   = obRedux(ObCat (obRedux e1) (obRedux e2))
obRedux (ObStar e)      = ObStar (obRedux e)

obAux :: ObExpr -> ObExpr
obAux ObDel       = ObDel
obAux ObEps     = ObEps
obAux (ObLit e) = ObDel
obAux (ObAlt e1 e2) = obRedux(ObAlt (obRedux(obAux e1)) (obRedux(obAux e2)))
obAux (ObCat e1 e2) = obRedux(ObCat (obRedux(obAux e1)) (obRedux(obAux e2)))
obAux (ObStar e)    = ObEps


obQ :: ObExpr -> ObExpr -> ObExpr
obQ e (ObCat l1 l2) = obRedux( obQ (obRedux (obQ e l1) ) l2)
obQ (ObDel) _     = ObDel
obQ (ObEps) l   = if (ObEps == obRedux(l)) then ObEps else ObDel
obQ (ObLit e) l = if (ObLit e /= l) then ObDel else ObEps
obQ (ObStar e) l    =  obRedux(ObCat (obRedux (obQ e l)) (obRedux (ObStar e)))
obQ (ObCat e1 e2) l = obRedux(ObAlt (obRedux(ObCat (obRedux(obQ e1 l)) e2)) (obRedux(ObCat (obAux e1) (obRedux(obQ e2 l)))))
obQ (ObAlt e1 e2) l =  obRedux(ObAlt (obRedux(obQ e1 l)) (obRedux(obQ e2 l)))

findString :: ObExpr -> [String]
findString ObDel       = error "Invalid Input: Empty Language"
findString ObEps     = [""]
findString (ObLit c) = [[c]]
findString (ObStar e)    = findString e
findString (ObAlt e1 e2) = findString e1 ++ findString e2
findString (ObCat e1 e2) = [s1 ++ s2 | s1 <- findString e1, s2 <- [""] ++ findString e2]
