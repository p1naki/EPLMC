module Definitions.PtExpr where

import Definitions.BoolForm (Prp(..), BForm, isValid)
import Definitions.ObExpr (ObExpr(..), obRedux)
-- import Parsers.ObParser (parseObExpr)

data PtExpr = PtDel
            | PtEps
            | PtLit Char
            | PtAlt PtExpr PtExpr
            | PtTest BForm
            | PtCat PtExpr PtExpr
            | PtStar PtExpr
  deriving (Eq)

showPtExpr :: PtExpr -> String
showPtExpr PtDel         = "$"
showPtExpr PtEps         = "@"
showPtExpr (PtLit c)     = [c]
showPtExpr (PtTest f)    = "?" ++ show f
showPtExpr (PtAlt e1 e2) = "(" ++ showPtExpr e1 ++  "+" ++ showPtExpr e2 ++ ")"
showPtExpr (PtCat e1 e2) = "(" ++ showPtExpr e1 ++ "." ++ showPtExpr e2 ++ ")"
showPtExpr (PtStar e)    = showPtExpr e ++ "*"

instance Show PtExpr where
    show = showPtExpr

ptConvert :: PtExpr -> [Prp] -> ObExpr
ptConvert PtDel ps          = ObDel
ptConvert PtEps ps          = ObEps
ptConvert (PtLit c) ps      = ObLit c
ptConvert (PtTest f) ps     = if (isValid ps f) then ObEps else ObDel
ptConvert (PtAlt e1 e2) ps  = obRedux ( ObAlt (ptConvert e1 ps) (ptConvert e2 ps))
ptConvert (PtCat e1 e2) ps  = obRedux ( ObCat (ptConvert e1 ps) (ptConvert e2 ps))
ptConvert (PtStar e) ps     = obRedux (ObStar (ptConvert e ps))