module EPLMC where

import qualified Data.Map as Map
import Data.List (nub)

import Util (apply,restrict,Rel,Erel,bl,fusion,dom,powerList,Agent(..))

import ObExpr
import Ob2DFA
import BoolForm
import PrtExpr
import ObParser
import PrtParser

data EpExpM state = Mo
    [state]
    [Agent]
    [(state,[Proposition])]
    [(state,ObExpr)]
    [(Agent,Erel state)]
    deriving (Eq)

instance Show state => Show (EpExpM state) where
  show (Mo worlds ags val exp accs) = concat
    [ "Mo\n  "
    , show worlds, "\n  "
    , show ags, "\n  "
    , show val, "\n  "
    , show exp, "\n  "
    , show accs, "\n  "
    ]

relE :: Show a => Agent -> EpExpM a -> Erel a
relE ag (Mo _ _ _ _ rels) = apply rels ag

data EPLForm phi = Top
            | Prp Proposition
            | Ng (EPLForm phi)
            | Conj [EPLForm phi]
            | Disj [EPLForm phi]
            | Kn Agent (EPLForm phi)
            | PO ObExpr (EPLForm phi)
            | PU PtdPrtM (EPLForm phi)
          deriving (Eq,Show)

impl :: EPLForm a -> EPLForm a -> EPLForm a
impl form1 form2 = Disj [Ng form1, form2]

returnObX :: (Eq a, Ord a, Show a) => a -> EpExpM a -> ObExpr
returnObX s m@(Mo _ _ _ obs _) = apply obs s

isTrueAt :: EpExpM Int -> Int -> EPLForm Int -> Bool
isTrueAt _ _ Top = True
isTrueAt (Mo _ _ val _ _) w (Prp p) = p `elem` apply val w
isTrueAt m w (Ng f)    = not (isTrueAt m w f)
isTrueAt m w (Conj fs) = all (isTrueAt m w) fs
isTrueAt m w (Disj fs) = any (isTrueAt m w) fs
isTrueAt m w (Kn ag f) = all (flip (isTrueAt m) f) (bl (relE ag m) w)
isTrueAt m w (PO e f)  = and [isTrueAt (updPO m s) w f | s <- strList]
  where
    expDFA  = eliminateDeads (filterMark (buildDfa (returnObX w m)))
    eDFA    =  eliminateDeads (filterMark (buildDfa e))
    pdList  = map (convertProduct eDFA) (findPartitions expDFA)
    strList = filter (/= [] ) $ map findStringDFA pdList
isTrueAt m@(Mo _ _ val _ _) w (PU (p@(Po _ _ prt _),t) f)
    = obRedux (ptConvert (apply prt t) (apply val w)) == ObDel 
    || isTrueAt (prtProduct m p) (returnState m p (w,t)) f

-- | global truth in a model
-- isTrue :: Show a => Ord a => EpExpM a -> EPLForm a -> Bool
-- isTrue m@(Mo _ _ _ _ _ points) f = all (\w -> isTrueAt m w f) points

--public observation
updPO :: (Show state, Ord state) => EpExpM state -> String -> EpExpM state
updPO m@(Mo states agents val obs rels) w =
    Mo states' agents val' obs' rels' where
    states' = [s | s <- states, (obRedux (obQ (apply obs s) (makeObs w))) /= ObDel ]
    val'    = [ (s, apply val s) | s <- states' ]
    obs'    = [(s, obRedux(obQ (apply obs s) (makeObs w)) )  | s <- states' ]
    rels'   = [ (ag, restrict states' r) | (ag,r) <- rels ]

data PrtM state = Po
    [state]
    [Agent]
    [(state,PrtExpr)]
    [(Agent,Erel state)]
    deriving (Eq)

type PtdPrtM = (PrtM Int, Int)

instance Show state => Show (PrtM state) where
  show (Po objects ags prt accs) = concat
    [ "Po\n  "
    , show objects, "\n  "
    , show ags, "\n  "
    , show prt, "\n  "
    , show accs, "\n  "
    ]

relP :: Show a => Agent -> PrtM a -> Erel a
relP ag (Po _ _ _ accs) = apply accs ag

data ComM a = Com
    [(a,a)]
    [Agent]
    [((a,a),[Proposition])]
    [((a,a),ObExpr)]
    [(Agent,[((a,a),(a,a))])]
    deriving (Eq)

instance Show state => Show (ComM state) where
  show (Com worlds ags val obs accs) = concat
    [ "Com\n  "
    , show worlds, "\n  "
    , show ags, "\n  "
    , show val, "\n  "
    , show obs, "\n  "
    , show accs, "\n  "
    ]

prtProduct' :: (Eq a,Show a) => EpExpM a -> PrtM a -> ComM a
prtProduct' md@(Mo worlds ags val obs accs) pt@(Po objects pags prt paccs)
  = Com worlds' ags val' obs' accs' where
    worlds' = filter predicate [(s,t) | s <- worlds, t <- objects]
    predicate tup = obRedux (ptConvert (apply prt (snd tup)) (apply val (fst tup))) /= ObDel 
    val'    = [(p,apply val (fst p))  | p <- worlds']
    obs'    = [(p, (obRedux (ptConvert  (apply prt (snd p)) (apply val (fst p))))) | p <- worlds']
    accs'   = [(ag, worldList ag worlds' accs paccs) | ag <- ags]
    worldList ag worlds' accs paccs = [(x,y) | x <- worlds', y <- worlds', func x y ag accs paccs == True]
    func x y ag accs paccs = if ((bl (apply accs ag) (fst x)) == (bl (apply accs ag) (fst y )))
                            && ((bl (apply paccs ag) (snd x)) == (bl (apply paccs ag) (snd y)))
                            then True else False

buildPrdMap ::(Ord a) => ComM a -> Map.Map (a,a) Int
buildPrdMap (Com worlds ags val obs accs) =
    Map.fromList $ zip worlds [1.. length worlds]

returnState :: (Ord a, Show a) => EpExpM a -> PrtM a -> (a,a) -> Int
returnState m p tup = xtractInt $ Map.lookup tup sMap
    where sMap = buildPrdMap $ prtProduct' m p

xtractInt :: Maybe Int -> Int
xtractInt t = case t of
  Just a -> a
  Nothing -> error "No valid value"

convertCom :: (Ord a) => ComM a -> EpExpM Int
convertCom c@(Com worlds ags val obs accs) = (Mo worlds' ags val' obs' accs') where
    sMap    = buildPrdMap c
    worlds' = [xtractInt $ Map.lookup f sMap | f <- worlds]
    val'    = [(xtractInt $ Map.lookup (fst f) sMap, snd f) | f <- val ]
    obs'    = [(xtractInt $ Map.lookup (fst f) sMap, snd f) | f <- obs ]
    accs'   = [(fst f, nubPart (tupleListToErel (snd f) sMap)) | f <- accs]

prtProduct :: (Eq a,Show a, Ord a) => EpExpM a -> PrtM a -> EpExpM Int
prtProduct m p = convertCom $ prtProduct' m p

tupleListToErel :: (Ord a) => [((a,a),(a,a))] -> Map.Map (a,a) Int -> Erel Int
tupleListToErel lst sMap = fusion $ tupleListToList
                           [ (xtractInt $ Map.lookup (fst f) sMap,
                             xtractInt $ Map.lookup (snd f) sMap)
                             | f <- lst]

nubPart :: Erel Int -> Erel Int
nubPart l = map (nub) l

tupleToList :: (Int,Int) -> [Int]
tupleToList tup = [fst tup, snd tup]

tupleListToList :: [(Int,Int)] -> [[Int]]
tupleListToList = map tupleToList