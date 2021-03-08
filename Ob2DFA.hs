-- Acknowledgement: Malvin Gattinger

module Ob2DFA where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Algorithm.Search (dfs)
import Data.Char
import System.Process
import ObExpr

data ObTree = E Int
            | L Char Int
            | A ObTree ObTree Int
            | C ObTree ObTree Int
            | S ObTree Int
        deriving (Show, Eq)

reNumber :: ObTree ->  Int -> ObTree
reNumber (E n) l = E (n + l)
reNumber (L c n) l = L c (n + l)
reNumber (A t1 t2 n) l = A t1' t2' (n + l)
    where t1' = reNumber t1 l
          t2' = reNumber t2 l
reNumber (C t1 t2 n) l = C t1' t2' (n + 1)
    where t1' = reNumber t1 l
          t2' = reNumber t2 l
reNumber (S t n) l = S t' (n + 1)
    where t' = reNumber t l

buildTreeHelper :: ObExpr -> Int -> (Int, ObTree)
buildTreeHelper ObEps n = (1, E 1)
buildTreeHelper (ObLit c) n = (1, L c 1)
buildTreeHelper (ObAlt r1 r2) n = (n1 + n2 + 1, reNumber sub_tree n)
    where (n1, t1) = buildTreeHelper r1 0
          (n2, t2) = buildTreeHelper r2 0
          t2' = reNumber t2 n1
          sub_tree = A t1 t2' (n1 + n2 + 1)
buildTreeHelper (ObCat r1 r2) n = (n1 + n2 + 1, reNumber sub_tree n)
    where (n1, t1) = buildTreeHelper r1 0
          (n2, t2) = buildTreeHelper r2 0
          t2' = reNumber t2 n1
          sub_tree = C t1 t2' (n1 + n2 + 1)
buildTreeHelper (ObStar r) n = (n1 + 1, reNumber sub_tree n)
    where (n1, t1) = buildTreeHelper r 0
          sub_tree = S t1 (n1 + 1)

buildTree :: ObExpr -> ObTree
buildTree r = t
  where (_, t) = buildTreeHelper r 0

getNodeNumber :: ObTree -> Int
getNodeNumber (E n) = n
getNodeNumber (L c n) = n
getNodeNumber (C t1 t2 n) = n
getNodeNumber (A t1 t2 n) = n
getNodeNumber (S t n) = n

buildNullableHelper :: ObTree -> Map.Map Int Bool -> Map.Map Int Bool
buildNullableHelper (E n) m = Map.insert n True m
buildNullableHelper (L c n) m = Map.insert n False m
buildNullableHelper (A t1 t2 n) m = Map.insert n (b1 || b2) m2
    where m1 = buildNullableHelper t1 m
          m2 = buildNullableHelper t2 m1
          n1 = getNodeNumber t1
          n2 = getNodeNumber t2
          b1 = lookupNullable m2 n1
          b2 = lookupNullable m2 n2
buildNullableHelper (C t1 t2 n) m = Map.insert n (b1 && b2) m2
    where m1 = buildNullableHelper t1 m
          m2 = buildNullableHelper t2 m1
          n1 = getNodeNumber t1
          n2 = getNodeNumber t2
          b1 = lookupNullable m2 n1
          b2 = lookupNullable m2 n2
buildNullableHelper (S t n) m = Map.insert n True m1
    where m1 = buildNullableHelper t m

buildNullable :: ObTree -> Map.Map Int Bool
buildNullable t = buildNullableHelper t Map.empty

buildFirstHelper :: ObTree -> Map.Map Int Bool -> Map.Map Int [Int] -> Map.Map Int [Int]
buildFirstHelper (E n) nMap m = Map.insert n [] m
buildFirstHelper (L c n) nMap m = Map.insert n [n] m
buildFirstHelper (A t1 t2 n) nMap m = Map.insert n (f1 ++ f2) m2
    where m1 = buildFirstHelper t1 nMap m
          m2 = buildFirstHelper t2 nMap m1
          f1 = lookupFirst m2 (getNodeNumber t1)
          f2 = lookupFirst m2 (getNodeNumber t2)
buildFirstHelper (C t1 t2 n) nMap m = if (b1) then Map.insert n (f1 ++ f2) m2 else Map.insert n f1 m2
    where m1 = buildFirstHelper t1 nMap m
          m2 = buildFirstHelper t2 nMap m1
          f1 = lookupFirst m2 (getNodeNumber t1)
          f2 = lookupFirst m2 (getNodeNumber t2)
          b1 = lookupNullable nMap (getNodeNumber t1)
buildFirstHelper (S t n) nMap m = Map.insert n f1 m1
    where m1 = buildFirstHelper t nMap m
          f1 = lookupFirst m1 (getNodeNumber t)

buildFirst :: ObTree -> Map.Map Int [Int]
buildFirst t = buildFirstHelper t (buildNullable t) Map.empty

buildLastHelper :: ObTree -> Map.Map Int Bool -> Map.Map Int [Int] -> Map.Map Int [Int]
buildLastHelper (E n) nMap m = Map.insert n [] m
buildLastHelper (L c n) nMap m = Map.insert n [n] m
buildLastHelper (A t1 t2 n) nMap m = Map.insert n (f1 ++ f2) m2
    where m1 = buildLastHelper t1 nMap m
          m2 = buildLastHelper t2 nMap m1
          f1 = lookupLast m2 (getNodeNumber t1)
          f2 = lookupLast m2 (getNodeNumber t2)
buildLastHelper (C t1 t2 n) nMap m = if (b2) then Map.insert n (f1 ++ f2) m2 else Map.insert n f2 m2
    where m1 = buildLastHelper t1 nMap m
          m2 = buildLastHelper t2 nMap m1
          f1 = lookupLast m2 (getNodeNumber t1)
          f2 = lookupLast m2 (getNodeNumber t2)
          b2 = lookupNullable nMap (getNodeNumber t2)
buildLastHelper (S t n) nMap m = Map.insert n f1 m1
    where m1 = buildLastHelper t nMap m
          f1 = lookupLast m1 (getNodeNumber t)

buildLast :: ObTree -> Map.Map Int [Int]
buildLast t = buildLastHelper t (buildNullable t) Map.empty

buildFollowHelper :: ObTree -> Map.Map Int [Int] -> Map.Map Int [Int] -> Map.Map Int [Int] -> Map.Map Int [Int]
buildFollowHelper (C t1 t2 n) firstPos lastPos followPos = updateMap followPos2 first2 last1
    where n1 = getNodeNumber t1
          n2 = getNodeNumber t2
          last1 = lookupLast lastPos n1
          first2 = lookupFirst firstPos n2
          followPos1 = buildFollowHelper t1 firstPos lastPos followPos
          followPos2 = buildFollowHelper t2 firstPos lastPos followPos1 
buildFollowHelper (S t1 n) firstPos lastPos followPos = updateMap followPos1 first1 last1
    where n1 = getNodeNumber t1
          last1 = lookupLast lastPos n1
          first1 = lookupFirst firstPos n1
          followPos1 = buildFollowHelper t1 firstPos lastPos followPos
buildFollowHelper (A t1 t2 n) firstPos lastPos followPos = followPos2
    where followPos1 = buildFollowHelper t1 firstPos lastPos followPos
          followPos2 = buildFollowHelper t2 firstPos lastPos followPos1
buildFollowHelper _ _ _ followPos = followPos

updateMap :: Map.Map Int [Int] -> [Int] -> [Int] -> Map.Map Int [Int]
updateMap followPos first (x:xs) = updateMap (Map.alter alterFn x followPos) first xs
    where alterFn (Just v) = Just (v ++ first)
          alterFn Nothing = Just first
updateMap followPos first [] = followPos

buildFollow :: ObTree -> Map.Map Int [Int]
buildFollow t = buildFollowHelper t (buildFirst t) (buildLast t) Map.empty

buildSymbolMapHelper :: ObTree -> Map.Map Char [Int] -> Map.Map Char [Int]
buildSymbolMapHelper (L c n) m = Map.alter alterFn c m
    where alterFn (Just v) = Just (n:v)
          alterFn Nothing = Just [n]
buildSymbolMapHelper (C t1 t2 n) m = buildSymbolMapHelper t2 m1
    where m1 = buildSymbolMapHelper t1 m
buildSymbolMapHelper (A t1 t2 n) m = buildSymbolMapHelper t2 m1
    where m1 = buildSymbolMapHelper t1 m
buildSymbolMapHelper (S t n) m = buildSymbolMapHelper t m
buildSymbolMapHelper _ m = m

buildSymbolMap :: ObTree -> Map.Map Char [Int]
buildSymbolMap t = buildSymbolMapHelper t Map.empty

symbolMapLookup :: Map.Map Char [Int] -> Char -> [Int]
symbolMapLookup m c = case Map.lookup c m of
                      Just l -> l
                      otherwise -> error "symbolMapLookup: symbol not found"

findAllP_to_a :: [Int] -> [Int] -> [Int]
findAllP_to_a (x:xs) aList = if (x `elem` aList) then x : (findAllP_to_a xs aList) else findAllP_to_a xs aList
findAllP_to_a [] aList = []

getUnion_followPos :: [Int] -> Map.Map Int [Int] -> [Int]
getUnion_followPos xs m = (Set.toList . Set.fromList) (concat union)
    where union = getUnion_Helper xs m

getUnion_Helper :: [Int] -> Map.Map Int [Int] -> [[Int]]
getUnion_Helper (x:xs) m = (lookupFollow m x) : (getUnion_Helper xs m)
getUnion_Helper [] m = []

isNewState :: [Int] -> [[Int]] -> Bool
isNewState state dStates =  if (state `elem` dStates) then False else True

innerForLoop :: [Int] -> Map.Map Char [Int] -> Map.Map Int [Int] -> [Char] -> [[Int]] -> [Trans] -> ([[Int]], [Trans])
innerForLoop state symbolMap followPos symbols dStates dTrans = innerForLoopHelper state symbolMap followPos symbols dStates dTrans []

innerForLoopHelper :: [Int] -> Map.Map Char [Int] -> Map.Map Int [Int] -> [Char] -> [[Int]] -> [Trans] -> [[Int]] -> ([[Int]], [Trans])
innerForLoopHelper state symbolMap followPos (a:as) dStates dTrans accStates = if (bool_isNewState)
    then innerForLoopHelper state symbolMap followPos as dStates newTrans (unionFollowPos: accStates)
    else innerForLoopHelper state symbolMap followPos as dStates newTrans accStates
    where allCorres_a = findAllP_to_a state (symbolMapLookup symbolMap a)
          unionFollowPos = getUnion_followPos allCorres_a followPos
          bool_isNewState = isNewState unionFollowPos (dStates ++ accStates)
          newTrans = (state, a, unionFollowPos) : dTrans
innerForLoopHelper state symbolMap followPos [] dStates dTrans accStates = (accStates, dTrans)

outerWhileLoopHelper :: [[Int]] -> Map.Map Char [Int] -> Map.Map Int [Int] -> [Char] -> [[Int]] -> [Trans] -> ([[Int]], [Trans])
outerWhileLoopHelper (state: states) symbolMap followPos symbols dStates dTrans =
    outerWhileLoopHelper (states ++ newStates) symbolMap followPos symbols (dStates ++ newStates) newTrans
    where (newStates, newTrans) = innerForLoop state symbolMap followPos symbols dStates dTrans
outerWhileLoopHelper [] _ _ _ dStates dTrans = (dStates, dTrans)

outerWhileLoop :: [[Int]] -> Map.Map Char [Int] -> Map.Map Int [Int] -> [Char] -> [[Int]] -> ([[Int]], [Trans])
outerWhileLoop leftStates symbolMap followPos symbols dStates = outerWhileLoopHelper leftStates symbolMap followPos symbols dStates []

getStartState :: ObTree -> Map.Map Int [Int] -> [Int]
getStartState t firstPos = lookupFirst firstPos (getNodeNumber t)

buildStates_Trans :: [Int] -> Map.Map Char [Int] -> Map.Map Int [Int] -> [Char] -> ([[Int]], [Trans])
buildStates_Trans startState symbolMap followPos symbols = outerWhileLoop [startState] symbolMap followPos symbols [startState]

getFinalStates :: [[Int]] -> Int -> [[Int]]
getFinalStates states n = [state | state <- states, n `elem` state]

type Trans = ([Int], Char, [Int])

data ObDFA = ObD [Int] [[Int]] [Trans] [[Int]] [Char]

instance Show ObDFA where
    show = showDFA

buildDfa :: ObExpr -> ObDFA
buildDfa obX = ObD startState states trans finalStates symbols
    where auxObX     = obAugment obX
          obXTree    = buildTree auxObX
          nullable   = buildNullable obXTree
          firstPos   = buildFirst obXTree
          lastPos    = buildLast obXTree
          followPos  = buildFollow obXTree
          symbolMap  = buildSymbolMap obXTree
          symbols    = Map.keys symbolMap
          startState = getStartState obXTree firstPos
          (states, trans) = buildStates_Trans startState symbolMap followPos symbols
          finalStates = getFinalStates states ((getNodeNumber obXTree) - 1) 

midTripwhere :: ([Int],Char,[Int]) -> Char
midTripwhere (s1,c,s2) = c

fstTripwhere :: ([Int],Char,[Int]) -> [Int]
fstTripwhere (s1,c,s2) = s1

lstTripwhere :: ([Int],Char,[Int]) -> [Int]
lstTripwhere (s1,c,s2) = s2

returnLabel :: [Int] -> [Int] -> [Trans] -> Char
returnLabel u v t = if (filter ((== v).lstTripwhere) (filter ((== u).fstTripwhere) t)) /= []
  then midTripwhere (head (filter ((== v).lstTripwhere) (filter ((== u).fstTripwhere) t)))
  else '#'

filterMark :: ObDFA -> ObDFA
filterMark (ObD start states trans final symbol) = ObD start states trans' final symbol'
    where symbol' = filter (/= '#') symbol
          trans'  = filter ((/= '#').midTripwhere) trans

eliminateDeads :: ObDFA -> ObDFA
eliminateDeads (ObD start states trans final symbol) = ObD start states' trans' final symbol
    where states' = filter (/= []) states
          trans' = filter ((/= []).lstTripwhere) $ filter ((/= []).fstTripwhere) trans 

reachableStates :: ObDFA -> [Int] -> [[Int]]
reachableStates (ObD _ _ trans _ _) s =  Set.toList $ Set.fromList (map (lstTripwhere) (filter ((== s).fstTripwhere) trans))

toGraph :: ObDFA -> Map.Map [Int] [[Int]]
toGraph fa@(ObD _ states _ _ _) = Map.fromList [(u,reachableStates fa u) | u <- states]

findPath :: ObDFA -> Maybe [[Int]]
findPath fa@(ObD start states trans final symbols) = dfs (( toGraph fa) Map.!) (`elem` final) start

extractPath :: Maybe [[Int]] -> [[Int]]
extractPath path = case path of
                   Just p -> p
                   Nothing -> []

returnT :: ObDFA -> [Trans]
returnT fa@(ObD _ _ t _ _) = t

findString :: [Int] -> [[Int]] -> [Trans] -> String
findString s [] trans = []
findString s p trans = (findChar s (head p) trans) ++ (findString (head p) (tail p) trans)

findChar :: [Int] -> [Int] -> [Trans] -> String
findChar u v t = [head [c | (u,c,v) <- filter ((== v).lstTripwhere) $ filter ((== u).fstTripwhere) t ]]

findStringDFA :: ObDFA -> String
findStringDFA fa@(ObD start states trans finals symbols) = findString start path trans
  where path = extractPath $ findPath fa

findPartitions :: ObDFA -> [ObDFA]
findPartitions fa@(ObD start states trans final symbol) = [ObD start states trans [final'] symbol | final' <- filter (not.( `elem` final)) states ] ++ [fa]

printDfa :: ObDFA -> IO()
printDfa (ObD startState states trans finalStates symbols) =
  putStr ("Initial State: \n" ++ (show startState) ++ "\n\n" ++
      "States : \n" ++ (showStates states) ++ "\n\n" ++
      "Moves: \n" ++ (showMoves trans) ++ "\n\n" ++
      "Final States: \n" ++ (showStates finalStates) ++ "\n" ++
      "Symbols: \n" ++ show symbols)

showDFA :: ObDFA -> String
showDFA (ObD startState states trans finalStates symbols) =
  "Initial State: \n" ++ (show startState) ++ "\n\n" ++
  "States : \n" ++ (showStates states) ++ "\n\n" ++
  "Moves: \n" ++ (showMoves trans) ++ "\n\n" ++
  "Final States: \n" ++ (showStates finalStates) ++ "\n" ++
  "Symbols: \n" ++ show symbols


showStates :: [[Int]] -> String
showStates (x:xs) = (show x) ++ "\n" ++ showStates(xs)
showStates [] = []

showMoves :: [([Int], Char, [Int])] -> String
showMoves ((state, c, finalState):xs) =
  ((show state) ++ " on " ++ [c] ++ " = " ++ (show finalState) ++ "\n") ++ (showMoves xs)
showMoves [] = []

lookupNullable :: Map.Map Int Bool -> Int -> Bool
lookupNullable m n = case (Map.lookup n m) of
                     Just b -> b
                     otherwise -> error "helper: key not found for nullable"

lookupFirst :: Map.Map Int [Int] -> Int -> [Int]
lookupFirst m n = case (Map.lookup n m) of
                  Just l -> l
                  otherwise -> error "helper: key not found for first"

lookupLast :: Map.Map Int [Int] -> Int -> [Int]
lookupLast m n = case (Map.lookup n m) of
                 Just l -> l
                 otherwise -> error "helper: key not found for last"

lookupFollow :: Map.Map Int [Int] -> Int -> [Int]
lookupFollow m n = case (Map.lookup n m) of
                   Just l -> l
                   otherwise -> []

reNumberKeys :: Map.Map Int a -> Int -> Map.Map Int a
reNumberKeys m n = Map.mapKeys (\k -> k + n) m

reNumberKV :: Map.Map Int [Int] -> Int -> Map.Map Int [Int]
reNumberKV m n = Map.map (\v -> map (\x -> x + n) v) (reNumberKeys m n)

mapUnion :: Map.Map Int a -> Map.Map Int a -> Map.Map Int a
mapUnion m1 m2 = Map.union m1 m2

prettyPrintList :: Show a => [a] -> String
prettyPrintList [] = []
prettyPrintList(x:xs) = show(x) ++ ['\n'] ++ (prettyPrintList xs)

type PdTrans = (([Int],[Int]), Char, ([Int],[Int]))

data PdDFA = PdD ([Int],[Int]) [([Int],[Int])] [PdTrans] [([Int],[Int])] [Char] deriving Show

productDFA :: ObDFA -> ObDFA -> PdDFA
productDFA (ObD s1 sts1 t1 f1 sym1) (ObD s2 sts2 t2 f2 sym2) = PdD s3 sts3 t3 f3 sym3
    where s3   = (s1,s2)
          sts3 = [(u,v) | u <- sts1, v <- sts2]
          t3   = [(u, func u v, v) | u <- sts3, v <- sts3, (func u v) /= '#']
          f3   = Set.toList ((Set.fromList (filter ((`elem` f1).fst) sts3))
                          `Set.union`
                          ((Set.fromList (filter ((`elem` f2).snd) sts3))))
          sym3 = Set.toList ((Set.fromList sym1) `Set.union` (Set.fromList sym2)) 
          func x y = if returnLabel (fst x) (fst y) t1 == returnLabel (snd x) (snd y) t2 then returnLabel (fst x) (fst y) t1 else '#'

buildStateMap :: PdDFA -> Map.Map ([Int],[Int]) [Int]
buildStateMap (PdD start states trans final symbol) = Map.fromList $ zip states (map (func) [1.. length states])
    where func i = [i]

extractVal :: Maybe [Int] -> [Int]
extractVal t = case t of
               Just a -> a
               Nothing -> []

convertTrans :: PdTrans -> Map.Map ([Int],[Int]) [Int] -> Trans
convertTrans (u, c, v) tupleMap = (extractVal $ Map.lookup u tupleMap, c, extractVal $ Map.lookup v tupleMap)

convertDFA :: PdDFA -> ObDFA
convertDFA pdfa@(PdD start states trans final symbol) = (ObD start' states' trans' final' symbol)
    where tupleMap = buildStateMap pdfa
          start'   = extractVal $ Map.lookup start tupleMap
          states'  = [extractVal $ Map.lookup f tupleMap | f <- states ]
          trans'   = [convertTrans t tupleMap | t <- trans ]
          final'   = [extractVal $ Map.lookup f tupleMap | f <- final ]

convertProduct :: ObDFA -> ObDFA -> ObDFA
convertProduct d1 d2 = convertDFA $ productDFA d1 d2