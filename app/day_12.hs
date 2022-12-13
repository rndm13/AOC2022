{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiWayIf         #-}

import           Data.Char   (ord)
import qualified Data.List   as L
import qualified Data.Matrix as Mx
import qualified Data.Maybe  as Mb
import Control.Arrow

data Cell = Height Int
          | Start
          | End
          | Unavailable 
          deriving stock (Show, Eq, Ord)

instance Read Cell where
  readsPrec _ i = [(c,tail i)]
    where c = if | head i == 'S' -> Start
                 | head i == 'E' -> End
                 | otherwise     -> Height ((ord . head $ i) - (ord 'a'))

readC :: Char -> Cell
readC c = (read :: String -> Cell) . L.singleton $ c

newtype ICell = ICell ((Int,Int), Cell) deriving stock (Eq, Show)

--                          Height map       Start   Elev 0
readHeightMap :: String -> (Mx.Matrix ICell, ICell, [ICell])
readHeightMap input = (m, s, e)
  where m = Mx.mapPos (\pos c -> ICell (pos, readC c)) . Mx.fromLists . lines $ input
        s = (maybe (error "Start not found") id) . L.find (\(ICell (_, c)) -> Start == c) . Mx.toList $ m
        e = filter (\(ICell (_, c)) -> getElevation c == 0) . Mx.toList $ m

getElevation :: Cell -> Int
getElevation Unavailable = 999
getElevation Start       = 0
getElevation End         = 25
getElevation (Height x)  = x

canMove :: Cell -> Cell -> Bool
canMove c1 c2 = e2 - e1 <= 1
  where e1 = getElevation c1
        e2 = getElevation c2

movement :: [(Int, Int)]
movement = [(0,1), (0,-1),
            (1,0), (-1,0)]

addPoints :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPoints (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

getCell :: ICell -> Cell
getCell (ICell (_, c)) = c

getPos  :: ICell -> (Int, Int)
getPos (ICell (p, _)) = p

getConnections :: Mx.Matrix ICell -> (Int, Int) -> [(Int, Int)]
getConnections matr pos = 
  (filter (\p -> canMove (getCell node) (getCell $ matr Mx.! p))) . 
  (map getPos) .
  Mb.catMaybes .
  map ((\(x,y) -> Mx.safeGet x y $ matr) . addPoints pos) $ movement 
    where node = matr Mx.! pos

bfs :: Mx.Matrix ICell -> [ICell] -> Int
bfs matr curCells 
  | any (\(ICell (_, c)) -> c == End) curCells = 0
  | null newCells = 999999999
  | otherwise = 1 + (bfs newMatr newCells)
  where newMatr  = foldr (\(ICell (p,_)) m -> Mx.unsafeSet (ICell (p,Unavailable)) (p) m) matr curCells
        newCells = L.nub . filter (`notElem` curCells) . foldr (\x s -> s <> (map (matr Mx.!) . getConnections matr . getPos $ x)) [] $ curCells

main :: IO ()
main = do
  (input, start, e0) <- readHeightMap <$> getContents
  print . (bfs input *** bfs input) $ ([start], e0)
