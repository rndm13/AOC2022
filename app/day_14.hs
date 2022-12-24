{-# LANGUAGE DerivingStrategies #-}

import qualified Data.Maybe as Mb
import Data.Char (isDigit)
import qualified Data.Map.Lazy as HM
import qualified Data.List as L
import qualified Data.List.Split as LS
import Control.Arrow
import Utils

data Point = Point 
  { _x :: Int
  , _y :: Int
  } deriving stock (Show, Eq, Ord)

readI :: String -> Int
readI = read

readP :: String -> Path
readP = read

instance Read Point where
  readsPrec _ i = [(Point x y, rest)]
    where x = readI . takeWhile (/=';') $ i
          y = readI . takeWhile (isDigit) . tail . dropWhile (/=';') $ i
          rest = dropWhile (isDigit) . tail . dropWhile (isDigit) $ i

data Path = Path 
  { _points :: [Point]
  } deriving stock Show

replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace search replacement original = L.intercalate replacement . LS.splitOn search $ original

instance Read Path where
  readsPrec _ i = (Path *** id) <$> (readList :: ReadS [Point]) repText
    where repText = '[':(replace " -> " "," . replace "," ";" $ i ++ "]")

fillPath :: Path -> Path
fillPath (Path points) = Path ((head points):(concat . zipWith (makeLine) points . tail $ points))

makeLine :: Point -> Point -> [Point]
makeLine p1 p2 = p2:(takeWhile (/=p2) . tail . iterate (\(Point x y) -> Point (x + dx) (y + dy)) $ p1)
          where dx = signum $ (_x p2) - (_x p1)
                dy = signum $ (_y p2) - (_y p1)

toCaveView :: [Path] -> HM.Map Int [Int]
toCaveView paths = (fmap L.sort) . HM.fromList . map (_x . head &&& map _y) . L.groupBy (sameX) . L.sort $ paths >>= _points

sameX :: Point -> Point -> Bool
sameX (Point x1 _) (Point x2 _) = x1 == x2

simSand :: HM.Map Int [Int] -> Point -> Maybe Point 
simSand cave (Point sandX sandY) = sand
  where dropSand x y = (-1+) <$> (HM.lookup x cave >>= L.find (>= y))
        mDrop = dropSand sandX sandY
        lDrop = mDrop >>= simSand cave . Point (sandX - 1) . (1+)
        rDrop = mDrop >>= simSand cave . Point (sandX + 1) . (1+)
        openBlock x y = Mb.isNothing (HM.lookup x cave >>= L.find (== y))
        sand
          | not . openBlock sandX $ sandY = Nothing
          | any id $ openBlock (sandX - 1) . (+1) <$> mDrop = lDrop
          | any id $ openBlock (sandX + 1) . (+1) <$> mDrop = rDrop
          | otherwise = Point sandX <$> mDrop

updateCave :: Maybe (HM.Map Int [Int]) -> Point -> Maybe (HM.Map Int [Int]) 
updateCave cave p  
  | Mb.isJust sand = HM.insertWith (\(a:_) b -> L.insert a b) (_x jSand) (L.singleton . _y $ jSand) <$> cave
  | otherwise = Nothing
  where sand = simSand (Mb.fromJust cave) p
        jSand = Mb.fromJust sand

solve :: HM.Map Int [Int] -> Int
solve cave = 
  (-1+) .
  length . 
  takeWhile (Mb.isJust) . 
  scanl (updateCave) (Just cave) $ repeat (Point 500 0)

addFloor :: [Path] -> [Path]
addFloor paths = (Path [Point 0 (botY), Point 1000 (botY)]):paths
  where botY = (+2) . maximum $ (paths >>= (fmap (_y) . _points))

main :: IO ()
main = do
  paths <- map (readP) . lines <$> getContents
  print . toBoth (solve . toCaveView . map fillPath) (id &&& addFloor) $ paths  
