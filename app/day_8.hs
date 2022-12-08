{-# LANGUAGE TemplateHaskell #-}

import           Data.Char       (digitToInt)
import qualified Data.List       as L
import qualified Data.Monoid     as Mi
import           Utils
import           Control.Arrow   ((&&&)) 

getRotations :: [[a]] -> [[[a]]]
getRotations arr = [arr, reverse <$> arr, L.transpose arr, reverse <$> L.transpose arr]

undoRotations :: [[[a]]] -> [[[a]]]
undoRotations [left, right, top, bottom] = [left, reverse <$> right, L.transpose top, L.transpose $ reverse <$> bottom]

getVisibility :: [Int] -> [Bool]
getVisibility arr = zipWith (>) arr . scanl max (-1) $ arr

getScores :: [Int] -> [Int] 
getScores arr = tail . scanr (\a s -> if a then 1 else s + 1) 0 . zipWith (>=) arr . scanl max (-1) $ arr

parseInput :: String -> [[Int]]
parseInput = ((digitToInt <$>) <$>) . lines

solveP1 :: [[Int]] -> Int
solveP1 = 
  (count id) .
  (Mi.getAny . foldMap Mi.Any <$>) .
  L.transpose .
  (concat <$>) .
  undoRotations .
  ((getVisibility <$>) <$>) .
  getRotations

solveP2 :: [[Int]] -> Int
solveP2 = 
  maximum .
  (product <$>) .
  L.transpose .
  (concat <$>) .
  undoRotations .
  ((getScores <$>) <$>) .
  getRotations

main :: IO ()
main =
  parseInput <$> getContents >>=
  print . (solveP1 &&& solveP2)
