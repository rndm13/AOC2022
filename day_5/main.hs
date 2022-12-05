{-# LANGUAGE TemplateHaskell, RankNTypes, DerivingStrategies #-}

module Main where

import qualified Data.List.Split as LS
import qualified Data.List       as L
import Data.Char (isAlpha)
import Control.Lens

data Crate = Crate Char
           | EmptySpace
           deriving (Eq)

instance Read Crate where
  readsPrec _ input = 
    if cur == "   "
      then [(EmptySpace, rest)]
      else [(Crate (cur !! 1), rest)]
        where 
          cur  = take 3 input
          rest = drop 4 input

instance Show Crate where
  show (Crate c) = [c]
  show EmptySpace = " " 

dropr :: Int -> [a] -> [a]
dropr n arr = take (length arr - n) arr

safeHead :: [a] -> Maybe a
safeHead []  = Nothing
safeHead arr = Just (head arr)

doMoveWith :: ([Crate] -> [Crate]) -> [Int] -> [[Crate]] -> [[Crate]]
doMoveWith func [count, from, to] crates = newCrates
  where
    toMove = take count (crates !! (from - 1))
    newCrates = crates & (over (ix (from - 1)) (drop count)) & (over (ix (to - 1)) (func toMove ++))

solveWith :: ([Int] -> [[Crate]] -> [[Crate]]) -> [[Crate]] -> [[Int]] -> [[Crate]]
solveWith mover crates moves = foldr (mover) crates (reverse moves)

printTops :: [[Crate]] -> IO()
printTops = putStrLn . concat . (map (show . maybe EmptySpace id . safeHead))

main :: IO ()
main = do
  input <- ((,) <$> (reverse . (map (L.dropWhile (==EmptySpace))) . L.transpose . 
                    ((map ((map (read :: String -> Crate)) . reverse . LS.chunksOf 4)) . dropr 1 . head))
                <*> (map (map (read :: String -> Int) . filter (not . null) . LS.splitWhen (==' ') . filter (not . isAlpha))) . last)
                      . LS.splitWhen null . lines <$> getContents
  printTops . (uncurry (solveWith $ doMoveWith reverse)) $ input
  printTops . (uncurry (solveWith $ doMoveWith id)) $ input
