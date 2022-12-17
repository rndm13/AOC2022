{-# LANGUAGE DerivingStrategies, MultiWayIf #-}

import Control.Arrow
import Data.Ord (comparing)
import qualified Data.List as L
import qualified Data.List.Split as LS

data MixedList = Single Int
               | List [MixedList]
               deriving stock Eq

instance Read MixedList where
  readsPrec _ i 
    | head i /= '[' = (Single *** id) <$> (reads :: ReadS Int) i
    | otherwise     = (List *** id) <$> (readList :: ReadS [MixedList]) i

instance Show MixedList where
  show (Single i) = show i
  show (List   i) = show i

instance Ord MixedList where
  compare (Single a) (Single b) = compare a b
  compare a@(Single _) b = compare (List [a]) b
  compare a b@(Single _) = compare a (List [b]) 
  compare (List a) (List b) = foldr (<>) (comparing (length) a b) . zipWith compare a $ b

solveP1 :: [[MixedList]] -> Int
solveP1 =
  sum . 
  (zipWith (\i b -> i * (fromEnum b)) [1..]) .
  (map (uncurry (<))) .
  (map (head &&& last))

readMS :: String -> MixedList
readMS = read

dividers :: [MixedList]
dividers = [readMS "[[2]]", readMS "[[6]]"]

solveP2 :: [[MixedList]] -> Int
solveP2 =
  product .
  map (fst) . 
  filter (\(_, b) -> b `elem` dividers) . 
  zip [1..] . 
  L.sort . 
  (dividers <>) .
  concat 

main :: IO ()
main = do
  input <- (map (map readMS)) .
            map (take 2) . 
            LS.chunksOf 3 . 
            lines <$> getContents 
  print . (solveP1 &&& solveP2) $ input
