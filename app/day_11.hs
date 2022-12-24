{-# LANGUAGE DerivingStrategies, TemplateHaskell, MultiWayIf #-}

import Control.Lens
import qualified Data.List as L
import qualified Data.List.Split as LS
import Control.Arrow
import Utils

data Monkey = Monkey
  { _id          :: Int
  , _items       :: [Int]
  , _operation   :: (Int -> Int)
  , _divisor     :: Int
  , _true        :: Int -- Id of monkey to throw when test is true
  , _false       :: Int -- when false
  , _inspections :: Int
  } 

makeLenses ''Monkey

monkeyOp :: String -> Int -> Int
monkeyOp [] _ = error "no string to turn into operation"
monkeyOp (_:str) val = (func val rhs)
  where noNew = drop 6 str
        oper  = noNew !! 4
        func  = if | oper == '+' -> (+)
                   | oper == '*' -> (*)
                   | otherwise -> error ("unknown operator '" <> [oper] <> "'")
        rstr  = getLastStr noNew
        rhs   = if | rstr == "old" -> val
                   | otherwise -> read rstr
        
monkeyDivisor :: String -> Int
monkeyDivisor [] = error "no string to turn into test"
monkeyDivisor (_:str) = rhs
  where rhs = read . getLastStr $ str

_test :: Monkey -> Int -> Bool
_test m x = (x `mod` (_divisor m)) /= 0

dropToCol :: String -> String
dropToCol = tail . dropWhile (/=':')

getLastStr :: String -> String
getLastStr = reverse . takeWhile (/=' ') . reverse

readI :: String -> Int
readI = read

instance Read Monkey where
  readsPrec _ i = [(Monkey i_ind i_items i_oper i_test i_true i_false 0, concat . drop 6 $ li)]
    where li       = lines i
          i_ind    = readI . getLastStr . init $ li !! 0
          i_items  = (readI <$>) . LS.splitWhen (==',') . dropToCol $ li !! 1
          i_oper   = monkeyOp . dropToCol $ li !! 2
          i_test   = monkeyDivisor . dropToCol $ li !! 3
          i_true   = readI . getLastStr $ li !! 4
          i_false  = readI . getLastStr $ li !! 5

instance Show Monkey where
  show m = "Monkey " <> (show . _id $ m) <> "(" <> (show . _true $ m) <> ", " <> (show . _false $ m) <> "): " <> (show . _items $ m)

tupToList :: (a,a) -> [a]
tupToList (a,b) = [a,b]

useMonkey :: Monkey -> [(Int,[Int])]
useMonkey m = zipIndex . L.partition ((_test m) . (_operation m)) $ (_items m)
  where zipIndex = tupToList . ((const (_false m) &&& (_operation m <$>)) *** (const (_true m) &&& (_operation m <$>)))

addUp :: (Int, [Int]) -> [[Int]] -> [[Int]]
addUp (ind, toAdd) = (ix ind) %~ (++ toAdd)

getItems :: [(Int,[Int])] -> [[Int]] -> [[Int]]
getItems ixLst initial = foldr addUp initial ixLst

updateMonkey :: [Monkey] -> Int -> [Monkey]
updateMonkey monkeys ind = setNewItems updMonkeys items2
  where items0 = _items <$> monkeys
        items1 = items0 & (ix ind) .~ []
        items2 = getItems (useMonkey $ monkeys !! ind)$ items1 
        updMonkeys = monkeys & (ix ind . inspections) %~ (+ (length . _items $ monkeys !! ind))

setNewItems :: [Monkey] -> [[Int]] -> [Monkey]
setNewItems monkeys newItems = zipWith (\m ni -> m & items .~ ni) monkeys newItems

updateMonkeys :: [Monkey] -> [Monkey]
updateMonkeys monkeys = foldl updateMonkey monkeys [0..(length monkeys - 1)]

solveN :: Int -> [Monkey] -> Int
solveN v monkeys = 
  $(makeListBinder 2) (*) . 
  take 2 . 
  reverse .
  L.sort .
  (map _inspections) $
  ((iterate (updateMonkeys) monkeys) !! v)

solveP1 :: [Monkey] -> Int
solveP1 input = solveN 20 monkeys
  where monkeys = map (operation %~ ((`div` 3) . )) input

solveP2 :: [Monkey] -> Int
solveP2 input = solveN 10000 monkeys
  where monkeys = map (operation %~ ((`mod` allLcm) . )) input
        allLcm  = foldr (\x s -> s `lcm` (_divisor x)) 1 input

main :: IO ()
main = do
  input <- ((read :: String -> Monkey) . L.intercalate "\n" <$>) . LS.chunksOf 7 . lines <$> getContents
  print . (solveP1 &&& solveP2) $ input
