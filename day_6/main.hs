import qualified Data.List     as L
import           Control.Arrow

allDifferent :: (Ord a) => [a] -> Bool
allDifferent []  = True
allDifferent (a:nextArr) = notElem a nextArr && (allDifferent nextArr) 

solve :: Int -> String -> Int
solve lngth str =
  maybe lngth (+lngth) . L.findIndex (allDifferent) 
  . take (length str - lngth) . scanr (\x a -> x:(take (lngth - 1) a)) [] $ str

main :: IO ()
main = do
  input <- getLine
  print . (solve 4 &&& solve 14) $ input
