import qualified Data.Map                      as Map
import qualified Data.Bifunctor                as Bifunctor

incrementKey :: Ord a => a -> Map.Map a Int -> Map.Map a Int
incrementKey k m = if Map.member k m
   then Map.insert k ((m Map.! k) + 1) m
   else Map.insert k 1 m

hasNRepeats :: Int -> String -> Bool
hasNRepeats n =
    elem n . Map.elems . foldl (\m x -> incrementKey x m) Map.empty

produceCounts :: String -> (Int, Int)
produceCounts s = (mapToInt hasTwo, mapToInt hasThree)
  where
    hasTwo   = hasNRepeats 2 s
    hasThree = hasNRepeats 3 s

mapToInt :: Bool -> Int
mapToInt x = if x then 1 else 0

sumTuples :: [(Int, Int)] -> (Int, Int)
sumTuples = foldl (\(x, y) (a, b) -> (x + a, y + b)) (0, 0)

checksum :: String -> Int
checksum =
    (\(x, y) -> x * y)
        . sumTuples
        . map produceCounts
        . lines

main = do
    input <- readFile "./Input.txt"
    print $ checksum input
