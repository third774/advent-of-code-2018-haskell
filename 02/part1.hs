import qualified Data.Map                      as Map

incrementKey :: Ord a => Map.Map a Int -> a -> Map.Map a Int
incrementKey m k = if Map.member k m
    then Map.insert k ((m Map.! k) + 1) m
    else Map.insert k 1 m

hasNRepeats :: Int -> String -> Bool
hasNRepeats n = elem n . Map.elems . foldl incrementKey Map.empty

produceCounts :: String -> (Int, Int)
produceCounts s = (mapToInt hasTwo, mapToInt hasThree)
  where
    hasTwo   = hasNRepeats 2 s
    hasThree = hasNRepeats 3 s

mapToInt :: Bool -> Int
mapToInt x = if x then 1 else 0

addTuples :: Num a => (a, a) -> (a, a) -> (a, a)
addTuples (x, y) (a, b) = (x + a, y + b)

sumTuples :: [(Int, Int)] -> (Int, Int)
sumTuples = foldl addTuples (0, 0)

multiplyTupleValues :: Num a => (a, a) -> a
multiplyTupleValues (a, b) = a * b

checksum :: String -> Int
checksum = multiplyTupleValues . sumTuples . map produceCounts . lines

main = do
    input <- readFile "./Input.txt"
    print $ checksum input
