import qualified Data.Set as Set

readInt :: String -> Int
readInt (x:xs)
    | x == '+' = read xs
    | x == '-' = read ('-':xs)

findFirstFreq :: [Int] -> Int
findFirstFreq xs = findFirstFreq' xs (Set.empty, 0, Nothing)

findFirstFreq' :: [Int] -> (Set.Set Int, Int, Maybe Int) -> Int
findFirstFreq' xs (set, freq, Just x) = x
findFirstFreq' xs (set, freq, Nothing) =
    let result = foldl processFreq (set, freq, Nothing) xs
    in findFirstFreq' xs result

processFreq :: (Set.Set Int, Int, Maybe Int) -> Int -> (Set.Set Int, Int, Maybe Int)
processFreq (set, freq, Just x) y = (set, freq, Just x)
processFreq (set, freq, Nothing) x = 
    if Set.member newFreq set then (set, freq, Just newFreq) else (Set.insert newFreq set, newFreq, Nothing)
        where newFreq = freq + x

main = do
    input <- readFile "./Input.txt"
    let answer = findFirstFreq . map readInt . lines $ input
    print answer

