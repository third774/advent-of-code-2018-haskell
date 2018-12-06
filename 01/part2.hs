import qualified Data.Set                      as Set

readInt :: String -> Int
readInt (x : xs) | x == '+' = read xs
                 | x == '-' = read ('-' : xs)

findFirstFreqRepeat :: [Int] -> Int
findFirstFreqRepeat xs = findFirstFreqRepeat' xs (Set.empty, 0, Nothing)

findFirstFreqRepeat' :: [Int] -> (Set.Set Int, Int, Maybe Int) -> Int
findFirstFreqRepeat' xs (set, freq, Just x) = x
findFirstFreqRepeat' xs (set, freq, Nothing) =
    let result = foldl processFreq (set, freq, Nothing) xs
    in  findFirstFreqRepeat' xs result

processFreq
    :: (Set.Set Int, Int, Maybe Int) -> Int -> (Set.Set Int, Int, Maybe Int)
processFreq (set, freq, Just x ) y = (set, freq, Just x)
processFreq (set, freq, Nothing) x = if Set.member newFreq set
    then (set, freq, Just newFreq)
    else (Set.insert newFreq set, newFreq, Nothing)
    where newFreq = freq + x

main = do
    input <- readFile "./Input.txt"
    let answer = findFirstFreqRepeat . map readInt . lines $ input
    print answer

