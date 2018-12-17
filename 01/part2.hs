import qualified Data.Set                      as Set

readInt :: String -> Int
readInt (x : xs) | x == '+' = read xs
                 | x == '-' = read ('-' : xs)

findFirstRepeat
    :: (Set.Set Int, Int, Maybe Int) -> Int -> (Set.Set Int, Int, Maybe Int)
findFirstRepeat (s, freq, _) i = if Set.member newFreq s
          then (Set.empty, 0, Just newFreq)
          else (Set.insert newFreq s, newFreq, Nothing)
    where newFreq = freq + i

haveNotFoundRepeat :: (Set.Set Int, Int, Maybe Int) -> Bool
haveNotFoundRepeat (_, _, Nothing) = True
haveNotFoundRepeat _               = False

main = do
    input <- readFile "./Input.txt"
    let (_, _, Just answer) =
            head
                . dropWhile haveNotFoundRepeat
                . scanl findFirstRepeat (Set.empty, 0, Nothing)
                . cycle
                . map readInt
                . lines
                $ input
    print answer

