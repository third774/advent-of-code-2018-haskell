
readInt :: String -> Int
readInt (x:xs)
    | x == '+' = read xs
    | x == '-' = read ('-':xs)

main = do
    input <- readFile "./Input.txt"
    let answer = sum . map readInt . lines $ input
    print answer
