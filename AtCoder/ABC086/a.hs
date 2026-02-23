readIntegers :: IO [Int]
readIntegers = map read . words <$> getLine

main :: IO ()
main = readIntegers >>= \[a, b] -> putStrLn $ if odd (a * b) then "Odd" else "Even"
