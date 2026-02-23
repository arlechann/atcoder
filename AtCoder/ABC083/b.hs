readInt :: IO Int
readInt = read <$> getLine

readInts :: IO [Int]
readInts = map read . words <$> getLine

main :: IO ()
main = readInts
  >>= \[n, a, b] -> print $ solve n a b

solve :: Int -> Int -> Int -> Int
solve n a b = sum . map fst . filter (\(_, x) -> a <= x && x <= b) . map (\n -> (n, digitSum n)) $ [1..n]
  where
    digitSum 0 = 0
    digitSum n = digitSum (n `div` 10) + n `mod` 10
