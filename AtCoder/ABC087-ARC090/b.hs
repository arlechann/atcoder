readInt :: IO Int
readInt = read <$> getLine

readInts :: IO [Int]
readInts = map read . words <$> getLine

main :: IO ()
main = readInt
  >>= \a -> readInt
  >>= \b -> readInt
  >>= \c -> readInt
  >>= \x -> print $ solve a b c x

solve :: Int -> Int -> Int -> Int -> Int
solve a b c x = length $ filter (\(a, b, c) -> isExact a b c) $ [0..a] >>= \a -> [0..b] >>= \b -> [0..c] >>= \c -> [(a, b, c)]
  where
    isExact a b c = a * 500 + b * 100 + c * 50 == x
