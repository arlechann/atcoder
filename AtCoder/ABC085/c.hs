import Data.List (find)

readInt :: IO Int
readInt = read <$> getLine

readInts :: IO [Int]
readInts = map read . words <$> getLine

chunks :: Int -> [a] -> [[a]]
chunks n x = chunks' n x []
  where
    chunks' n [] acc = reverse acc
    chunks' n x acc = let (x1, x2) = splitAt n x in chunks' n x2 (x1 : acc)

addLast :: [a] -> a -> [a]
-- addLast a b = a ++ [b]
addLast = (. (: [])) . (++)

main :: IO ()
main = readInts >>= \[n, y] -> putStrLn $ case solve n y of
  Just (x, y, z) -> unwords $ map show [x, y, z]
  Nothing -> "-1 -1 -1"

solve :: Int -> Int -> Maybe (Int, Int, Int)
solve n yen = find (\(x, y, z) -> x * 10000 + y * 5000 + z * 1000 == yen) $ [0..n] >>= \x -> (\y -> (x, y, n - x - y)) <$> [0..(n - x)]
