import Data.List (sortBy)

readInt :: IO Int
readInt = read <$> getLine

readInts :: IO [Int]
readInts = map read . words <$> getLine

chunks :: Int -> [a] -> [[a]]
chunks n x = rec x []
  where
    rec [] acc = reverse acc
    rec x acc = let (x1, x2) = splitAt n x in rec x2 (x1 : acc)

addLast :: [a] -> a -> [a]
-- addLast a b = a ++ [b]
addLast = (. (: [])) . (++)

main :: IO ()
main = readInt >>= \n -> readInts >>= \a -> print $ solve n a

solve :: Int -> [Int] -> Int
solve n a = solve' n (addLast (sortBy (flip compare) a) 0)
  where
    solve' n a = foldr (\[a, b] acc -> acc + a - b) 0 $ filter ((== 2) . length) $ chunks 2 a
