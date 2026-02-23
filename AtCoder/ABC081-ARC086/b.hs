import Data.Maybe (isJust)
import Data.List (find)

readInteger :: IO Int
readInteger = read <$> getLine

readIntegers :: IO [Int]
readIntegers = map read . words <$> getLine

main :: IO ()
main = readInteger >>= \n -> readIntegers >>= \a -> print $ solve n a

solve :: Int -> [Int] -> Int
solve n a = rec a 0
  where
    rec a x = if isJust $ find odd a then x else rec (map (`div` 2) a) (succ x)
