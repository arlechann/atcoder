import Control.Monad (replicateM)
import Data.List (group, sort)

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
main = readInt >>= \n -> replicateM n readInt >>= \d -> print $ solve n d

solve :: Int -> [Int] -> Int
solve n d = length $ group $ sort d
