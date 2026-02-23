import Control.Monad

type Time = Int
type Point = (Int, Int)
type Plan = (Time, Point)

planTime :: Plan -> Time
planTime = fst

planPoint :: Plan -> Point
planPoint = snd

pointX :: Point -> Int
pointX = fst

pointY :: Point -> Int
pointY = snd

timeParity :: Time -> Int
timeParity t = t `mod` 2

pointParity :: Point -> Int
pointParity p = uncurry (+) p `mod` 2

isValidParity :: Plan -> Bool
isValidParity p = (timeParity . planTime) p == (pointParity . planPoint) p

diff :: Int -> Int -> Int
diff a b = abs $ a - b

interval :: Time -> Time -> Time
interval t1 t2 = t2 - t1

intervals :: [Time] -> [Time]
intervals ts = zipWith (-) ts (0:ts)

distance :: Point -> Point -> Int
distance p1 p2 = diff (pointX p1) (pointX p2) + diff (pointY p1) (pointY p2)

distances :: [Point] -> [Int]
distances ps = zipWith distance ((0, 0) : ps) ps

readPlan :: IO Plan
readPlan = parse . map read . words <$> getLine
  where
    parse :: [Int] -> Plan
    parse [t, x, y] = (t, (x, y))
    parse _ = undefined

readInt :: IO Int
readInt = read <$> getLine

yn :: Bool -> String
yn True = "Yes"
yn False = "No"

main :: IO ()
main = readInt
  >>= \n -> replicateM n readPlan
  >>= \plans -> putStrLn $ yn $ all isValidParity plans && all (uncurry (>=)) (zip (intervals $ map planTime plans) (distances $ map planPoint plans))
