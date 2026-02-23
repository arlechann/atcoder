main :: IO ()
main = getLine >>= \line -> print $ count '1' line

count :: Char -> String -> Int
count char line = rec line 0
  where
    rec [] c = c
    rec (s : sx) c = rec sx (c + if s == char then 1 else 0)
