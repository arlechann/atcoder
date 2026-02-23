dayDream :: String -> Bool
dayDream [] = True
dayDream ('m':'a':'e':'r':'d':xs) = dayDream xs
dayDream ('r':'e':'m':'a':'e':'r':'d':xs) = dayDream xs
dayDream ('e':'s':'a':'r':'e':xs) = dayDream xs
dayDream ('r':'e':'s':'a':'r':'e':xs) = dayDream xs
dayDream _ = False

main :: IO ()
main = getLine
  >>= \s -> putStrLn $ if (dayDream . reverse) s then "YES" else "NO"
