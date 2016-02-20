module Utils.Helpers where

indexed :: [a] -> [(Int, a)]
indexed xs = zip [0..length xs] xs

pairsToStrings :: (Show a, Show b) => [(a, b)] -> [String]
pairsToStrings = map (\(x, y) -> show x ++ show y)
