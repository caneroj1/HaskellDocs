module Utils.Helpers where

import qualified Data.Text      as Text
import           Data.Text.Read

indexes :: [a] -> [(Int, a)]
indexes xs = zip [0..length xs] xs

pairsToStrings :: (Show a, Show b) => [(a, b)] -> [String]
pairsToStrings = map (\(x, y) -> removeQuotes $ show x ++ show y)

removeQuotes :: String -> String
removeQuotes = filter (/= '"')

textToInteger :: Text.Text -> Integer
textToInteger text = go $ decimal text
  where go (Left  _)        = 0
        go (Right (num, _)) = num
