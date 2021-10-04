-- input | runhaskell part-2.hs

import Data.Char (ord)

data Entry = Entry
  { lowest :: Int
  , highest :: Int
  , c :: Char
  , password :: String
  }
  deriving (Show)

main = interact solve

solve :: String -> String
solve input = show answer
  where
    entries = parseEntry <$> lines input
    answer = length $ filter checkEntry entries

checkEntry :: Entry -> Bool
checkEntry entry =
  firstMatch `xor` secondMatch
  where
    password_ = password entry
    firstMatch = password_ !! (lowest entry - 1) == c entry
    secondMatch = password_ !! (highest entry - 1) == c entry

xor :: Bool -> Bool -> Bool
xor False False = False
xor True False = True
xor False True = True
xor True True = False

parseEntry :: String -> Entry
parseEntry s =
  Entry
    { lowest = lowest_
    , highest = highest_
    , c = c_
    , password = password_
    }
  where
    (lowest_, rest) = parseInt s
    (highest_, rest') = parseInt $ tail rest
    c_ = head $ tail rest'
    rest'' = tail $ tail rest'
    password_ = tail $ tail rest''

parseInt :: String -> (Int, String)
parseInt = go 0
  where
    go value s = case parseDigit s of
      Just (digit, rest) -> go (value * 10 + digit) rest
      Nothing -> (value, s)

parseDigit :: String -> Maybe (Int, String)
parseDigit (x:xs)
  | x `elem` ['0'..'9'] = Just (ord x - ord '0', xs)
  | otherwise = Nothing
