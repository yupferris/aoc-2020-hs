-- cat input | runhaskell part-1.hs

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
  charCount >= lowest entry && charCount <= highest entry
  where
    charCount = length $ filter (== c entry) $ password entry

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
