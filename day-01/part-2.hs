-- input | runhaskell part-2.hs

main = interact solve

solve :: String -> String
solve input = show answer
  where
    entries = map read $ lines input
    answer =
      head
        [ x * y * z
        | x <- entries
        , y <- entries
        , z <- entries
        , x + y + z == 2020
        ]
