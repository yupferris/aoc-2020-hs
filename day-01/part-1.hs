-- cat input | runhaskell part-1.hs

main = interact solve

solve :: String -> String
solve input = show output
  where
    entries = map read $ lines input
    output =
      head
        [ x * y
        | x <- entries
        , y <- entries
        , x + y == 2020
        ]
