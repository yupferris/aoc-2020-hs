-- cat input | runhaskell part-1.hs

main = interact solve

solve :: String -> String
solve input = show answer
  where
    answer = traverseMap 0 0 3 1 0 $ lines input

traverseMap :: Int -> Int -> Int -> Int -> Int -> [String] -> Int
traverseMap x y slopeX slopeY trees map
  | y >= mapHeight = trees
  | otherwise = traverseMap x' y' slopeX slopeY trees' map
  where
    mapHeight = length map
    mapWidth = length $ head map
    mapX = x `mod` mapWidth
    foundTree = map !! y !! mapX == '#'
    trees' = if foundTree then trees + 1 else trees
    x' = x + slopeX
    y' = y + slopeY
