-- cat input | runhaskell part-2.hs

main = interact solve

solve :: String -> String
solve input = show answer
  where
    answer = product $ traverseMap 0 0 0 (lines input) <$> [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

traverseMap :: Int -> Int -> Int -> [String] -> (Int, Int) -> Int
traverseMap x y trees map slope@(slopeX, slopeY)
  | y >= mapHeight = trees
  | otherwise = traverseMap x' y' trees' map slope
  where
    mapHeight = length map
    mapWidth = length $ head map
    mapX = x `mod` mapWidth
    foundTree = map !! y !! mapX == '#'
    trees' = if foundTree then trees + 1 else trees
    x' = x + slopeX
    y' = y + slopeY
