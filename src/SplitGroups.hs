{-# LANGUAGE ParallelListComp #-}

module SplitGroups where

spanGroups :: Int -> [a] -> [[a]]
spanGroups _ [] = []
spanGroups 1 xs = map (: []) xs
spanGroups n xs
    | length xs > n = take n xs : spanGroups n (tail xs)
    | otherwise     = [xs]


splitGroups :: Int -> [a] -> [[a]]
splitGroups n xs = splitGroupsOf (length xs `div` n) xs

splitGroupsOf :: Int -> [a] -> [[a]]
splitGroupsOf _ [] = []
splitGroupsOf 0 xs = map (: []) xs
splitGroupsOf 1 xs = map (: []) xs
splitGroupsOf n xs = take n xs : splitGroupsOf n (drop n xs)


splitGroups' :: Int -> [a] -> [[a]]
splitGroups' n xs = splitGroupsOf' (length xs `div` n) xs

splitGroupsOf' :: Int -> [a] -> [[a]]
splitGroupsOf' _ [] = []
splitGroupsOf' 0 xs = map (: []) xs
splitGroupsOf' 1 xs = map (: []) xs
splitGroupsOf' n xs = fmap (\k -> mkComb k n xs) [0..n-1]
    where mkComb :: Int -> Int -> [a] -> [a]
          mkComb off m ys = let zs = [(y, i) | y <- ys | i <- [0..]] in
                                [x | (x,y) <- zs, (y-off) `mod` m == 0]

