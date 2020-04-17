countGreater1st' :: (Ord a) => [a] -> Int
countGreater1st' [] = 0
countGreater1st' [x] = 0
countGreater1st' (x:y:xs) 
        | y > x = 1 + countGreater1st' (y:xs)
        | otherwise = 0 + countGreater1st' (x:xs)

main :: IO()
main = print $ countGreater1st' [3,4,1,1,1]