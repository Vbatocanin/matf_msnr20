-- bez greske
countGreater1st :: (Ord a) => [a] -> Int
countGreater1st [] = 0
countGreater1st [x] = 0
countGreater1st (x:y:xs) 
		| y > x = 1 + countGreater1st (x:xs)
		| otherwise = 0 + countGreater1st (x:xs)

-- sa jednom greskom
countGreater1st' :: (Ord a) => [a] -> Int
countGreater1st' [] = 0
countGreater1st' [x] = 0
countGreater1st' (x:y:xs) 
		| y > x = 1 + countGreater1st (y:xs)
									-- ^
									-- bag, treba x, a ne y
		| otherwise = 0 + countGreater1st (x:xs)


-- sa dve greske
countGreater1st'' :: (Ord a) => [a] -> Int
countGreater1st'' [] = 0
countGreater1st'' [x] = 0
countGreater1st'' (x:y:xs) 
		| y >= x = 1 + countGreater1st (y:xs)
		--  ^^							^ 
		--  bag, treba >, a ne >=		bag, treba x, a ne y
		| otherwise = 0 + countGreater1st (x:xs)

