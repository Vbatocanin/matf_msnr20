{-# LANGUAGE TemplateHaskell, ViewPatterns, PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
import Debug

debug [d|
    countGreater1st :: (Ord a) => [a] -> Int
    countGreater1st [] = 0
    countGreater1st [x] = 0
    countGreater1st (x:y:xs) 
            | y > x = 1 + countGreater1st (x:xs)
            | otherwise = 0 + countGreater1st (x:xs)
    
    countGreater1st' :: (Ord a) => [a] -> Int
    countGreater1st' [] = 0
    countGreater1st' [x] = 0
    countGreater1st' (x:y:xs) 
            | y > x = 1 + countGreater1st (y:xs)
                                        -- ^
                                        -- bag, treba x, a ne y
            | otherwise = 0 + countGreater1st (x:xs)

    countGreater1st'' :: (Ord a) => [a] -> Int
    countGreater1st'' [] = 0
    countGreater1st'' [x] = 0
    countGreater1st'' (x:y:xs) 
            | y >= x = 1 + countGreater1st (y:xs)
            -- ^^							^ 
            --  bag, treba >, a ne >=		bag, treba x, a ne y
            | otherwise = 0 + countGreater1st (x:xs)
    |]
