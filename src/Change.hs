module Change
    where

change :: Integer -> [Integer] -> Integer
change n _ | n < 0 = 0
change 0 _ = 1
change _ [] = 0
change n (c0:cs) = change (n-c0) (c0:cs) + change n cs
