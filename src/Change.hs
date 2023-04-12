module Change
    where

change :: Integer -> [Integer] -> Integer
change n _ | n < 0 = 0
change 0 _ = 1
change _ [] = 0
change n [c] = change (n-c) [c]
change n [c0,c1] = change (n-c0) [c0,c1] + change (n-c1) [c1]
change _ _ = 10
