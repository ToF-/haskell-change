module Change
    where

change :: Integer -> [Integer] -> Integer
change n _ | n < 0 = 0
change 0 _ = 1
change n [c] = change (n-c) [c]
