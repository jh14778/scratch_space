module XUtils where

import Text.Parsec

tie :: a -> b -> (a, b)
tie a b = (a, b)

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = x : (filter ((/=) x) $ unique xs)

maybeParse :: Parsec String () a -> Parsec String () (Maybe a)
maybeParse p = option Nothing (p >>= return.Just)
