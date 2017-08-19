module XUtils where

import Text.Parsec

tie :: a -> b -> (a, b)
tie a b = (a, b)

maybeParse :: Parsec String () t -> Parsec String () (Maybe t)
maybeParse p = option Nothing (p >>= return.Just)
