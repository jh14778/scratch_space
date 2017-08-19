module Main where

import XExpression
import XStatement
import Text.Parsec

test :: String -> IO ()
test = parseTest (ifElseStmt <* eof)

forever :: IO () -> IO ()
forever a = a >> forever a

main :: IO ()
main = forever $ getLine >>= test
