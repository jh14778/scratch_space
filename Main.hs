module Main where

import XExpression
import XStatement
import XType
import XPrint
import Text.Parsec

test :: String -> IO ()
test s = print $ runParser externStmt () "" s >>= return.showStmt

forever :: IO () -> IO ()
forever a = a >> forever a

main :: IO ()
main = forever $ getLine >>= test
