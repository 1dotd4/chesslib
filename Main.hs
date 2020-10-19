module Main where

import ModularArithmetic

main :: IO ()
main = putStrLn . show $ modPow 3 5 7
