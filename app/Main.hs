module Main where

import Lib
import Chapter09.GoodKingMarkov

x1 = rollDie

main :: IO ()
main = do 
  putStrLn "42"
  putStrLn $ show x1
