module Main where

import Lib
import Chapter09.GoodKingMarkov

main :: IO ()
main = do 
--  putStrLn "42"
  let env = mkMarkovData
  let env' = travel env
  putStrLn $ show (generator env)
  putStrLn $ show (generator env')
  putStrLn $ show (chain env')
  putStrLn $ show (fromInteger . toInteger . fromEnum $ Seven)
