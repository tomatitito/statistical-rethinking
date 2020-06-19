module Main where

import Lib
import Chapter09.GoodKingMarkov
import RIO (liftIO, runReaderT, ReaderT)

main :: IO ()
--main = do
----  putStrLn "42"
----  let env = mkEnv
----  let env' = travel' env
--  putStrLn $ show "Islands here..."
--main = do
--  let env = mkEnv
--      unpacked = runReaderT travel'
--  islands <- unpacked env
--  putStrLn $ show islands
main = do
  chain <- travel
  print chain
