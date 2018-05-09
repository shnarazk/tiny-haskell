module Main where
import Lib

main :: IO ()
main = mapM_ (putStrLn . typing) . lines =<< getContents
