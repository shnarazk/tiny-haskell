module Main where
import Lib

main :: IO ()
main = putStrLn . typing . init =<< getContents
