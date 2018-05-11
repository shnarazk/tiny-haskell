module Main where
import Lib
import Control.Monad
import System.IO
import System.Exit

main :: IO ()
main = do
  let prompt = putStr "TinyHaskell> " >> hFlush stdout
      eval k
        | k == ":q" = die "Bye."
        | otherwise = putStrLn $ typing k
  prompt
  mapM_ (const prompt <=< eval) . lines =<< getContents

