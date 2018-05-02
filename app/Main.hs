module Main where

import Lib
import Parser

main :: IO ()
main = do
  str <- getContents
  case runHaskell str of
    Left err  -> putStrLn err
    Right exp -> do print exp
                    print $ runInfer exp
