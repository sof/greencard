--
-- A Main module for simple GreenCard test.
--
-- Defines a main IO action that invokes the IO action
-- foo defined in World.gc and implemented in C.

module Main(main) where

import World

main :: IO ()
main = do
  x <- foo "A string from Haskell" 2.5
  putStrLn ("foo returned the value " ++ (show x))
