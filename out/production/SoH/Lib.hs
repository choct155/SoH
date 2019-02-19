module Lib where

data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
  deriving Show

someFunc :: IO ()
someFunc = putStrLn "someFunc"

printSomeThing :: IO ()
printSomeThing = print Shoe

greaterThan100 :: [Int] -> [Int]
greaterThan100 [] = []
greaterThan100 inputList = filter (\x -> x > 100) inputList

timesTwo :: Int -> Int
timesTwo x = x * 2

prodTwo :: Int -> Int -> Int
prodTwo x y =  x * y
