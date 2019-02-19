module RecPatterns where

data IntList = Empty | Cons Int IntList
  deriving Show

testList :: IntList
testList = Cons 1 (Cons 2 (Cons 3 Empty))

mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList f l = case l of
  Empty -> Empty
  (Cons head tail) -> Cons (f head) (mapIntList f tail)

filterIntList :: (Int -> Bool) -> IntList -> IntList
filterIntList p l =
  case l of
    Empty -> Empty
    (Cons head tail) ->
      if p head
        then Cons head (filterIntList p tail)
        else filterIntList p tail

foldIntListRight :: Int -> (Int -> Int -> Int) -> IntList -> Int
foldIntListRight id f l =
  case l of
    Empty -> id
    (Cons head tail) -> f head (foldIntListRight id f tail)

foldIntListLeft :: Int -> (Int -> Int -> Int) -> IntList -> Int
foldIntListLeft curVal f l =
  case l of
    Empty -> curVal
    (Cons first (Cons second tail)) -> foldIntListLeft curVal f (Cons (f first second) tail)

