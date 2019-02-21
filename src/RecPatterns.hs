module RecPatterns where

data IntList = Empty | Cons Int IntList
  deriving Show

data MyList t = E | C t (MyList t)
  deriving Show

testList :: IntList
testList = Cons 1 (Cons 2 (Cons 3 Empty))

testList2 :: MyList Int
testList2 = C 1 (C 2 (C 3 E))

mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList f l = case l of
  Empty -> Empty
  (Cons head tail) -> Cons (f head) (mapIntList f tail)

mapList :: (t -> u) -> MyList t -> MyList u
mapList f E = E
mapList f (C head tail) = C (f head) (mapList f tail)

filterIntList :: (Int -> Bool) -> IntList -> IntList
filterIntList p l =
  case l of
    Empty -> Empty
    (Cons head tail) ->
      if p head
        then Cons head (filterIntList p tail)
        else filterIntList p tail

filterList :: (t -> Bool) -> MyList t -> MyList t
filterList p E = E
filterList p (C head tail)
  | p head = C head (filterList p tail)
  | otherwise = filterList p tail

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

