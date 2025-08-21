-- 00: Simple Naive Fibbonacci in Haskell
fib :: Int -> Int
fib 0 = 1
fib 1 = 1 
fib n = fib (n-1) + fib (n-2)

-- 01: Find the last element of a list!
myLast :: [a] -> a
myLast [] = error "There's no last element in an empty list!"
myLast [x] = x
myLast (_:x) = myLast x

-- 02: Find the second last element of a list!
mySecondLast :: [a] -> a
mySecondLast [] = error "There's no second last element in an empty list!"
mySecondLast [b] = error "There's no second last element in a one-element list"
mySecondLast [x, y] = x
mySecondLast (_:xs) = mySecondLast xs

-- 03: Find the kth-1 element of a list!
elementAt :: [a] -> Int -> a
elementAt [] n = error "There's no elements in an empty list"
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs (n-1)

-- 04: Find the length of a list
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs 

-- 05: Reverse a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (h:xs) = (myReverse xs) ++ [h]

-- 06: Is Palindrome?
isPalindrome :: Eq a => [a] -> Bool
isPalindrome lst = (reverse lst) == lst

-- 07: Flatten a List.
data NestedList a = Elem a | List [NestedList a]
myFlatten :: NestedList a -> [a]
myFlatten (Elem x) = [x]
myFlatten (List (x:xs)) = myFlatten x ++ (myFlatten (List xs))

-- 08: Compress a List.
myCompress :: Eq a => [a] -> [a]
myCompress [] = []
myCompress [a] = [a]
myCompress (a:b:xs) = if (a == b) then myCompress(b : xs) else a : myCompress(b : xs)

-- 09: Pack a List.


