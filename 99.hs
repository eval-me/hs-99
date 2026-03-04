-- 00: Simple Naive Fibbonacci in Haskell
fib :: Int -> Int
fib 0 = 1
fib 1 = 1 
fib n = fib (n-1) + fib (n-2)

-- 01: Find the last element of a list!
myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast [x] = Just x
myLast (_:x) = myLast x

-- 02: Find the second last element of a list!
mySecondLast :: [a] -> Maybe a
mySecondLast [] = Nothing
mySecondLast [b] = Nothing
mySecondLast [x, y] = Just x
mySecondLast (_:xs) = mySecondLast xs

-- 03: Find the kth-1 element of a list!
elementAt :: [a] -> Int -> Maybe a
elementAt [] n = Nothing
elementAt (x:_) 1 = Just x
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

-- 09: Pack duplicates in a list.
myPack :: Eq a => [a] -> [[a]]
myPack [] = []
myPack (x:xs) = (takeWhile (==x) (x:xs)) : myPack (dropWhile (==x) (x:xs))

-- 10: Run-length encoding of a list.
myEncode :: Eq a => [a] -> [(Int, a)]
myEncode [] = []
myEncode lst = map (\pack -> (length pack, pack !! 0)) (myPack lst)

-- 11: Modified run-length encoding.
data L11 a = Single a | Multiple Int a
    deriving (Show)

myEncodeModified :: Eq a => [a] -> [L11 a]
myEncodeModified lst = map translate (myEncode lst)
  where translate = (\(len, elm) ->
                       if len == 1 then Single elm
                       else Multiple len elm)

-- 12: Decode modified run-length encoding.
myDecodeModified :: Eq a => [L11 a] -> [a]
myDecodeModified lst = concat $ map decode lst
  where decode = (\l ->
                    case l of
                      Single elm     -> [elm]
                      Multiple n elm -> replicate n elm)

-- 13: Direct run-length encoding.
myDirectEncoding :: Eq a => [a] -> [(Int, a)]
myDirectEncoding [] = []
myDirectEncoding (x:xs) = myHelper xs (1, x)
  where
    myHelper [] acc = [acc]
    myHelper (x:xs) (acc, elm) = if x == elm then myHelper xs (acc+1, elm)
                                 else (acc, elm) : myHelper xs (1, x)

-- 14: Duplicate elements
myDupli :: [a] -> [a]
myDupli [] = []
myDupli (x : xs) = x : x : myDupli xs 

-- 15: Replicate elements given a number
myRepli :: [a] -> Int -> [a]
myRepli [] _ = []
myRepli lst 0 = lst
myRepli lst 1 = lst
myRepli [a] n = a : (myRepli [a] (n-1))
myRepli (x : xs) n = (myRepli (x : []) n) ++ (myRepli xs n)
