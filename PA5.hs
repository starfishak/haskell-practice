module PA5 where

count5 :: [Int] -> Int
count5 [] = 0
count5 (x:xs)
     | x == 5    = 1 + count5 xs
     | otherwise = count5 xs

sumAll :: [Int] -> Int
sumAll [] = 0
sumAll (x:xs) = x + sum xs

binaryToDecimal :: Int -> Int
binaryToDecimal 0 = 0
binaryToDecimal x = 2 * binaryToDecimal (x `div` 10) + (x `mod` 10)

addBinary :: [Int] -> Int
addBinary [] = 0
addBinary (x:xs) = binaryToDecimal x + addBinary xs

last' :: [a] -> a
last' [] = error "please do not pass in an empty list"
last' (x:[])  = x
last' (x:xs) = last' xs

init' :: [a] -> [a]
init' [] = error "please do not pass in an empty list"
init' [x] = []
init' (x:xs) = x : init' xs

palindrome :: [Char] -> Bool
palindrome [] = True
palindrome x = reverse x == x

filterOut :: (a -> Bool) -> [a] -> [a]
filterOut func [] = []
filterOut func (x:xs)
          | func x = filterOut func xs
          | otherwise = x : filterOut func xs

nTimes :: Int -> (Int -> Int) -> [Int]
nTimes n func = [func(n) | n <- [0..n-1]] 

count :: (a -> Bool) -> [a] -> Int
count func [] = 0
count func (x:xs)
          | func x = 1 + count func xs
          | otherwise = 0 + count func xs

insert :: (Ord a) =>  a-> [a] -> [a]
insert val [] = [val]
insert val (x:xs)
          | val <= x = val:(x:xs)
          | otherwise = x : insert val xs

insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)

combine :: (a -> a -> a) -> [a] -> [a] -> [a]
combine func [][] = []
combine func [] _ = []
combine func _ [] = []
combine func (x:xs)(y:ys) = func x y : combine func xs ys

minimum' :: (Ord a) => [a] -> a
minimum' [] = error "no elements in array"
minimum' [x] = x
minimum' (x:xs) = isMin x (minimum' xs)
     where isMin x z
               | x < z = x
               | z < x = z
               | x == z = x

allTrue :: [Bool] -> Bool
allTrue [] = True
allTrue (x:xs)
     | x == False = False 
     | x == True = allTrue xs

anyTrue :: [Bool] -> Bool
anyTrue [] = False
anyTrue (x:xs)
     | x == True = True 
     | x == False = anyTrue xs

replicate' :: Int -> a -> [a]
replicate' 0 v = []
replicate' n v = [ v | _v <- [0..n-1]] 

lengths :: [[Char]] -> [Int]
lengths = map length

divisors :: [Int] -> [[Int]]
divisors [] = []
divisors (x:xs) = div' x : divisors xs
     where div' x = [d | d <- [1..(x-1)], x `mod` d == 0]

prime :: [Int] -> [Bool]
prime lst = [ check x | x <- lst]
          where check val 
                    | length (divisors [val]!!0) == 1 = True
                    | otherwise = False