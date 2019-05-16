-- intersection :: Eq a => [a] -> [a] -> [a]
-- intersection [] x = []
-- intersection (x:xs) y
--     | elem' x y = x : intersection xs y
--     | otherwise = intersectionxs y

allStar :: [Char] -> [Char]
allStar [] = ""
allStar (x:[]) = [x]
allStar (x:xs) = x : '*' : allStar xs

stringClean :: [Char] -> [Char]
stringClean [] = []
stringClean (x:[]) = [x]
stringClean (x:y:ys)
        | x == y    = stringClean(y:ys)
        | otherwise = x : stringClean (y:ys)

-- where & let
    -- where = local names that are visible only to the function
    -- Example:
-- quadratic :: Double -> Double -> Double -> (Double, Double)
-- quadratic a b c
--     | denom == 0 = error "No solution"
--     | disc < 0 = error "Complex result"
--     | otherwise = ( ( (-b) + sqrt disc ) / denom ,
--             ( (-b) - sqrt disc ) / denom )
--     where denom = 2 * a
--             disc  =b * b - 4.0 * a * c

-- return number of "c" chars in a list of strings
countCAll :: [[Char]] -> Int       
countCAll [] = 0
countCAll (x:xs) = countCOne x + countCAll xs
    where countCOne [] = 0
          countCOne (x:xs)
                | x =='c' = 1 + countCOne xs
                | otherwise = countCOne xs


    -- let
        -- similar to where, but more local (ie cannot span guards), and can be used as an experssion.
circleArea :: Double -> Double
circleArea r
    | r < 0 = error "Invalid radius"
    | otherwise = let pi = 3.1415
                  in pi * r ^ 2

-- Lambda functions
        -- came out of functional languages
        -- "\" denotes lambda function
        -- Examples: 
-- (\x -> x * 2) 8 -- returns 16
-- (\x -> x ^ 2) 8 -- returns 64
-- (\x y -> x + y) 3 7 -- returns 10


-- Maps
        -- Apply to all functionality
        -- first parameter: a function (usually of one parameter, but not necessarily)
        -- second parameter: a list
        -- syndax: map lambda function list
        -- Examples:
-- map succ [1,2,3,4]
-- map (\x -> x * 2) [1,2,3]    -- returns [2,4,6]


-- Functions as Parameters
        -- Defining functions that can take functions as parameters
starSome :: (Char -> Bool) -> [Char] -> [Char]
starSome func [] = []
starSome func (x:xs)
    | func x = '*' : starSome func xs
    | otherwise = x : starSome func xs

-- Same thing in a map and lamdba 
-- map (\x -> if  x == 'c' then '*' else x) "aabbccce"

-- Same thing, easier to write
starSome :: (Char -> Bool) -> [Char] -> [Char]
starSome func x = map (\y -> if f y then '*' else y) x

-- Currying
