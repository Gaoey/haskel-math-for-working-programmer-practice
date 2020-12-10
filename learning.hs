import Data.List

manutd_legend :: Int -> String
manutd_legend 7 = "xxxx"
manutd_legend _ = "Noo"

factorial :: Int -> Int
factorial 0 = 1
factorial numb = numb * factorial (numb - 1)

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci numb = fibonacci (numb - 2) + fibonacci (numb - 1)

countElem :: [a] -> String
countElem [] = "empty"
countElem (_:[]) = "one"
countElem (_:_:[]) = "two"
countElem _ = "many"

grade :: Int -> Char
grade score
  | score > 90 = 'A'
  | score < 90 = 'F'

initials :: (String, String) -> String
initials name = [first] ++ "." ++ [last]
  where first = head (fst name)
        last = head (snd name)

sum' :: [Int] -> Int
sum' [] = 0
sum' numb = head numb + sum'(tail numb)
-- sum' (x:xs) = x + sum'(xs)

-- easy exercises
length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length'(xs)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

-- a little bit harder
prime_to :: Int -> [Int]
prime_to n = sieve [2..n]
  where sieve [] = []
        sieve (x:xs) = x : sieve [y | y <- xs, (y `rem` x) /= 0]

permute :: (Eq a) => [a] -> [[a]]
permute [] = [[]]
permute xs = [x:ys | x <- xs, ys <- permute (Data.List.delete x xs)]

-- permute [a,b,c]
-- a | b c ==> a : permute (b, c)
-- a | c b ==^
-- =====
-- b | a c ==> b : permute (a, c)
-- b | c a ==^
-- =====
-- c | a b ==> c : permute (a, b)
-- c | b a ==^

-- hanoi :: Int -> [(Int,Int)]
-- hanoi n = hanoi' n 1 2 3
-- hanoi' 0 _ _ _ = []
-- hanoi' n beg tmp dest = tops_to_tmp ++ bottom_to_dest ++ tops_to_dst
--   where tops_to_tmp = hanoi' (n-1) beg dest tmp
--         bottom_to_dest = (beg dest)
--         tops_to_dst =  hanoi' (n-1) beg dest tmp

--    |      |     |
--    =      |     |
--   ===     |     |
--  =====    |     |
--  start   tmp   dst

-- HOF 

twice :: (a -> a) -> a -> a 
twice f n = f (f n)

-- twice (* 2) 3

thrice :: ( a->a ) -> a -> a
thrice f n  = (f . f . f) n

-- thrice (* 2) 3

map' :: (a->b)->[a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) = if (f x) then [x] else [] ++ filter' f xs

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 _ [] = []
filter2 f (x:xs)
  | f x = x : filter2 f xs
  | otherwise = filter2 f xs

-- Custom Type 
data Bool' = True | False
data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun -- enum type
    deriving (Show)

-- type classes // Num, Eq .. etc

-- Either, | is union type
data Shape = Circle Float | Square Float -- Circle :: Float -> Shape -> we called this constructor
    deriving (Show)

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Square s) = s * s

-- this isn't really good - why? => bad documentation - Float คือ ไรวะ เส้นทะแยงมุม ความสูง มันไม่สื่อ
-- data Shape2 = Circle2 Radius | Square2 Side 
--     deriving (Show)

-- type Radius = Float
-- type Side = Float

-- area2 :: Shape -> Area
-- area2 (Circle r) = pi * r * r

-- Polymorphic type
data Tree a = EmptyTree | Node a (Tree a ) (Tree a) 
    deriving (Show)