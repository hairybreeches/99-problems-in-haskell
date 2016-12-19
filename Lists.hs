module NinetyNineProblems.Lists
where

import Data.List ((\\))
import GHC.Exts (sortWith)
import qualified Data.Map.Strict as Map

myLast :: [a] -> a
myLast = foldl1 (\x y -> y)

myButLast :: [a] -> a
myButLast [x,y] = x
myButLast (h:t) = myButLast t

elementAt :: [a] -> Int -> a
elementAt xs index = head (drop (index -1) xs)

myLength :: [a] -> Int
myLength [] = 0
myLength (h:t) = 1 + myLength t

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (h:t) = myReverse t ++ [h]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == (reverse x)

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (List xs) = concatMap flatten xs
flatten (Elem x) = [x]

prependUnique :: (Eq a) => [a] -> a -> [a]
prependUnique [] x = [x]
prependUnique xs y
    | (head xs) == y  = xs
    | otherwise = y:xs

compress :: (Eq a) => [a] -> [a]
compress = reverse . (foldl prependUnique [])

packElement :: (Eq a) => [[a]] -> a -> [[a]]
packElement [] x = [[x]]
packElement globalList@(currentList@(currentElement:_):globalTail) y
    | currentElement == y  = (y:currentList):globalTail
    | otherwise = [y] : globalList

pack  :: (Eq a) => [a] -> [[a]]
pack  = reverse . (foldl packElement [])

encodeElement :: (Eq a) => [(Int, a)] -> a -> [(Int, a)]
encodeElement [] x = [(1,x)]
encodeElement globalList@((currentCount, currentElement):globalTail) y
    | currentElement == y  = (currentCount + 1, currentElement):globalTail
    | otherwise = (1,y) : globalList

data EncodedRun a = Single a | Multiple Int a
    deriving Show

createEncodedRun :: (Int, a) -> EncodedRun a
createEncodedRun (1, a) = Single a
createEncodedRun (n, a) = Multiple n a

encode :: (Eq a) => [a] -> [EncodedRun a]
encode = (map createEncodedRun) . reverse . (foldl encodeElement [])

decodeRun :: EncodedRun a -> [a]
decodeRun (Single x) = [x]
decodeRun (Multiple n x) = replicate n x

decode :: [EncodedRun a] -> [a]
decode = concatMap decodeRun


repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

dupli :: [a] -> [a]
dupli = (flip repli) 2

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = (take (n - 1) xs) ++ (dropEvery (drop n xs) n)

split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)

slice :: [a] -> Int -> Int -> [a]
slice xs start finish = take (finish - start + 1) (drop (start - 1) xs)

rotate :: [a] -> Int -> [a]
rotate xs n
    | n < 0 = 
        let l = (length xs)
        in rotate xs (n `mod` l)
    | otherwise = 
        let (start, finish) = split xs n
        in finish ++ start

removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (element, before ++ after)
    where (before, (element:after)) = split xs (n - 1)
   
insertAt :: a -> [a] -> Int -> [a]
insertAt newValue xs index = before ++ [newValue] ++ after
    where (before, after) = split xs (index - 1)
    
range :: Int -> Int -> [Int]
range a b = [a .. b]

type Combination a = [a]
type Partition a = [[a]]

combinations :: Int -> [a] -> [Combination a]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (h:t) = map (h:) (combinations (n - 1) t) ++ combinations n t

group :: (Eq a) => [Int] -> [a] -> [Partition a]
group [] [] = [[]]
group (n:ns) xs = concat [[ c:subPartition |subPartition <- group ns (xs \\ c)]| c <- combinations n xs]

lsort :: [[a]] -> [[a]]
lsort = sortWith length

type FrequencyLookup a = Map.Map a Int

addFrequency :: (Ord a) => a -> FrequencyLookup a -> FrequencyLookup a
addFrequency k = Map.insertWith (+) k 1

frequencies :: (Ord a) => [a] -> FrequencyLookup a
frequencies = foldr
    addFrequency
    Map.empty

lookupLength :: FrequencyLookup Int -> [a] -> Int
lookupLength freqs x = Map.findWithDefault 0 (length x) freqs

lfsort :: [[a]] -> [[a]]
lfsort xs =  
    let freqs = frequencies (map length xs)
    in sortWith (lookupLength freqs) xs
