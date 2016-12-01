module NinetyNineProblems.Lists
where

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
