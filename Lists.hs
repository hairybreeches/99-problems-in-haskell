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
