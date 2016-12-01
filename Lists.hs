module NinetyNineProblems.Lists
where

myLast :: [a] -> a
myLast = foldl1 (\x y -> y)

myButLast :: [a] -> a
myButLast [x,y] = x
myButLast (h:t) = myButLast t

elementAt :: [a] -> Int -> a
elementAt xs index = head (drop (index -1) xs)