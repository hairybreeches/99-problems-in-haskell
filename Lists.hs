module NinetyNineProblems.Lists
where

myLast :: [a] -> a
myLast = foldl1 (\x y -> y)