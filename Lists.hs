module NinetyNineProblems.Lists
where

myLast :: [a] -> a
myLast [x] = x
myLast (h:t) = myLast t