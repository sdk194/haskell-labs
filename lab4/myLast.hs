myLast :: [a] -> a

myLast [a] = a
myLast (h:t) = myLast t
