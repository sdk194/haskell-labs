myTail :: [a] -> [a]

myTail [] = error "must not be empty"
myTail (_:t) = t
