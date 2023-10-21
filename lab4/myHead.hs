myHead :: [a] -> a

myHead [] = error "must not be empty"
myHead (a:_) = a
