myInit :: [a] -> [a]

myInit [] = error "must be non empty"
myInit [h] = []
myInit (h:t) = h:myInit t
