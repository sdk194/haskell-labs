myAppend :: [a] -> [a] -> [a]

myAppend [] y = y
myAppend (h:t) y = h:myAppend t y
