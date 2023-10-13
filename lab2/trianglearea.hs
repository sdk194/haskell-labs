s :: Float -> Float -> Float -> Float
s x y z = (x + y + z) / 2

triangleArea :: Float -> Float -> Float -> Float
triangleArea x y z = if (s x y z) * (s x y z - x) * (s x y z - y) * (s x y z - z) < 0
    then error "Not a triangle"
    else sqrt((s x y z) * (s x y z - x) * (s x y z - y) * (s x y z - z))