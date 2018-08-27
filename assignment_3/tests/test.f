-- fun main x y z= let
--     fun f y = x + g y
--     fun g z = let
--         fun f x = x * z
--         in f x
--     in g z + f x

fun main x = let fun addToX v = x + v
             in addToX 5
