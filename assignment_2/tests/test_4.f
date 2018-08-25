fun main x y z n = 
    let 
        fun f1 v = x + f2 v
        fun f2 j =
            let
                fun g2 b = b + f3 j
            in g2 y + f3 x
        fun f3 k = 
            let
                fun g3 c = c * f1 k
            in g3 z
    in f1 n
