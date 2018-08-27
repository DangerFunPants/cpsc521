fun main x1 x2 x3 = let
    fun f1 x4 = x1 + f2 x4
    fun f2 x5 = let
        fun f3 x6 = x6 * x5
        in f3 x1
    in f2 x3 + f1 x1
