fun main x y = let
    fun add p = add_to_x p
    fun add_to_x q = add_to_y q + x
    fun add_to_y q = q + y
    in add y + x
