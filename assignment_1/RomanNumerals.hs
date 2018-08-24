data RomanNumerals
    = I
    | V
    | X
    | L
    | C
    | D
    | M
    deriving (Show, Eq, Enum)

toDec r = 
    case r of
        I -> 1
        V -> 5
        X -> 10
        L -> 50
        C -> 100
        D -> 500
        M -> 1000

toRoman 0 _ = []
toRoman n I = replicate n I
toRoman n r = thisLevel
    where 
        (rep, nextN) = n `divMod` (toDec r)
        nextR = pred r
        check = ((toDec r) - n) `mod` (toDec (pred r))
        thisLevel = 
            case check of 
                0 -> [ (pred r), r ]++(toRoman interN nextR)
                otherwise -> (replicate rep r)++(toRoman nextN nextR) 
        interN = n - ((toDec r) - (toDec (pred r)))
