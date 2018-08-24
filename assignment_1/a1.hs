import Data.Monoid 
import Control.Monad (filterM)
import qualified Data.Set as S
import Data.List (groupBy, sortOn)

data List a
    = Cons a (List a)
    | Nil
    deriving (Show, Eq)

instance Monoid (List a) where
    mempty = Nil
    mappend = app

app :: List a -> List a -> List a
app (Cons a rest) l2 = Cons a (app rest l2)
app Nil l2 = l2

rev :: List a -> List a
rev l = rev' l Nil
    where
        rev' (Cons a rest) lr = rev' rest (Cons a lr)
        rev' Nil  lr = lr

flatten :: List (List a) -> List a
flatten (Cons a rest) = a <> (flatten rest)
flatten Nil = Nil

greaterInList :: Integer -> [Integer] -> [Integer]
greaterInList a (x:xs) = if x > a
                            then x:(greaterInList a xs)
                            else greaterInList a xs

lexInt :: [Int] -> [Int] -> Bool
lexInt l r = x > y
    where
        x = (concat . (fmap show)) l
        y = (concat . (fmap show)) r

msplit :: [a] -> ([a], [a])
msplit xs = (ls, rs)
    where
        indList = zip [1..] xs
        ls = fmap snd $ filter (odd . fst) indList
        rs = fmap snd $ filter (even . fst) indList

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [a] = [a]
mergeSort as = merge leftList rightList
    where
        (ll, rl) = splitAt ((length as) `div` 2) as
        leftList = mergeSort ll
        rightList = mergeSort rl

merge :: (Ord a) => [a] -> [a] -> [a]
merge (x:xs) (y:ys) =
    if x < y
        then (x:(merge xs (y:ys)))
        else (y:(merge (x:xs) ys))
merge [] ys = ys
merge xs [] = xs

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort [a] = [a]
quickSort as = left ++ [pivot] ++ right
    where
        (ll, lr) = splitAt ((length as) `div` 2) as
        pivot = (head lr)
        smaller = ll++(tail lr)
        left = quickSort [ x | x <- smaller, x < pivot ]
        right = quickSort [ x | x <- smaller,  x >= pivot ]

member :: (Eq a) => a -> [a] -> Bool
member a (x:xs) = (a == x) || (member a xs)
member _ [] = False

relGrp :: (a -> b -> Bool) -> [a] -> [b] -> [(a, [b])]
relGrp rel (a:as) bs = (a,(filter (rel a) bs)):(relGrp rel as bs)
relGrp _ [] _ = []

group :: (a -> a -> Bool) -> [a] -> [[a]]
group _ [] = []
group fn (a:as) = thisGroup:(group fn rest)
    where
        thisGroup = a:(findGroup a fn as)
        (_, rest) = splitAt ((pred . length) thisGroup) as

findGroup last fn [] = []
findGroup last fn (a:as) = 
    if fn last a
        then a:(findGroup a fn as)
        else []

subset :: [a] -> [[a]]
subset = filterM (\_ -> [True, False])

perm :: (Eq a) => [a] -> [[a]]
perm [] = [[]]
perm as = concat (fmap conFn as)
    where
        conFn e = fmap (\l -> e:l) recCall
            where
                recCall = perm (filter (/= e) as)

data ETree
    = Var String
    | Opn String [ETree]

uniqueList :: (Ord a) => [a] -> [a]
uniqueList = (S.toList . S.fromList)

varsOf' :: ETree -> [String]
varsOf' (Var s) = [s]
varsOf' (Opn opName args) = concat $ fmap varsOf' args
varsOf = (uniqueList . varsOf')

factorial 0 = 1
factorial n = foldl1 (*) [1..n]

addPoly :: [Float] -> [Float] -> [Float]
addPoly = zipWith (+)

mulPoly f1 f2 = (sumTerms . concat) comp
    where
        f1' = zip [0..] f1
        f2' = zip [0..] f2
        comp = fmap (\(d_i, t_i) -> fmap (\(d_j, t_j) -> ((d_i + d_j), (t_i * t_j))) f2') f1'
sumTerms ts = fmap sum gs'
    where
        ds = uniqueList (fmap fst ts)
        gs = sortOn (fst . head) (fmap (\i -> filter (\t -> (fst t) == i) ts) ds)
        gs' = (fmap . fmap) snd gs

data Rose a
    = RS a [Rose a]
    deriving (Show)

instance Foldable Rose where
    -- foldMap :: Monoid m => (a -> m) -> t a -> m
    foldMap f (RS a rest) = (f a) `mappend` cs
        where
            mpd = fmap (foldMap f) rest
            cs = mconcat mpd

width :: (Rose a) -> Int
width rt = length rt

main :: IO ()
main = do
    let ts = RS 1 [(RS 2 [RS 3 [], RS 4 []]), (RS 5 [RS 6 [], RS 7 []])]
    putStrLn $ show ts
    putStrLn $ show (width ts)



















