
module SetUL (Set, null, member, empty, fromList, toList, insert, delete)
    where

import Prelude hiding (null)
import qualified Data.List as List (nub, sort, null)

newtype Set a = Set [a]

instance (Eq m) => Eq (Set m) where
    s1 == s2    = s1 `eq` s2

instance (Show m, Ord m) => Show (Set m) where
    show set = "fromList " ++ show sortedList
        where (Set sortedList) = sort set 

fromList :: (Eq a) => [a] -> Set a
fromList l = Set $ List.nub l

toList :: Set a -> [a]
toList (Set l) = l

delete :: (Eq a) => a -> Set a -> Set a
delete val (Set l) = Set $ filter (/= val) l

insert :: (Eq a) => a -> Set a -> Set a
insert val (Set l)
    | not $ val `elem` l = Set (val:l)
    | otherwise          = Set l

member :: (Eq a) => a -> Set a -> Bool
member val (Set l) = val `elem` l

sort :: (Ord a) => Set a -> Set a
sort (Set l) = Set ( List.sort l)

null :: Set a -> Bool
null (Set l) = List.null l

empty :: Set a
empty = Set []

eq :: (Eq a) => Set a -> Set a -> Bool
eq (Set l1) (Set l2) = foldr (\val acc -> acc && val `elem` l2) True l1

union :: (Eq a) => Set a -> Set a -> Set a
union (Set l1) (Set l2) = Set $ List.nub $ l1 ++ l2

intersection :: (Eq a) => Set a -> Set a -> Set a
intersection (Set l1) (Set l2) = Set $ filter (\x -> x `elem` l2) l1

crossproduct :: Set a -> Set b -> Set (a,b)
crossproduct (Set l1) (Set l2) = Set $ [ (x,y) | x <- l1, y <- l2 ]

difference :: (Eq a) => Set a -> Set a -> Set a
difference (Set l1) (Set l2) = Set $ filter (\x -> not $ x `elem` l2) l1
