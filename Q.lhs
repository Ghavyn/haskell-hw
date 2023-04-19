


> data Queue a = Q [a]
>   deriving (Show, Eq)

behaviors:

mkEmpty :: Queue a
isEmpty :: Queue a -> Bool
enqueue :: Queue a -> a -> Queue a     -- reverse the args if you want
dequeue :: Queue a -> Queue a
top     :: Queue a -> a

> mkEmpty = Q []

> isEmpty (Q []) = True
> isEmpty _      = False

> enqueue (Q xs) a = (Q (xs ++ [a]))

> dequeue (Q (x:xs)) = (Q xs)

> top (Q (x:_)) = x


key/value pairs
[(key, value)]
insert key value
find key
update key value

> data Map a  v = M [(a,v)]
>   deriving (Show, Eq)

assume the (k,v) not in the map

the following is just on lists -- convert to Maps

> insert k v (M m) = M ((k,v):m)

> find k (M []) = error "key not found"
> find k (M ((x,y):xs))
>   | x == k    = y
>   | otherwise = find k (M xs)

> update k v (M []) = error "key not found"
> update k v (M xs) = (M (update' k v xs))
>   where
>     update' k v ((x,y):xs) 
>       | x == k    = ((x,v):xs)
>       | otherwise = (x,y) : (update' k v xs)

ghci> insert 'a' 1 (M [])
M [('a',1)]
ghci> insert 'b' 2 $ insert 'a' 1 (M [])
M [('b',2),('a',1)]
ghci> find 'a' $ insert 'b' 2 $ insert 'a' 1 (M [])
1
ghci> find 'b' $ insert 'b' 2 $ insert 'a' 1 (M [])
2
ghci> 
ghci> update 'a' 6 $ insert 'b' 2 $ insert 'a' 1 (M [])
M [('b',2),('a',6)]
ghci> 


adjacency list??

V, (M (a,[a]))

{1,2,3} , [(1, [2,3]), (2, [3,4])]


