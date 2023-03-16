> import Data.Either ( isLeft, fromRight, isRight )

Binary Trees
    1. Define a Binary Tree data type (BinT t) with values stored at both leaves and internal nodes. Write the following map and foldr functions on your 
    binary tree data type. Be sure to include tests for your code in your handin.
        a.mapT - applies a function to every value node in the tree.
        b.foldrT - folds up all elements in a binary tree with an associative operator (like (+) or (*) but not (-) and not (/) ).
        
> data (BinT t) = Empty | Leaf t | Node (BinT t) t (BinT t)
>       deriving (Show)

> mapT :: (a -> b) -> BinT a -> BinT b
> mapT f Empty = Empty
> mapT f (Leaf a) = Leaf $ f a
> mapT f (Node (left) root (right)) = Node (mapT f left) (f root) (mapT f right)

> testMap:: IO ()
> testMap = print (mapT (*5) (Node Empty 7 (Node (Leaf 4) 2 (Leaf 5))))

> foldrT :: (t1 -> t2 -> t2) -> t2 -> BinT t1 -> t2
> foldrT f acc Empty = acc
> foldrT f acc (Leaf v) = f v acc
> foldrT f acc (Node l v r) = f v flr
>   where
>       fll = foldrT f acc l
>       flr = foldrT f fll r

> testFold :: IO ()
> testFold = print (foldrT (+) 0 (Node Empty 7 (Node (Leaf 4) 2 (Leaf 5))))

Expression Trees
    2. Define an Expression Tree algebraic data (Expr t) with constructors Add, Sub, Mul, Div that stand for the corresponding arithmetic operators.

> data Expr = Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr| Val Int
>       deriving (Show)

    3. Code a simple evaluator over Int (or Num t) expression trees that returns an Int (or more generally a Num t).
    Test your evaluator, including a test for divide-by-zero. You get to decide how you want to handle divide-by-zero for this first evaluator version.

> simpEval :: Expr -> Int
> simpEval (Add l r) = simpEval l + simpEval r 
> simpEval (Mul l r) = simpEval l * simpEval r 
> simpEval (Sub l r) = simpEval l - simpEval r
> simpEval (Div l r) = simpEval l `quot` simpEval r
> simpEval (Val v) = v

> testEval :: IO ()
> testEval = print (simpEval $ Add (Div (Val 6) (Val 5)) (Mul (Val 6) (Val 5)) )

> testEvalZero :: IO ()
> testEvalZero = print (simpEval $ Add (Div (Val 6) (Val 0)) (Mul (Val 6) (Val 5)) )

    4. Now convert the evaluator to a function that checks for divide by zero and returns an "Either a b" type. A divide by zero should return an error string
    in the "Left" constructor value and a correct calculation should return the result in the "Right" constructor value.

> simpEval' :: Expr -> Either String Int
> simpEval' (Div _ (Val 0)) = Left "Can't divide by zero"
> simpEval' (Div l r)
>   | isLeft el = el
>   | isLeft er = er 
>   | otherwise = Right (fromRight 0 el `quot` fromRight 0 er)
>       where
>           el = simpEval' l
>           er = simpEval' r
> simpEval' (Mul l r)
>   | isLeft el = el
>   | isLeft er = er 
>   | otherwise = Right (fromRight 0 el * fromRight 0 er)
>       where
>           el = simpEval' l
>           er = simpEval' r
> simpEval' (Add l r)
>   | isLeft el = el
>   | isLeft er = er 
>   | otherwise = Right (fromRight 0 el + fromRight 0 er)
>       where
>           el = simpEval' l
>           er = simpEval' r
> simpEval' (Sub l r)
>   | isLeft el = el
>   | isLeft er = er 
>   | otherwise = Right (fromRight 0 el - fromRight 0 er)
>       where
>           el = simpEval' l
>           er = simpEval' r
> simpEval' (Val v) = Right v

> testEval' :: IO ()
> testEval' = print (simpEval' $ Add (Div (Val 6) (Val 5)) (Mul (Val 6) (Val 5)) )

> testEvalZero' :: IO ()
> testEvalZero' = print (simpEval' $ Add (Div (Val 6) (Val 0)) (Mul (Val 6) (Val 5)) )

    5. Write a wrapper function around your evaluator that evaluates an arithmetic expression (coded using your Expression Tree data type) and returns a value or 
    prints the error message produced by the evaluator. Test your evaluator, including a test for divide-by-zero.

> simpEval'' :: Expr -> Int
> simpEval'' e
>   | isRight (simpEval' e) = fromRight 0 (simpEval' e)
>   | otherwise = error "Error: Divide by Zero"

> testEval'' :: IO ()
> testEval'' = print (simpEval'' $ Add (Div (Val 6) (Val 5)) (Mul (Val 6) (Val 5)) )

> testEvalZero'' :: IO ()
> testEvalZero'' = print (simpEval'' $ Add (Div (Val 6) (Val 0)) (Mul (Val 6) (Val 5)) )