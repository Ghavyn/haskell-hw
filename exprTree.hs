import Data.Maybe
data Expr = Val Double | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr

evalExpr :: Expr -> Double
evalExpr (Val x) = x
evalExpr (Add e1 e2) = evalExpr e1 + evalExpr e2
evalExpr (Sub e1 e2) = evalExpr e1 - evalExpr e2
evalExpr (Mul e1 e2) = evalExpr e1 * evalExpr e2
evalExpr (Div e1 e2) = evalExpr e1 / evalExpr e2

evalAndPrint :: Expr -> IO ()
evalAndPrint expr =
  case evalExprW expr of
    Left err -> putStrLn err
    Right result -> putStrLn $ "Result: " ++ show result

evalExprW:: Expr -> Either String Double
evalExprW expr
  | isJust maybe_value = Right $ fromJust maybe_value
  | otherwise = Left "Error: Divide by Zero"
  where maybe_value = evalExpr' expr

evalExpr' :: Expr -> Maybe Double
evalExpr' (Val v) = Just v
evalExpr' (Add e1 e2) = (+) <$> evalExpr' e1 <*> evalExpr' e2
evalExpr' (Sub e1 e2) = (-) <$> evalExpr' e1 <*> evalExpr' e2
evalExpr' (Mul e1 e2) = (*) <$> evalExpr' e1 <*> evalExpr' e2
evalExpr' (Div e1 e2) = do
  x <- evalExpr' e1
  y <- evalExpr' e2
  if y == 0 then Nothing else Just (x / y)