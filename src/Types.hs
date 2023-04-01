-- flp-22-fun (types.hs)
-- VUT FIT 2023
-- author: Daniel Pátek (xpatek08)

module Types where

data Item = Item { weight :: Int, cost :: Int } deriving (Eq)

data Knapsack = Knapsack { maxWeight :: Int, minCost :: Int, items :: [Item] } deriving (Eq)

indent :: String -> String
indent str = replicate 2 ' ' ++ str

instance Show Item where
    show (Item item_weight item_cost) = 
        "Item {\n" ++
        indent ("weight: " ++ show item_weight ++ "\n") ++
        indent ("cost: " ++ show item_cost ++ "\n") ++
        "}\n"

instance Show Knapsack where
    show (Knapsack ks_maxWeight ks_minCost ks_items) =
        "Knapsack {\n" ++
        indent ("maxWeight: " ++ show ks_maxWeight ++ "\n") ++
        indent ("minCost: " ++ show ks_minCost ++ "\n") ++
        indent "items: [\n" ++
        concatMap (\item -> unlines $ map (indent . indent) (lines $ show item)) ks_items ++
        indent "]\n" ++
        "}\n"

-- custom print solution helper function
printSolution :: [Int] -> String
printSolution xs
    | any (==1) xs = "Solution " ++ "[" ++ unwords (map show xs) ++ "]\n"
    | otherwise = "False\n"