-- flp-22-fun (Solve.hs)
-- VUT FIT 2023
-- author: Daniel Pátek (xpatek08)

module Solve where

import Data.List (subsequences, elemIndex)
import Types (Knapsack (..), Item (..))

-- knapsack brute-force method
-- maximum compares each subset with each other one by one in order to find the biggest cost
-- subsets are obtained via "subsequences" method and filtered to only use ones below the maxWight treshold and ks_minCost treshhold
-- returns empty list if no solution found
knapsackBruteForce :: Knapsack -> [Int]
knapsackBruteForce (Knapsack ks_maxWeight ks_minCost ks_items)
    | validSubsets == [] = [] -- do not continue if not any possible subset was found
    | otherwise = map (\i -> if i `elem` validSubsets !! maxIndex then 1 else 0) indices
    where
        -- generate indices (kinda id for each Item) for preserve order EG. [0, 1, 2, 3..]
        indices = [0..(length ks_items)-1]
        -- filter all subsets to respect constraints (maxWeight and minConst) EG. [1, 2], [0, 2, 3]
        validSubsets = filter (\subset -> totalWeight ks_items subset <= ks_maxWeight && totalCost ks_items subset >= ks_minCost) $ subsequences indices
        subsetCosts = map (totalCost ks_items) validSubsets
        -- find out index of maximum cost subset in the filtered subsets  
        maxIndex = case elemIndex (maximum subsetCosts) subsetCosts of
            Just i -> i
            Nothing -> error "Cannot require maximum from subsets." -- this should never happen since i'm checking empty list higher

-- calculate total weight of knapsack items
totalWeight :: [Item] -> [Int] -> Int
totalWeight ks_items subset = sum $ map (\i -> weight (ks_items !! i)) subset

-- calculate total cost of knapsack itms
totalCost :: [Item] -> [Int] -> Int
totalCost ks_items subset = sum $ map (\i -> cost (ks_items !! i)) subset
