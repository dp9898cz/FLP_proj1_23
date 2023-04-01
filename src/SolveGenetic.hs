-- flp-22-fun (SolveGenetic.hs)
-- VUT FIT 2023
-- author: Daniel Pátek (xpatek08)

module SolveGenetic where

import Data.List (maximumBy)
import Data.Ord (comparing)
import System.Random
import Types (Knapsack (..), Item (..))

data Individual = Individual { genes :: [Bool], fitness :: Int }

type Population = [Individual]

-- create infinite list of generators based on one default generator
generatorList :: StdGen -> [StdGen]
generatorList gen = gen : generatorList (snd (next gen))

-- calculate fitness of one individual set of genes
-- uses sum of cost values
calculateFitness :: Knapsack -> [Bool] -> Int
calculateFitness ks i_genes
  | valid == False = 0 -- not valid sets of genes have fitness of zero
  | otherwise = selected_items_cost -- get only the costs of genes that are true
  where 
    costs = map cost (items ks)
    weights = map weight (items ks)
    selected_items_cost = sum $ map fst $ filter snd $ zip costs i_genes
    selected_items_weight = sum $ map fst $ filter snd $ zip weights i_genes
    valid = (selected_items_weight <= maxWeight ks) && (selected_items_cost >= minCost ks)

-- make one individual set of genes with fitness function result
-- uses let-in syntax to avoid generator running twice
makeIndividual :: Knapsack -> StdGen -> Individual
makeIndividual ks rng =
  let 
    i_genes = take (length $ items ks) (randoms rng) -- random list of Bool values (the size of all possible items)
    i_fitness = calculateFitness ks i_genes
  in Individual i_genes i_fitness

-- randomly initialize a population of individuals
-- generates new generator for each individual
initializePopulation :: Knapsack -> Int -> StdGen -> Population
initializePopulation ks size rng = map (makeIndividual ks) (take size $ generatorList rng)

getChance :: StdGen -> Float
getChance rng = fst (randomR (0.0, 1.0 :: Float) rng)

-- select one best individual from current population to propagate
reproductionWithChance :: Population -> StdGen -> [Individual]
reproductionWithChance pop rng
  | getChance rng > 0.0 = [] -- 30% chance
  | otherwise = [maximumBy (comparing fitness) pop]

-- crossover two individuals (parrents) to produce two individuals (children)
crossoverWithChance :: Knapsack -> Individual -> Individual -> StdGen -> (Individual, Individual)
crossoverWithChance ks i1 i2 rng
  | getChance rng > 0.3 = (i1, i2) -- 70% chance
  | otherwise =
    let 
      (r, _) = randomR (0, length (genes i1) - 1) rng -- get random point where to crossover
      (g1, g2) = splitAt r $ genes i1
      (g3, g4) = splitAt r $ genes i2
    in (Individual (g1 ++ g4) (calculateFitness ks $ g1 ++ g4), Individual (g3 ++ g2) (calculateFitness ks $ g3 ++ g2))

-- mutate an individual by flipping it's one random bit
mutateWithChance :: Knapsack -> StdGen -> Individual -> Individual
mutateWithChance ks rng i
  | getChance rng > 0.9 = i 
  | otherwise =
    let 
      (r, _) = randomR (0, length (genes i) - 1) rng -- get random point where to mutate
      (g1, g2:g3) = splitAt r $ genes i -- g2 is the bit we want
    in Individual (g1 ++ (not g2) : g3) (calculateFitness ks (g1 ++ (not g2) : g3))

-- tournament selection algorithm
-- inputs: old population and number generator
-- randomly selects 4 of individuals and does two battles to select 2 of them and doeas crossover
tournamentSelect :: Knapsack -> Population -> StdGen -> (Individual, Individual)
tournamentSelect ks p rng = 
  let
    [idx1, idx2, idx3, idx4] = take 4 $ randomRs (0, length p - 1) rng -- select 4 random candidates
    [p1, p2, p3, p4] = map (p !!) [idx1, idx2, idx3, idx4]
    winner1 = if fitness p1 >= fitness p2 then p1 else p2
    winner2 = if fitness p3 >= fitness p4 then p3 else p4
  in crossoverWithChance ks winner1 winner2 rng

-- create next generation of individuals
-- each tournament should return 2 individuals
-- needs to generate infinitely until it can take pop_size
nextGeneration :: Knapsack -> Population -> StdGen -> Population
nextGeneration ks pop rng = take (length pop) $ map (mutateWithChance ks rng) $ (reproductionWithChance pop rng) ++ cycle newIndividuals
  where
    newIndividuals = concatMap (\(x,y) -> [x,y]) $ map (tournamentSelect ks pop) (generatorList rng)

loopGenetic :: Knapsack -> Int -> Int -> Population -> StdGen -> [Int]
loopGenetic ks maxGen gen pop rng
  | gen == maxGen = if (fitness $ maximumBy (comparing fitness) pop) > 0 then (map fromEnum $ genes $ maximumBy (comparing fitness) pop) else replicate (length pop) 0
  | otherwise = 
    let
      n_rng = snd (next rng)
    in loopGenetic ks maxGen (gen + 1) (nextGeneration ks pop n_rng) n_rng

-- Run the genetic algorithm for a specified number of generations
-- n = number of generations
-- size = how many individual in one generation
knapsackGenetic :: Knapsack -> Int -> Int -> [Int]
knapsackGenetic ks n size
  | any (/= 0) solution = solution
  | otherwise = []
  where
    rng = mkStdGen 42 -- use a fixed seed for reproducible results
    solution = loopGenetic ks n 0 (initializePopulation ks size rng) rng