-- flp-22-fun (main.hs)
-- VUT FIT 2023
-- author: Daniel Pátek (xpatek08)

module Main where

import System.Environment
import System.IO

import Parser (parseKnapsack)
import Solve (knapsackBruteForce)
import SolveGenetic (knapsackGenetic)
import Types (printSolution, Knapsack (..))

main :: IO ()
main = getArgs >>= handleArgs >>= putStr

handleArgs :: [String] -> IO String
handleArgs [argument] = getContents >>= return . parseArg argument -- get contents from stdin
handleArgs [argument, file] = openFile file ReadMode >>= hGetContents >>= return . parseArg argument
handleArgs _ = error "Wrong number of arguments. Run program with -h to show help.\n"

-- take a string as input and returns a maybe knapsack
getKnapsack :: String -> Maybe Knapsack
getKnapsack str = case parseKnapsack str of
    Left err -> error ("Error when parsing Knapsack instance: \n" ++ (show err) ++ "\n") >> Nothing
    Right knapsack -> Just knapsack

-- parse argument
parseArg :: String -> String -> String
parseArg arg str
  | arg == "-i" = case getKnapsack str of
    Nothing -> ""
    Just ks -> show ks
  | arg == "-b" = case getKnapsack str of
    Nothing -> ""
    Just ks -> printSolution $ knapsackBruteForce ks
  | arg == "-o" = case getKnapsack str of
    Nothing -> ""
    Just ks -> printSolution $ knapsackGenetic ks 50 100 -- 50 generations of 100 individuals
  | arg == "-h" =
    "Knapsack problem using bruteforce or genetic algorithm.\n" ++
    "Usage: ./flp22-fun [-i|-b|-o] [textFile]\n" ++
    "-i Show imported knapsack instance.\n" ++
    "-b Brutefoce imported knapsack instance.\n" ++
    "-o Solve imported knapsack instance using genetic algorithm.\n" ++
    "TextFile argument is not mandatory. If not specified - stdin is used.\n"
  | otherwise = "Unknown argument: \"" ++ arg ++ "\". Run program with -h to show help.\n"