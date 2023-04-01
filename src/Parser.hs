-- flp-22-fun (Parser.hs)
-- VUT FIT 2023
-- author: Daniel Pátek (xpatek08)

module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import Types (Knapsack (..), Item (..))

-- NOTE
-- Intentionally adding the "_ <- " part to supress [-Wunused-do-bind] warning

-- Parser for an item
itemParser :: Parser Item
itemParser = do
    spaces -- leading spaces (just to be sure)
    _ <- string "Item {"
    spaces
    _ <- string "weight: "
    item_weight <- many1 digit
    spaces
    _ <- string "cost: "
    item_cost <- many1 digit
    spaces
    _ <- char '}'
    spaces -- trailing spaces (just to be sure)
    return $ Item (read item_weight) (read item_cost)

-- Parser for a knapsack
knapsackParser :: Parser Knapsack
knapsackParser = do
    spaces -- leading spaces consume
    _ <- string "Knapsack"
    spaces
    _ <- char '{'
    spaces
    _ <- string "maxWeight: "
    ks_maxWeight <- many1 digit
    spaces
    _ <- string "minCost: "
    ks_minCost <- many1 digit
    spaces
    _ <- string "items: ["
    ks_items <- many1 (do {
        spaces; 
        item <- itemParser; 
        spaces; 
        return item
        })
    _ <- char ']'
    spaces
    _ <- char '}'
    spaces -- trailing spaces consume
    return $ Knapsack (read ks_maxWeight) (read ks_minCost) ks_items

-- Export the function to parse a knapsack
parseKnapsack :: String -> Either ParseError Knapsack
parseKnapsack = parse knapsackParser ""