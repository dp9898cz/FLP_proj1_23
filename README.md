# Knapsack Problem Solver

This program is written in Haskell and provides two different algorithms to solve the Knapsack problem: a brute-force algorithm and a genetic algorithm. 

## Installation

You need to have `ghc` installed. Then run `make` in root directory to compile the program.

## Usage

To use this program, run the following command in your terminal:

```
./flp22-fun -i|-b|-o [textFile]
```

Here are the available options:

- `-i`: Show the imported Knapsack instance
- `-b`: Solve the imported Knapsack instance using brute-force algorithm
- `-o`: Solve the imported Knapsack instance using genetic algorithm

The `textFile` argument is not mandatory. If not specified, the program will use standard input.

## Author

This program was created by Daniel Patek (xpatek08). 

## Example Usage

Here's an example of how to run the program:

```
./flp22-fun -i example.txt
./flp22-fun -b < example.txt
```

This will show the imported Knapsack instance from the `example.txt` file.

## Input format example

```
Knapsack {
    maxWeight: 45
    minCost: 100
    items: [
        Item {
            weight: 25
            cost: 250
        }
        Item {
            weight: 10
            cost: 300
        }
    ]
}
```

## Output format examples

```
Solution [1,1]
Solution [1,0]
False
```


