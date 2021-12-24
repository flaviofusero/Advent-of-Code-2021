# Advent-of-Code-2021
My solutions for Advent of Code 2021. I summarize below the key takeaways for each problem, and / or point out interesting tools I used

### Day 1
* Prefer vectorization to `for` loops when possible

### Day 2
* Regex are useful to parse the input
* Sometimes `cumsum` can vectorize problems which apparently require `for` loops

### Day 4
* Use `readr::read_file(input) %>% stringr::str_split('\r\n\r\n')` to parse input where individual data points are multi-line tables

### Day 5
* Functional approach: write functions that solve the problem for individual list elements, then generalize to the whole list with `lapply` (or `purrr` functions)
* Matrices can be subsetted with another matrix

### Day 6
* Use hash tables to solve problems which require counting
* Use `table` to make a hash table (frequency table) from a list

### Day 7
* Use `scan` to parse simple inputs where the separator is a single byte

### Day 9
* Use BFS / DFS to search mazes
* Use `apply` for columnwise or rowwise operation on matrices

### Day 11
* Use complex number to represent 2d grids, so that most operations behave as expected (sums, rotations, distances, ...)

### Day 12
* Recursive solutions can be slow, but their performance can be improved by caching the results. See Day 21 for an example application.

### Day 15
* Use Dijkstra's algorithm or A* for pathfinding (or cheat it out with the `igraph` library)

### Day 16
* Use `Rmpfr` for integers with arbitrary precision

### Day 18
* Use the `eval(parse(text))` trick to evaluate text expressions (don't do it on untrusted input!)
* Might also want to explore the `data.tree` library

### Day 21
* Cache the results of recursive functions to improve speed. This is useful when the space of possible inputs is not too big.

### Day 23
* Implement your own A* or Dijkstra for pathfinding, even if you don't have the complete graph at your disposal, but only the neighbor-finding function

### Day 24
* Sometimes the particular structure of your input can make the problem easier when solving the general case is not an option