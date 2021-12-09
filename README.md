# Advent-of-Code-2021
My solutions for Advent of Code 2021. I summarize below the key takeaways for each problem, and / or to point out interesting tools I used to solve them

## Day 1
* Prefer vectorization to `for` loops when possible

## Day 2
* Regex are useful to parse the input
* Sometimes `cumsum` can vectorize problems which apparently require `for` loops

## Day 3
* Clear naming of variables and functions can do wonders for code readability 

## Day 4
* Use `readr::read_file(input) %>% stringr::str_split('\r\n\r\n')` to parse input where individual data points are multi-line tables

## Day 5
* Functional approach: write functions that solve the problem for individual list elements, then generalize to the whole list with `lapply`
* Matrices can be subsetted with another matrix

## Day 6
* Use hash tables to solve problems which require counting
* Use `table` to make a hash table (frequency table) from a list

## Day 7
* Use `scan` to parse simple inputs where the separator is a single byte

## Day 9
* Use BFS / DFS to search mazes