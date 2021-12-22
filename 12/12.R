library(magrittr)
library(purrr)
library(data.table)
library(stringr)

path = 'input.txt'

parse_data <- function(path) {
  dat <- fread(path, sep = '-', header = FALSE, col.names = c('N1', 'N2'))
}

neighbors <- function(dat, node) {
  c(dat[N1 == node, N2], dat[N2 == node, N1])
}

moves <- function(dat, node, small_visited, part2 = FALSE, small_twice = FALSE) {
  if (part2 == TRUE & small_twice == FALSE) {
    m <- neighbors(dat, node)
  } else {
    m <- setdiff(neighbors(dat, node), small_visited)
  }
  
  setdiff(m, 'start')
}

count_paths <- function(dat, node, small_visited, part2 = FALSE, small_twice = FALSE) {
  if (node == 'end') {
    return(1)
  }
  if (part2 == TRUE & node %in% small_visited) {
    small_twice <- TRUE 
  }
  if (node == tolower(node)) small_visited <- union(small_visited, node)
  if (length(moves(dat, node, small_visited, part2, small_twice)) == 0) {
    return(0)
  }
  
  sum(map_dbl(
    moves(dat, node, small_visited, part2, small_twice),
    ~ count_paths(dat, ., small_visited, part2, small_twice)
  ))
} 

dat <- parse_data(path)
count_paths(dat, 'start', '')
count_paths(dat, 'start', '', part2 = TRUE, small_twice = FALSE)
