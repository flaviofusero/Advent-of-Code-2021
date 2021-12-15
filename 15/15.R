library(tidyverse)
library(igraph)

path <- 'input.txt'

parse_data <- function(path) {
  dat <- readLines(path) %>% 
    str_split('', simplify = TRUE) %>% 
    apply(c(1, 2), FUN = as.numeric)
  
  dat
}

make_big_grid <- function(dat, n) {
  next_piece <- v_expansion <- dat
  
  for (i in 1:(n-1)) {
    next_piece <- (next_piece %% 9) + 1
    v_expansion <- rbind(v_expansion, next_piece)
  }
  
  next_piece <- full_expansion <- v_expansion
  for (i in 1:(n-1)) {
    next_piece <- (next_piece %% 9) + 1
    full_expansion <- cbind(full_expansion, next_piece)
  }
  
  full_expansion
}

ans <- function(grid) {
  g <- make_lattice(c(nrow(grid), ncol(grid))) %>% 
    as.directed(mode = 'mutual')
  E(g)$weight <- grid[get.edgelist(g)[, 2]]
  
  distances(g, 1, nrow(grid) * ncol(grid), 'out')
}

# part 1
ans(parse_data(path))

# part 2
ans(make_big_grid(parse_data(path), n = 5))
