library(purrr)
library(stringr)

path <- 'input.txt'
parse_data <- function(path) {
  readLines(path) %>% 
    str_split('', simplify = TRUE)
}

get_destination <- function(grid, i, j) {
  if (grid[i, j] == '>') {
    return(if (j < ncol(grid)) matrix(c(i, j + 1), 1) else matrix(c(i, 1), 1))
  } else if (grid[i, j] == 'v') {
    return(if (i < nrow(grid)) matrix(c(i + 1, j), 1) else matrix(c(1, j), 1))
  }
}

is_free <- function(cell) cell == '.'

play_loop <- function(grid) {
  new_grid <- grid
  for (type in c('>', 'v')) {
    for (i in 1:nrow(grid)) {
      for (j in 1:ncol(grid)) {
        dest <- get_destination(grid, i, j)
        if (grid[i, j] == type & isTRUE(is_free(grid[dest]))) {
          new_grid[i, j] <- '.'
          new_grid[dest] <- type 
        }
      }
    }
    grid <- new_grid
  }
  return(new_grid)
}

play_game <- function(grid) {
  i <- 0
  while (TRUE) {
    i <- i + 1
    new_grid <- play_loop(grid)
    if (isTRUE(all.equal(grid, new_grid))) {
      return(i)
    }
    grid <- new_grid
  }
}

play_game(parse_data(path))
