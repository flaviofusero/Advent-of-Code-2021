library(purrr)
library(stringr)

path <- 'input.txt'
enh <- unlist(str_split(readLines(path)[1], ''))
inp <- str_split(readLines(path)[3:length(readLines(path))], '', simplify = TRUE)

max_col <- ncol(inp) + 105
grid <- matrix('.', max_col, max_col)
start <- ceiling((max_col - ncol(inp)) / 2)

grid[start:(start + ncol(inp) - 1), start:(start + ncol(inp) - 1)] <- inp

enh[enh == '.'] <- '0'
enh[enh == '#'] <- '1'
grid[grid == '.'] <- '0'
grid[grid == '#'] <- '1'

play_loop <- function(grid, enh, parity) {
  new_grid <- grid
  for (i in 2:(max_col - 1)) {
    for (j in 2:(max_col - 1)) {
      if (i == 2 | i == (max_col - 1) | j == 2 | j == (max_col - 1)) {
        if (parity == 'odd') {
          index <- 0
        } else if (parity == 'even') {
          index <- 511 
        }
      } else {
        index <- grid[(i - 1):(i + 1), (j - 1): (j + 1)] %>% 
          t %>% 
          paste0(collapse = '') %>% 
          strtoi(base = 2)
      }
      new_grid[i, j] <- enh[index + 1]
    }
  }
  return(new_grid)
}

play_game <- function(grid, enh, n) {
  for (iter in 1:n) {
    if (iter %% 2 == 1) {
      grid <- play_loop(grid, enh, 'odd')
    } else {
      grid <- play_loop(grid, enh, 'even')
    }
  }
  return(grid)
}

g <- play_game(grid, enh, 50)
sum(g == '1')
