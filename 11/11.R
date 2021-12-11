library(tidyverse)

parse_data <- function(path) {
  dat <- readLines(path) %>% 
    str_split('', simplify = TRUE) %>% 
    apply(c(1, 2), FUN = as.numeric)
  
  return(dat = dat)
}

get_neighbors <- function(dat) {
  ij <- complex(real = rep(1:ncol(dat), ncol(dat)), imaginary = rep(1:ncol(dat), each = ncol(dat)))
  
  map(ij, ~ which(between(abs(ij - .), 1, sqrt(2))))
}

play_loop <- function(dat) {
  dat <- (dat + 1) %% 10
  n <- get_neighbors(dat)
  flashed <- c()
  new_flashes <- which(dat == 0)
  
  while (length(new_flashes)) {
    flashed <- append(flashed, new_flashes)
    adj2zero <- unlist(n[new_flashes])
    for (x in adj2zero) dat[x] <- (dat[x] + sign(dat[x])) %% 10
    new_flashes <- setdiff(which(dat == 0), flashed)
  }
  
  dat
}

# part 1

play_game <- function(dat, n) {
  flashes <- 0
  for (i in 1:n) {
    dat <- play_loop(dat) 
    flashes <- flashes + sum(dat == 0)
  }
  
  flashes
}

play_game(parse_data(path), 100)

# part 2

get_first_flash <- function(dat) {
  iter <- 0
  while (sum(dat == 0) < length(dat)) {
    iter <- iter + 1
    dat <- play_loop(dat)
  }
  
  iter
}

get_first_flash(parse_data(path))
