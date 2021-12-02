library(dplyr)
library(data.table)
library(stringr)

parse_data <- function(path) {
  dat <- readLines(path)
  dir = str_extract_all(dat, '[:alpha:]+')
  dist = str_extract_all(dat, '[:digit:]+') %>% as.numeric
  
  data.table(dir, dist)
}

dt <- parse_data('input.txt')

# part 1

ans1 <- function(dt) {
  h <- dt[dir == 'forward', sum(dist)]
  d <- dt[dir %in% c('up', 'down'), sum(
    ifelse(dir == 'up', -dist, ifelse(
      dir == 'down', dist, 
      NA)))]
  
  h * d
}

ans1(dt)

# part 2

ans2 <- function(dt) {
  df <- copy(dt)
  df[, aim := cumsum(
    ifelse(dir == 'up', -dist, ifelse(
      dir == 'down', dist,
      0
    ))
  )]
  
  df[, `:=` (h = cumsum(ifelse(dir == 'forward', dist, 0)),
             d = cumsum(ifelse(dir == 'forward', dist * aim, 0))
  )]
  
  df[.N, h * d]
}

ans2(dt)
