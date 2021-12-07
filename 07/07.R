library(magrittr)
library(stringr)
library(purrr)

parse_data <- function(path) {
  input <- readLines(path) %>% 
    str_extract_all('[:digit:]+', simplify = TRUE) %>% 
    as.numeric 
  
  table(input)
}

path = 'input.txt'
dat <- parse_data(path)

get_dist_to <- function(n, dat, dist) {
  positions <- as.numeric(names(dat))
  dist_to_n <- map_dbl(positions, ~ dist(.x, n))
  
  sum(dat[paste(positions)] * dist_to_n)
}

ans <- function(dat, dist) {
  positions <- as.numeric(names(dat))
  x <- min(positions):max(positions)
  min(map_dbl(x, ~ get_dist_to(.x, dat = dat, dist = dist)))
}

ans(dat, dist = function(x, y) abs(x - y))
ans(dat, dist = function(x, y) abs(x - y) * (abs(x - y) + 1) / 2)