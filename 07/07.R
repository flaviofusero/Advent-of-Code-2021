library(magrittr)
library(stringr)
library(purrr)

path = 'input.txt'
dat <- scan("input.txt", what = integer(), sep = ",") %>%
  table

get_dist_to <- function(n, dat, dist) {
  positions <- as.numeric(names(dat))
  dist_to_n <- map_dbl(positions, ~ dist(., n))
  
  sum(dat[paste(positions)] * dist_to_n)
}

ans <- function(dat, dist) {
  positions <- as.numeric(names(dat))
  x <- min(positions):max(positions)
  min(map_dbl(x, ~ get_dist_to(., dat = dat, dist = dist)))
}

ans(dat, dist = function(x, y) abs(x - y))
ans(dat, dist = function(x, y) abs(x - y) * (abs(x - y) + 1) / 2)