library(magrittr)
library(stringr)
library(readr)

path = 'input.txt'

parse_data <- function(path) {
  dat <- readLines(path)
  str_extract_all(dat, '[:digit:]') %>% 
    lapply(as.numeric) %>% 
    unlist
}

make_initial_state <- function(dat) {
  counters <- table(dat)
  for (i in 0:8) if (is.na(counters[paste0(i)])) counters[paste0(i)] <- 0
  counters[order(factor(names(counters)))]
}

evolve_state <- function(counters) {
  zeroes <- counters[1]
  counters <- lead(counters)
  counters[7] <- counters[7] + zeroes
  counters[9] <- zeroes
  
  counters
}

elapse_days <- function(counters, iters) {
  for (i in 1:iters) {
    counters <- evolve_state(counters)
  }
  
  counters
}

dat <- parse_data(path)
ans <- function(dat, days) {
  elapse_days(make_initial_state(dat), days) %>% sum
}

ans(dat, 80)
options(scipen = 999) 
ans(dat, 256)