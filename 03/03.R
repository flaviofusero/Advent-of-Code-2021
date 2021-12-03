library(dplyr)
library(stringr)

parse_data <- function(path) {
  dat <- readLines(path)
  str_extract_all(dat, '[:digit:]') %>% 
    lapply(as.numeric)
}

dat <- parse_data('input.txt')

# part 1

sum_nth_bits <- function(n, dat) {
  lapply(dat, `[[`, n) %>% 
    unlist %>% 
    sum
}

make_gamma <- function(dat) {
  freqs <- lapply(1:length(dat[[1]]), sum_nth_bits, dat) %>% unlist
  as.numeric(freqs >= length(dat) / 2)
}

make_epsilon <- function(dat) 1 - make_gamma(dat)

ans1 <- function(dat) {
  gamma_chr <- make_gamma(dat) %>% as.character %>% paste0(collapse = '')
  epsilon_chr <- make_epsilon(dat) %>% as.character %>% paste0(collapse = '')
  
  strtoi(gamma_chr, base = 2) * strtoi(epsilon_chr, base = 2)
}

ans1(dat)

# part 2

get_oxy_co2_level <- function(dat, n = bit, type) {
  stopifnot(type %in% c('oxy', 'co2'))
  if (length(dat) == 1) {
    return(dat)
  }
  
  nth_bits <- lapply(dat, `[[`, n) %>% unlist
  if (type == 'oxy') target_bit <- make_gamma(dat)[n]
  if (type == 'co2') target_bit <- make_epsilon(dat)[n]
  
  return(get_oxy_co2_level(dat = dat[which(nth_bits == target_bit)], 
                           n = n + 1,
                           type = type))
}  

ans2 <- function(dat) {
  for (type in c('oxy', 'co2'))
    assign(type, get_oxy_co2_level(dat, n = 1, type) %>% 
             unlist %>% 
             as.character %>% 
             paste0(collapse = '') %>% 
             strtoi(base = 2)
    )
  
  oxy * co2
}

ans2(dat)
