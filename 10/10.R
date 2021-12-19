library(tidyverse)

simplify <- function(line) {
  for (pair in c('()', '[]', '{}', '<>')) line <- gsub(pair, '', line, fixed = TRUE)
  line
}
simplify_all <- function(line) {
  for (i in 1:(nchar(line)/2)) line <- simplify(line) 
  line
}

# part 1
lines <- readLines('input.txt') %>% map(simplify_all)
first_illeg <- lines %>% str_extract('[\\)\\]\\}\\>]')

(c(3, 57, 1197, 25137) * (first_illeg %>% table)) %>% sum  

# part 2
incomplete <- lines[which(is.na(first_illeg))]

get_score <- function(line) {
  score <- 0
  points <- c('(' = 1, '[' = 2, '{' = 3, '<' = 4)
  for (char in rev(unlist(str_split(line, '')))) score <- 5 * score + points[char]
  score
}

incomplete %>% 
  map_dbl(get_score) %>% 
  sort %>% 
  median
