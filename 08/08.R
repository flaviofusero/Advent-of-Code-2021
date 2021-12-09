library(magrittr)
library(stringr)
library(purrr)

path = 'input.txt'

parse_data <- function(path) {
  dat <- readLines(path) %>% 
    str_split(fixed(' | '))
  
  list(signals = str_split(map(dat, 1), ' '),
       displays = str_split(map(dat, 2), ' ')
  )
}

# utils for operations character by character in strings
chardiff <- function(s1, s2) setdiff(unlist(str_split(s1, '')), unlist(str_split(s2, ''))) %>% unlist
charint <- function(s1, s2) do.call(intersect, strsplit(c(paste0(s1, collapse = ''), paste0(s2, collapse = '')), split = ""))

get_candidate_wires <- function(line) {
  n_wires <- c('0' = 6, '1' = 2, '2' = 5, '3' = 5, '4' = 4,
               '5' = 5, '6' = 6, '7' = 3, '8' = 7, '9' = 6)
  
  map(0:9,
      ~ line[which(nchar(line) == n_wires[paste(.)] )]
  ) %>% 
    setNames(0:9)
}


get_wire_map <- function(candidates) {
  wires <- 'abcdefg'
  
  a <- chardiff(candidates[['7']], candidates[['1']])
  c <- chardiff(candidates[['7']], Reduce(charint, candidates[['6']])) %>% 
    chardiff(a)
  f <- chardiff(candidates[['1']], c)
  b <- chardiff(chardiff(wires, candidates[['2']][which(!grepl(f, candidates[['2']]))] ),
                f)
  d <- chardiff(candidates[['4']], paste0(b, c, f))
  g <- chardiff(
    Reduce(charint, candidates[['6']]),
    c(d, a, b, c, f)
  )
  e <- chardiff(wires, c(a, b, c, d, f, g))
  
  c('a' = a, 'b' = b, 'c' = c, 'd' = d, 'e' = e, 'f' = f, 'g' = g)
}

get_output <- function(wire_map, display) {
  n_to_signal <- c('0' = 'abcefg', '1' = 'cf', '2' = 'acdeg', '3' = 'dafgc', '4' = 'bcdf', 
                   '5' = 'abdfg', '6' = 'abdefg', '7' = 'acf', '8' = 'abcdefg', '9' = 'abcdfg') 
  
  for (i in 1:length(n_to_signal)) {
    for (wire in c('a', 'b', 'c', 'd', 'e', 'f', 'g')) {
      v <- str_split(n_to_signal[i], '') %>% 
        unlist %>% 
        map_chr(~ wire_map[[.x]]) %>% paste0(collapse = '')
    }
    n_to_signal[i] <- v
  }
  
  translated_display <- map_chr(
    display,
    function(d) {
      names(n_to_signal)[which(map_lgl(str_split(n_to_signal, ''), ~ setequal(., unlist(str_split(d, '')))))]
    }
  )
  
  paste0(translated_display, collapse = '') %>% 
    as.numeric
}

ans <- function(dat) {
  
  translated_displays <- dat %>% 
    pmap(~ c(.x, .y)) %>% 
    map(get_candidate_wires) %>% 
    map(get_wire_map) %>% 
    map2(dat[['displays']], get_output)
  
  sum(unlist(translated_displays))
}

ans(parse_data(path))