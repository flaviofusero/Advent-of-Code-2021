library(purrr)
library(readr)
library(data.table)

path <- 'input.txt'

parse_data <- function(path) {
  dat <- map(
    readLines(path),
    function(line) { 
      line <- eval(parse(
        text = gsub('[', 'list(l=', gsub(']', ')', gsub(',', ',r=', line), fixed = TRUE), fixed = TRUE)
      ))
      line <- line %>% unlist %>% setNames(gsub('.', '', names(unlist(line)), fixed = TRUE))
      return(line)
    })
  
  return(dat)
}

# Might want to try out data.tree:
# dt <- FromListSimple(dat)
# L5 <- Traverse(dt, filterFun = function(x) x$level == 5)
# etc

sum_nums <- function(n, m) {
  names(n) <- paste0('l', names(n))
  names(m) <- paste0('r', names(m))
  
  return(c(n, m))
}

explode <- function(num) {
  i <- (which(map_dbl(names(num), nchar) >= 5))[1:2]
  if (i[1] > 1) num[i[1] - 1] <- num[i[1] - 1] + num[i[1]]
  if (i[2] < length(num)) num[i[2] + 1] <- num[i[2] + 1] + num[i[2]]
  num[i[1]] <- 0
  num <- num[-i[2]]
  str_sub(names(num)[i[1]], -1, -1) <- ''
  
  return(num)
}

split <- function(num) {
  i <- (which(num >= 10))[1]
  n <- num[i]
  num <- append(num[-i], ceiling(n / 2), after = i - 1) %>% 
    append(floor(n / 2), after = i - 1)
  names(num)[c(i, i+1)] <- paste0(names(num)[c(i, i+1)], c('l', 'r'))
  
  return(num)
}

reduce_num <- function(num) {
  explodable <- any(map_dbl(names(num), nchar) >= 5)
  splittable <- any(num >= 10)
  while (explodable | splittable) {
    if (explodable) {
      num <- explode(num)
    } else if (splittable) {
      num <- split(num)
    } 
    
    explodable <- any(map_dbl(names(num), nchar) >= 5)
    splittable <- any(num >= 10)
  }
  
  return(num)
}

get_magnitude <- function(num) {
  max_depth <- max(map_dbl(names(num), nchar))
  for (i in max_depth:1) {
    nleft <- names(num)[which(names(num) %like% 'l$' & map_dbl(names(num), nchar) == i)]
    nright <- paste0(substr(nleft, 1, nchar(nleft) - 1), 'r')
    num[which(names(num) %in% nleft)] <- 3 * num[nleft] + 2 * num[nright]
    str_sub(names(num)[which(names(num) %in% nleft)], -1, -1) <- ''
    num <- num[-which(names(num) %in% nright)]
  }
  
  return(magnitude = num)
}

# part 1
Reduce(function(x, y) reduce_num(sum_nums(x, y)), parse_data(path)) %>% 
  get_magnitude

# part 2
pairs <- (expand.grid(parse_data(path), parse_data(path)))[
  -c(1, 1 + cumsum(rep(1 + length(parse_data(path)), 100))), ]

map2(pairs[, 1], pairs[, 2], ~ reduce_num(sum_nums(unlist(.x), unlist(.y)))) %>% 
  map_dbl(get_magnitude) %>% 
  max
