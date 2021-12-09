library(magrittr)
library(stringr)
library(readr)

parse_data <- function(path) {  
  input <- read_file(path) %>% 
    str_split('\r\n\r\n', simplify = TRUE) %>% 
    str_extract_all('[:digit:]+') %>% 
    lapply(as.numeric)
  
  nums <- input[[1]]
  
  ncol <- input[[2]] %>% 
    length %>% 
    sqrt
  
  tables <- lapply(input[-1], matrix, ncol = ncol)
  marked <- rep(
    list(matrix(FALSE, ncol = ncol)), 
    length(input[-1]))
  
  return(list(nums = nums, tables = tables, marked = marked))
}

path = 'input.txt'
dat <- parse_data(path)

extract_number <- function(iter, dat) {
  num <- dat$nums[iter]
  num_index <- lapply(dat$tables, function(x) which(x == num, arr.ind = T))
  
  for (i in 1:length(dat$tables)) {
    if (length(num_index[[i]])) dat$marked[[i]][num_index[[i]][1], num_index[[i]][2]] <- TRUE
  }
  
  return(dat)         
}

check_victory <- function(dat) {
  ncols <- ncol(dat$tables[[1]])
  table_wins <- lapply(dat$marked, function(x) any(c(colSums(x), rowSums(x)) == ncols)) %>% 
    unlist
  
  if (any(table_wins)) {
    return(which(table_wins == TRUE)[1])
  }
  
  return(0)
}

get_winning_score <- function(dat, iter, winning_table_index) {
  winning_table <- dat$tables[[winning_table_index]]
  unmarked <- which(dat$marked[[winning_table_index]] == FALSE, arr.ind = TRUE)
  return(sum(winning_table[unmarked]) * dat$nums[iter])
}

# part 1

ans1 <- function(dat) {
  iter = 0
  
  while (!check_victory(dat)) {
    iter <- iter + 1
    dat <- extract_number(iter = iter, dat = dat)
  }
  
  get_winning_score(dat = dat, iter = iter, winning_table_index = check_victory(dat))
}

ans1(dat)

# part 2

ans2 <- function(dat) {
  iter = 0
  
  while(!(length(dat$tables) == 1 & check_victory(dat))) {
    winning_index <- check_victory(dat)
    
    while (!winning_index) {
      iter <- iter + 1
      dat <- extract_number(iter = iter, dat = dat)
      winning_index <- check_victory(dat)
    }
    
    if (length(dat$tables) > 1) {
      dat$tables <- dat$tables[-winning_index]
      dat$marked <- dat$marked[-winning_index]
    }
  }
  
  get_winning_score(dat = dat, iter = iter, winning_table_index = 1)
}

ans2(dat)
