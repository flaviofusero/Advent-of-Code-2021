library(tidyverse)
library(data.table)

path <- 'input.txt'

parse_data <- function(path) {
  readLines(path) %>% 
    str_split('', simplify = TRUE) %>% 
    apply(c(1, 2), FUN = as.numeric)
}

get_neighbors_rc <- function(dat, r, c) {
  rc_neigh <- matrix(c(r - 1, c, r + 1, c, r, c - 1, r, c + 1), ncol = 2, byrow = TRUE) %>% 
    as.data.table
  valid_rc <- rc_neigh[which(pmap(rc_neigh, min) > 0 & 
                               rc_neigh[, 1] <= nrow(dat) &
                               rc_neigh[, 2] <= ncol(dat)
  )]
  
  as.matrix(valid_rc)
}

get_low_points_rc <- function(dat) {
  low_points_rc <- c()
  
  for (r in 1:nrow(dat)) {
    for (c in 1:ncol(dat)) {
      if (all(dat[r, c] < dat[get_neighbors_rc(dat, r, c)])) {
        low_points_rc <- rbind(low_points_rc, c(r, c))
      }
    }
  }
  
  low_points_rc
}

dat <- parse_data(path)

# part 1
low_points_rc <- dat %>% 
  get_low_points_rc

sum(dat[low_point_rc] + 1)

# part 2
get_basin_size <- function(dat, low_point_rc) {
  basin <- list(low_point_rc)
  to_check <- basin
  checked <- list()
  
  while (length(to_check)) { 
    rc <- to_check[1]
    to_check <- to_check[-1]
    
    if (!rc %in% checked) {
      checked <- append(checked, rc)
      neighbors_rc <- get_neighbors_rc(dat, rc[[1]][1], rc[[1]][2])
      higher_points_rc <- neighbors_rc[which(dat[matrix(rc[[1]], ncol = 2)] < dat[neighbors_rc] &
                                               dat[neighbors_rc] < 9), ] %>% 
        matrix(ncol = 2)
      higher_points_rc <- map2(higher_points_rc[, 1], higher_points_rc[, 2], function(x, y) c(x,y))
      basin <- unique(append(basin, higher_points_rc))
      to_check <- unique(append(to_check, higher_points_rc))
    }
  }
  
  return(length(basin))
}

# part 2
basin_sizes <- apply(low_points_rc, 1, function(x) get_basin_size(dat, x)) %>% 
  sort(decreasing = TRUE)

prod(basin_sizes[1:3])
