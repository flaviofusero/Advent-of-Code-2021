library(tidyverse)

path = 'input.txt'

dat <- str_extract_all(readLines(path), '-?[0-9]+') %>% 
  unlist %>% 
  as.numeric %>% 
  setNames(c('xmin', 'xmax', 'ymin', 'ymax'))

# part 1
y_bound <- (abs(dat['ymin']) - 1) * abs(dat['ymin']) / 2
step_bound <- abs(3 * dat['ymin'])

# part 2
make_xy <- function(v0_x, v0_y, step_bound) {
  x <- cumsum(v0_x - sign(v0_x) * c(0:v0_x, rep(v0_x, step_bound)))
  y <- cumsum(v0_y - 0:step_bound)
  
  return(list('x' = x[1:step_bound], 'y' = y[1:step_bound]))
}

ans2 <- function(dat) {
  r <- list()
  for (v0_x in 1:dat['xmax']) {
    for (v0_y in dat['ymin']:abs(dat['ymin'])) {
      xy <- make_xy(v0_x, v0_y, step_bound)
      
      if (any(map2_lgl(xy[['x']], xy[['y']],
                       ~ between(.x, dat['xmin'], dat['xmax']) &
                       between(.y, dat['ymin'], dat['ymax'])
      ))) {
        r[[length(r) + 1]] <- list(v0_x, v0_y)
      }
    }
  }
  
  length(r)
}

ans2(dat)
